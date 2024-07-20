use super::{DirectiveError, ParseError, ParseResult, ParserConfig, SyntaxError, ValueError};
use crate::ast::*;
use crate::lexer::{token, Keyword, Token};
use crate::print::{PrettyPrint, PrettyString};
use crate::runtime::{self as rt, Context};
use crate::source::{SourceId, SourcePos, Spanned};

use either::{Either, Left, Right};
use rug::ops::CompleteRound;
use rug::{Complete, Float, Integer};
use ustr::Ustr;

const TABWIDTH: &str = "    ";
const FLOAT_LIT_PRECISION: u32 = 53;

const PAREN_DELIM: (Token, Token) = (Token::LDelim("("), Token::RDelim(")"));

/*
    import ::= 'import' _ <path>

    op_decl_assign ::= <op_decl_start> _ '(' <operator> ')' _ <op_decl_param_list> _ '=' <expr>
    op_decl_define ::= <op_decl_start> _ '(' <operator> ')' _ <op_fn_param_list> _ <block_expr>

    op_decl_start ::= ('prefix' | 'postfix' | 'infix') _ 'operator'

    prefix_op_decl ::= 'prefix' _ 'operator' _ '(' <operator> ')' _ <op_param_list> _ '=' <expr>
    postfix_op_decl ::= 'postfix' _ 'operator' _ '(' <operator> ')' _ <op_param_list> _ '=' <expr>
    infix_op_decl ::= 'infix' _ 'operator' _ '(' <operator> ')' _ <op_param_list> _ '=' <expr>

    dimension_decl ::= 'dimension' _ <ident> _ ['=' <dim_expr>]
    base_unit_decl ::= 'base' _ 'unit' _ <ident> _ [<suffix_list>] _ '=' _ <dim_expr>
    sub_unit_decl ::= 'unit' _ <ident> _ [<suffix_list>] '[' <dim_expr> ']' _ '=' <num_expr>

    fn_decl ::= 'fn' _ <ident> _ <param_list> _ [<dim_ret>|<type>] _ <block_expr>

    ------------------------

    dim_expr ::= <dim_expr_term> _ <dim_operator> _ <dim_expr>
            | <dim_expr_term> _ <dim_expr>
            | <dim_expr_term>

    dim_expr_term ::= ('-' | '+') _ <dim_expr_term>
                    | <dim_expr_atom>

    dim_expr_atom ::= '(' _ <dim_expr> _ ')'
                    | <ident>
                    | <number>

    ------------------------

    expr ::= <expr_term> _ <infix_op> _ <expr>
          | <expr_term> _ <postfix_op>
          | <expr_term> _ <unit>
          | <expr_term>

    expr_term ::= <prefix_op> _ <expr>
                | '(' _ <expr> _ ')'
                | <expr_atom>

    expr_atom ::= 'if' <expr> <block_expr> 'else' <block_expr>
                | <path>
                | <number>

    ------------------------

    block_expr ::= '{' [ '\n' ] _ <expr> ('\n' _ <expr> _)* _ '}'

    dim_ret ::= '[' _ <dim_expr> _ ']'

    param_list ::= '(' ((_ <param> _) ** ',') ')'

    op_decl_param_list ::= '(' ((_ <op_decl_param> _) ** ',') ')'

    op_fn_param_list ::= '(' ((_ <op_fn_param> _) ** ',') ')'

    param ::= <ident> '[' <dim_expr> ']'
            | <ident> ':' <type>
            | <ident>

    op_decl_param ::= '[' <dim_expr> ']'
                    | <type>

    op_fn_param ::= <ident> _ '[' _ <dim_expr> _ ']'
                  | <ident> _ ':' _ <type>

    dim_operator ::= '+' | '*' | '^'

    suffix_list ::= '{' ((_ <ident> _) ** ',') '}'

    type ::= 'any' | 'int' | 'float' | 'num'

    operator ::= <op_char>+
    unit ::= <ident>
    path ::= <ident> [ '::' (<ident> ++ '::') ]
    ident ::= <id_char_start> <id_char_continue>*
    number ::= <integer> | <float>
*/

pub struct Parser<'a> {
    ctx: &'a mut rt::Module,
    config: ParserConfig,

    tokens: &'a [(Token, SourceSpan)],
    idx: usize,
    pos: usize,
    source_id: SourceId,

    trace_on: bool,
    trace_level: usize,
}

impl<'a> Parser<'a> {
    pub fn new(ctx: &'a mut rt::Module, tokens: &'a [(Token, SourceSpan)]) -> Self {
        Self {
            ctx,
            config: ParserConfig::default(),

            tokens,
            idx: 0,
            pos: 0,
            source_id: SourceId::INVALID,

            trace_on: std::env::var("TRACE_PARSER").is_ok(),
            trace_level: 0,
        }
    }

    // module ::= (_ <item> _) ** '\n'
    pub fn parse(&mut self) -> ParseResult<Module> {
        let mut items = vec![];
        while !self.peek_token().is_eof() {
            let item = self.parse_item()?;
            // self.trace_debug(&format!("at position {:?}", self.position().as_span()));
            if !self.peek_token().is_eof() {
                self.expect(Token::NewLine, "expected newline")?;
            }

            if let Some(item) = item {
                items.push(item);
            }
        }

        Ok(Module::new(self.source_id, self.ctx.id, items))
    }

    // item ::= [ <import> | <directive> | <base_unit_decl> | <sub_unit_decl> | <dim_decl> | <fn_decl> | <expr> ]
    fn parse_item(&mut self) -> ParseResult<Option<Item>> {
        self.trace("parse_item", |parser| {
            parser.consume_any(Token::Space);

            let next_token = parser.peek_token();
            let item = if next_token.is_directive_start() {
                let directive = parser.parse_directive()?;
                Some(Item::directive(directive))
            } else if next_token == &Token::Keyword(Keyword::Import) {
                let path = parser.parse_import()?;
                Some(Item::import(path))
            } else if next_token == &Token::Keyword(Keyword::Base) {
                let decl = parser.parse_base_unit_decl()?;
                Some(Item::unit_decl(decl))
            } else if next_token == &Token::Keyword(Keyword::Unit) {
                let decl = parser.parse_sub_unit_decl()?;
                Some(Item::unit_decl(decl))
            } else if next_token == &Token::Keyword(Keyword::Dimension) {
                let decl = parser.parse_dim_decl()?;
                Some(Item::dim_decl(decl))
            } else if next_token == &Token::Keyword(Keyword::Fn) {
                let decl = parser.parse_fn_decl()?;
                Some(Item::fn_decl(decl))
            } else if matches!(next_token, &Token::Keyword(kw) if kw.is_op_decl()) {
                let decl = parser.parse_op_decl()?;
                let op = rt::Operator::new(
                    decl.name.as_spanned_ustr(),
                    decl.kind,
                    decl.assoc,
                    decl.prec,
                    decl.body.clone(),
                );

                parser.ctx.register_operator(op)?;
                Some(Item::op_decl(decl))
            } else if !next_token.is_eol() {
                let expr = parser.parse_expr(0)?;
                Some(Item::expr(expr))
            } else {
                None
            };

            Ok(item)
        })
    }

    fn parse_import(&mut self) -> ParseResult<Path> {
        self.trace("parse_import", |parser| {
            parser.expect(Token::Keyword(Keyword::Import), "expected 'import'")?;
            parser.consume_any(Token::Space);
            let path = parser.parse_path()?;
            Ok(path)
        })
    }

    fn parse_directive(&mut self) -> ParseResult<Directive> {
        self.span_and_trace("parse_directive", |parser| {
            parser.expect(Token::DirectiveStart, "expected '#['")?;
            parser.consume_any(Token::Space);

            let directive = parser.spanned(|parser| {
                let Some(Token::Directive(directive)) = parser.consume_if(Token::is_directive)
                else {
                    let err = SyntaxError::new("expected directive", parser.position());
                    return Err(ParseError::from(err));
                };
                Ok(directive)
            })?;

            let directive = match directive.as_str() {
                "associativity" => {
                    parser.expect(Token::AssignOp, "expected '='")?;
                    parser.consume_any(Token::Space);

                    let assoc =
                        parser.expect_map("expected \"left\" or \"right\"", |t| match &t {
                            Token::String(value) => match value.as_str() {
                                "left" => Some(OpAssoc::Left),
                                "right" => Some(OpAssoc::Right),
                                _ => None,
                            },
                            _ => None,
                        })?;

                    parser.config.associativity = assoc;
                    Directive::associativity(assoc)
                }
                "binary_coercion" => {
                    parser.expect(Token::AssignOp, "expected '='")?;
                    parser.consume_any(Token::Space);

                    let arg = parser.parse_string_lit()?;
                    let behavior = match arg.value.as_str() {
                        "left" => BinaryCoercion::Left,
                        "right" => BinaryCoercion::Right,
                        "float_or_left" => BinaryCoercion::FloatOrLeft,
                        "int_or_left" => BinaryCoercion::IntOrLeft,
                        _ => {
                            let err = ValueError::new(
                                "expected one of: \"left\", \"right\", \"float_or_left\", or \"int_or_left\"",
                                arg.as_spanned_string(),
                            );
                            return Err(ParseError::from(err));
                        }
                    };
                    Directive::binary_coercion(behavior)
                }
                "coercion" => {
                    parser.expect(Token::AssignOp, "expected '='")?;
                    parser.consume_any(Token::Space);

                    let arg = parser.parse_string_lit()?;
                    let behavior = match arg.value.as_str() {
                        "auto" => Coercion::Auto,
                        "never" => Coercion::Never,
                        _ => {
                            let err = ValueError::new(
                                "expected one of: \"auto\" or \"never\"",
                                arg.as_spanned_string(),
                            );
                            return Err(ParseError::from(err));
                        }
                    };
                    Directive::coerce(behavior)
                }
                "float_conversion" => {
                    parser.expect(Token::AssignOp, "expected '='")?;
                    parser.consume_any(Token::Space);

                    let arg = parser.parse_string_lit()?;
                    let behavior = match arg.value.as_str() {
                        "trunc" => FloatConversion::Trunc,
                        "round" => FloatConversion::Round,
                        _ => {
                            let err = ValueError::new(
                                "expected one of: \"trunc\" or \"round\"",
                                arg.as_spanned_string(),
                            );
                            return Err(ParseError::from(err));
                        }
                    };
                    Directive::float_conversion(behavior)
                }
                "precedence" => {
                    parser.expect(Token::AssignOp, "expected '='")?;
                    parser.consume_any(Token::Space);

                    let prec = parser.parse_integer()?.to_isize_wrapping();
                    parser.config.precedence = prec;
                    Directive::precedence(prec)
                }
                "precision" => {
                    parser.expect(Token::AssignOp, "expected '='")?;
                    parser.consume_any(Token::Space);

                    let prec = parser.parse_integer()?.to_u32_wrapping();
                    Directive::precision(prec)
                }
                _ => {
                    let (directive, span) = directive.into_pair();
                    let err = DirectiveError::new("unknown directive", directive, span);
                    return Err(ParseError::from(err));
                }
            };

            parser.consume_any(Token::Space);
            parser.expect(Token::DirectiveEnd, "expected ']'")?;
            Ok(directive)
        })
    }

    // dimension_decl ::= 'dimension' _ <ident> _ ['=' <dim_expr>]
    fn parse_dim_decl(&mut self) -> ParseResult<DimDecl> {
        self.span_and_trace("parse_dim_decl", |parser| {
            parser.expect(Token::Keyword(Keyword::Dimension), "expected 'dimension'")?;
            parser.consume_any(Token::Space);

            let name = parser.parse_ident()?;
            parser.consume_any(Token::Space);

            if let Some(Token::AssignOp) = parser.consume_if(|t| t == &Token::AssignOp) {
                parser.consume_any(Token::Space);
                let expr = parser.parse_dim_expr()?;
                Ok(DimDecl::new(name, Some(expr)))
            } else {
                Ok(DimDecl::new(name, None))
            }
        })
    }

    // base_unit_decl ::= 'base' _ 'unit' _ <ident> _ [<suffix_list>] _ '=' _ <dim_expr>
    fn parse_base_unit_decl(&mut self) -> ParseResult<UnitDecl> {
        self.span_and_trace("parse_base_unit_decl", |parser| {
            parser.expect(Token::Keyword(Keyword::Base), "expected 'base'")?;
            parser.consume_any(Token::Space);
            parser.expect(Token::Keyword(Keyword::Unit), "expected 'unit'")?;
            parser.consume_any(Token::Space);

            let name = parser.parse_ident()?;
            parser.consume_any(Token::Space);

            let suffixes = if parser.peek_token() == &Token::LDelim("{") {
                parser.parse_suffix_list()?
            } else {
                vec![]
            };

            parser.consume_any(Token::Space);
            parser.expect(Token::AssignOp, "expected '='")?;
            parser.consume_any(Token::Space);

            let dim_expr = parser.parse_dim_expr()?;
            Ok(UnitDecl::base_unit(name, suffixes, dim_expr))
        })
    }

    // sub_unit_decl ::= 'unit' _ <ident> _ [<suffix_list>] '[' <dim_expr> ']' _ '=' <expr>
    fn parse_sub_unit_decl(&mut self) -> ParseResult<UnitDecl> {
        self.span_and_trace("parse_sub_unit_decl", |parser| {
            parser.expect(Token::Keyword(Keyword::Unit), "expected 'unit'")?;
            parser.consume_any(Token::Space);

            let name = parser.parse_ident()?;
            parser.consume_any(Token::Space);

            let suffixes = if parser.peek_token() == &Token::LDelim("{") {
                parser.parse_suffix_list()?
            } else if parser.peek_token() == &Token::LDelim("[") {
                vec![]
            } else {
                return Err(SyntaxError::new("expected '[' or '{'", parser.position()).into());
            };

            parser.consume_any(Token::Space);
            parser.expect(Token::LDelim("["), "expected '['")?;
            parser.consume_any(Token::Space);

            let dimension = parser.parse_dim_expr()?;
            parser.consume_any(Token::Space);
            parser.expect(Token::RDelim("]"), "expected ']'")?;

            parser.consume_any(Token::Space);
            parser.expect(Token::AssignOp, "expected '='")?;
            parser.consume_any(Token::Space);

            let scalar = parser.parse_expr(0)?;
            Ok(UnitDecl::sub_unit(name, suffixes, dimension, scalar))
        })
    }

    // op_decl_assign ::= <op_decl_start> _ '(' <operator> ')' _ <op_decl_param_list> _ '=' <expr>
    // op_decl_define ::= <op_decl_start> _ 'fn' _ '(' <operator> ')' _ <op_fn_param_list> _ <block_expr>
    fn parse_op_decl(&mut self) -> ParseResult<OpDecl> {
        self.span_and_trace("parse_op_decl", |parser| {
            let op_kind = parser.expect_map("expected operator keyword", |t| match &t {
                Token::Keyword(Keyword::Prefix) => Some(OpKind::Prefix),
                Token::Keyword(Keyword::Postfix) => Some(OpKind::Postfix),
                Token::Keyword(Keyword::Infix) => Some(OpKind::Infix),
                _ => None,
            })?;

            parser.consume_any(Token::Space);
            parser.expect(Token::Keyword(Keyword::Operator), "expected 'operator'")?;
            parser.consume_any(Token::Space);

            let is_fn = if parser.consume_one(Token::Keyword(Keyword::Fn)).is_some() {
                parser.consume_any(Token::Space);
                true
            } else {
                false
            };

            parser.expect(Token::LDelim("("), "expected '('")?;
            parser.consume_any(Token::Space);
            let op = parser.parse_operator(op_kind, /* is_decl= */ true)?;
            parser.consume_any(Token::Space);

            parser.expect(Token::RDelim(")"), "expected ')'")?;
            parser.consume_any(Token::Space);

            let params = parser.parse_list(Token::Comma, PAREN_DELIM, |p| {
                if is_fn {
                    p.parse_op_fn_param()
                } else {
                    p.parse_op_decl_param()
                }
            })?;
            parser.consume_any(Token::Space);

            let body = if is_fn {
                Right(parser.parse_block_expr()?)
            } else {
                parser.expect(Token::AssignOp, "expected '='")?;
                parser.consume_any(Token::Space);
                Left(parser.parse_path()?)
            };

            Ok(OpDecl::new(
                op,
                op_kind,
                parser.config.associativity,
                parser.config.precedence,
                params,
                body,
            ))
        })
    }

    // fn_decl ::= 'fn' _ <ident> _ <param_list> _ [ <ret_ret> ] _ <block_expr>
    fn parse_fn_decl(&mut self) -> ParseResult<FnDecl> {
        self.span_and_trace("parse_fn_decl", |parser| {
            parser.expect(Token::Keyword(Keyword::Fn), "expected 'fn'")?;
            parser.consume_any(Token::Space);

            let name = parser.parse_ident()?;
            parser.consume_any(Token::Space);

            let params = parser.parse_list(Token::Comma, PAREN_DELIM, |p| p.parse_param())?;
            parser.consume_any(Token::Space);

            let ret = if parser.peek_token() == &Token::LDelim("[") {
                Some(Left(parser.parse_dim_ret()?))
            } else if parser.peek_token().is_identifier() {
                Some(Right(parser.parse_type()?))
            } else if parser.peek_token() == &Token::LDelim("{") {
                None
            } else {
                return Err(
                    SyntaxError::new("expected return type or '{'", parser.position()).into(),
                );
            };

            parser.consume_any(Token::Space);
            let body = parser.parse_block_expr()?;
            Ok(FnDecl::new(name, params, body, ret))
        })
    }

    // dim_expr ::= <dim_expr_term> _ ('*' | '/') _ <dim_expr>
    //           | <dim_expr_term> _ <dim_expr>
    //           | <dim_expr_term>
    fn parse_dim_expr(&mut self) -> ParseResult<DimExpr> {
        self.span_and_trace("parse_dim_expr", |parser| {
            let term = parser.parse_dim_expr_term()?;
            parser.consume_any(Token::Space);

            if parser.consume_one(Token::operator("*")).is_some() {
                let rhs = parser.parse_dim_expr_term()?;
                Ok(DimExpr::mul(term, rhs))
            } else if parser.consume_one(Token::operator("/")).is_some() {
                let rhs = parser.parse_dim_expr_term()?;
                Ok(DimExpr::div(term, rhs))
            } else if is_dim_expr(parser.peek_token()) {
                let rhs = parser.parse_dim_expr_term()?;
                Ok(DimExpr::mul(term, rhs))
            } else {
                Ok(term)
            }
        })
    }

    // dim_expr_term ::= <dim_expr_atom> _ '^' _ <dim_expr>
    //                 | <dim_expr_atom>
    fn parse_dim_expr_term(&mut self) -> ParseResult<DimExpr> {
        self.span_and_trace("parse_dim_expr_term", |parser| {
            let atom = parser.parse_dim_expr_atom()?;
            parser.consume_any(Token::Space);

            if parser.consume_one(Token::operator("^")).is_some() {
                let term = parser.parse_dim_expr()?;
                Ok(DimExpr::pow(atom, term))
            } else {
                Ok(atom)
            }
        })
    }

    // dim_expr_atom ::= ('+' | '-') _ <dim_expr>
    //                 | '(' _ <dim_expr> _ ')'
    //                 | <ident>
    //                 | <number>
    fn parse_dim_expr_atom(&mut self) -> ParseResult<DimExpr> {
        self.span_and_trace("parse_dim_expr_atom", |parser| {
            if parser.consume_one(Token::operator("+")).is_some() {
                Ok(parser.parse_dim_expr()?)
            } else if parser.consume_one(Token::operator("-")).is_some() {
                let expr = parser.parse_dim_expr()?;
                Ok(DimExpr::neg(expr))
            } else if parser.consume_one(Token::LDelim("(")).is_some() {
                parser.consume_any(Token::Space);
                let expr = parser.parse_dim_expr()?;
                parser.consume_any(Token::Space);
                parser.expect(Token::RDelim(")"), "expected ')'")?;
                Ok(expr)
            } else if matches!(parser.peek_token(), &Token::Identifier(_)) {
                let ident = parser.parse_ident()?;
                Ok(DimExpr::ident(ident))
            } else if parser.peek_token().is_number() {
                let number = parser.parse_number()?;
                Ok(DimExpr::number(number))
            } else {
                Err(SyntaxError::new("expected dimension expression", parser.position()).into())
            }
        })
    }

    // expr ::= <expr_term> _ <infix_op> _ <expr>
    //       | <expr_term> _ <postfix_op>
    //       | <expr_term> _ <unit>
    //       | <expr_term>
    fn parse_expr(&mut self, min_prec: isize) -> ParseResult<Expr> {
        self.trace(&format!("parse_expr [min_prec={}]", min_prec), |parser| {
            let mut start_pos = parser.pos;
            let mut lhs = parser.parse_expr_term()?;
            while true {
                parser.trace_debug("parsing expr loop");

                parser.consume_any(Token::Space);
                if is_postfix_op(parser.peek_token(), &parser.ctx) {
                    let op = parser.parse_operator(OpKind::Postfix, /*is_decl=*/ false)?;
                    parser.consume_any(Token::Space);

                    lhs = Expr::postfix_op(lhs, op).with_span(SourceSpan::new(
                        parser.source_id,
                        start_pos,
                        parser.pos,
                    ));
                    start_pos = parser.pos;
                } else if is_infix_op(parser.peek_token(), &parser.ctx) {
                    let operator = parser.peek_operator(OpKind::Infix)?;
                    let op = parser
                        .ctx
                        .resolve_operator(OpKind::Infix, operator.as_spanned_ustr())?;
                    if op.prec < min_prec {
                        parser.trace_debug("breaking loop");
                        break;
                    }

                    let mut next_prec = op.prec;
                    if op.is_right() {
                        next_prec += 1;
                    }

                    // consume operator
                    parser.parse_operator(OpKind::Infix, /*is_decl=*/ false)?;
                    parser.consume_any(Token::Space);

                    let rhs = parser.parse_expr(next_prec)?;
                    lhs = Expr::infix_op(operator, lhs, rhs).with_span(SourceSpan::new(
                        parser.source_id,
                        start_pos,
                        parser.pos,
                    ));
                    start_pos = parser.pos;
                } else if is_unit_suffix(parser.peek_token(), &parser.ctx) {
                    let unit = parser.parse_unit()?;
                    lhs = Expr::unit(lhs, unit).with_span(SourceSpan::new(
                        parser.source_id,
                        start_pos,
                        parser.pos,
                    ));
                    start_pos = parser.pos;
                } else {
                    break;
                }
            }

            Ok(lhs)
        })
    }

    // expr_term ::= '(' _ <expr> _ ')'
    //             | <prefix_op> _ <expr>
    //             | <expr_atom>
    fn parse_expr_term(&mut self) -> ParseResult<Expr> {
        self.trace("parse_expr_term", |parser| {
            if parser.consume_one(Token::LDelim("(")).is_some() {
                parser.consume_any(Token::Space);
                let expr = parser.parse_expr(0)?;
                parser.consume_any(Token::Space);
                parser.expect(Token::RDelim(")"), "expected ')'")?;
                Ok(expr)
            } else if is_prefix_op(parser.peek_token(), &parser.ctx) {
                let operator = parser.parse_operator(OpKind::Prefix, /*is_decl=*/ false)?;
                parser.consume_any(Token::Space);

                let op = parser
                    .ctx
                    .resolve_operator(OpKind::Prefix, operator.as_spanned_ustr())?;

                let rhs = parser.parse_expr(op.prec)?;
                Ok(Expr::prefix_op(operator, rhs))
            } else {
                parser.parse_expr_atom()
            }
        })
    }

    // expr_atom ::= 'if' <expr> <block_expr> 'else' <block_expr>
    //             | <path>
    //             | <number>
    fn parse_expr_atom(&mut self) -> ParseResult<Expr> {
        self.trace("parse_expr_atom", |parser| {
            if parser.consume_one(Token::Keyword(Keyword::If)).is_some() {
                parser.consume_any(Token::Space);
                let cond = parser.parse_expr(0)?;
                parser.consume_any(Token::Space);
                let then = parser.parse_block_expr()?;
                parser.consume_any(Token::Space);
                parser.expect(Token::Keyword(Keyword::Else), "expected 'else'")?;
                parser.consume_any(Token::Space);
                let else_ = parser.parse_block_expr()?;
                Ok(Expr::if_else(cond, then, else_))
            } else if parser.peek_token().is_identifier() {
                let path = parser.parse_path()?;
                Ok(Expr::path(path))
            } else if parser.peek_token().is_number() {
                let number = parser.parse_number()?;
                Ok(Expr::number(number))
            } else {
                Err(SyntaxError::new("expected expression", parser.position()).into())
            }
        })
    }

    // block_expr ::= '{' [ '\n' ] _ <expr> ('\n' _ <expr> _)* _ [ '\n' ] '}'
    fn parse_block_expr(&mut self) -> ParseResult<ListNode<Expr>> {
        self.span_and_trace("parse_block_expr", |parser| {
            parser.expect(Token::LDelim("{"), "expected '{'")?;
            let l_delim = ("{", parser.position());
            parser.consume_one(Token::NewLine);
            parser.consume_any(Token::Space);

            let mut exprs = vec![parser.parse_expr(0)?];
            parser.consume_any(Token::Space);

            while parser.peek_token() != &Token::RDelim("}") {
                parser.consume_any(Token::Space);
                if !exprs.is_empty() {
                    parser.expect(Token::NewLine, "expected newline")?;
                    parser.consume_any(Token::Space);
                    if parser.peek_token() == &Token::RDelim("}") {
                        break;
                    }
                }

                exprs.push(parser.parse_expr(0)?);
                parser.consume_any(Token::Space);
            }

            parser.expect(Token::RDelim("}"), "expected '}'")?;
            let r_delim = ("}", parser.position());
            Ok(ListNode::new(exprs).with_delims(l_delim, r_delim))
        })
    }

    // dim_ret ::= '[' _ <dim_expr> _ ']'
    fn parse_dim_ret(&mut self) -> ParseResult<DimExpr> {
        self.span_and_trace("parse_dim_ret", |parser| {
            parser.consume_any(Token::Space);
            let dim = parser.parse_dim_expr()?;
            parser.consume_any(Token::Space);
            parser.expect(Token::RDelim("]"), "expected ']'")?;
            Ok(dim)
        })
    }

    // param ::= <ident> '[' <dim_expr> ']'
    //         | <ident> ':' <type>
    //         | <ident>
    fn parse_param(&mut self) -> ParseResult<Param> {
        self.span_and_trace("parse_param", |parser| {
            let name = parser.parse_ident()?;
            let anno = if parser.consume_one(Token::LDelim("[")).is_some() {
                let dim = parser.parse_dim_expr()?;
                parser.expect(Token::RDelim("]"), "expected ']'")?;
                Some(Either::Left(dim))
            } else if parser.consume_one(Token::Colon).is_some() {
                let ty = parser.parse_type()?;
                Some(Either::Right(ty))
            } else {
                None
            };

            Ok(Param::new(name, anno))
        })
    }

    // op_decl_param ::= '[' <dim_expr> ']'
    //            | <type>
    fn parse_op_decl_param(&mut self) -> ParseResult<Param> {
        self.span_and_trace("parse_op_decl_param", |parser| {
            let anno = if parser.consume_one(Token::LDelim("[")).is_some() {
                let dim = parser.parse_dim_expr()?;
                parser.expect(Token::RDelim("]"), "expected ']'")?;
                Some(Either::Left(dim))
            } else {
                let ty = parser.parse_type()?;
                Some(Either::Right(ty))
            };

            let name = Ident::new(Ustr::from(""));
            Ok(Param::new(name, anno))
        })
    }

    // op_fn_param ::= <ident> _ '[' _ <dim_expr> _ ']'
    //               | <ident> _ ':' _ <type>
    fn parse_op_fn_param(&mut self) -> ParseResult<Param> {
        self.span_and_trace("parse_op_fn_param", |parser| {
            let name = parser.parse_ident()?;
            parser.consume_any(Token::Space);

            let anno = if parser.consume_one(Token::LDelim("[")).is_some() {
                parser.consume_any(Token::Space);
                let dim = parser.parse_dim_expr()?;
                parser.consume_any(Token::Space);
                parser.expect(Token::RDelim("]"), "expected ']'")?;
                Some(Either::Left(dim))
            } else if parser.consume_one(Token::Colon).is_some() {
                parser.consume_any(Token::Space);
                let ty = parser.parse_type()?;
                Some(Either::Right(ty))
            } else {
                return Err(
                    SyntaxError::new("expected parameter annotation", parser.position()).into(),
                );
            };

            Ok(Param::new(name, anno))
        })
    }

    // suffix_list ::= '{' ((_ <ident> _) ** ',') '}'
    fn parse_suffix_list(&mut self) -> ParseResult<Vec<Ident>> {
        self.trace("parse_suffix_list", |parser| {
            parser.expect(Token::LDelim("{"), "expected '{'")?;
            parser.consume_any(Token::Space);

            let mut suffixes = vec![];
            while parser.peek_token() != &Token::RDelim("}") {
                suffixes.push(parser.parse_ident()?);
                parser.consume_one(Token::Comma);
                parser.consume_any(Token::Space);
            }

            parser.expect(Token::RDelim("}"), "expected '}'")?;
            Ok(suffixes)
        })
    }

    // path ::= <ident> [ '::' (<ident> ** '::') ]
    fn parse_path(&mut self) -> ParseResult<Path> {
        self.span_and_trace("parse_path", |parser| {
            let mut parts = vec![];
            parts.push(parser.parse_ident()?);
            while parser.consume_one(Token::PathOp).is_some() {
                parts.push(parser.parse_ident()?);
            }

            Ok(Path::new(parts))
        })
    }

    // type ::= 'any' | 'int' | 'float' | 'num'
    fn parse_type(&mut self) -> ParseResult<Ty> {
        self.span_and_trace("parse_type", |parser| {
            let raw_ty: Spanned<Ustr> = parser.spanned(|parser| {
                parser.expect_map("expected type", |t| match t {
                    Token::Identifier(raw) => Some(raw.clone()),
                    _ => None,
                })
            })?;

            let ty = match raw_ty.as_str() {
                "any" => Ty::any(),
                "int" => Ty::int(),
                "float" => Ty::float(),
                "num" => Ty::num(),
                _ => {
                    let err = ValueError::new("invalid type", raw_ty.to_string_inner())
                        .with_extra("expected one of: any, int, float or num".to_owned());
                    return Err(ParseError::from(err));
                }
            };
            Ok(ty)
        })
    }

    // operator ::= <op>+
    fn parse_operator(&mut self, kind: OpKind, is_decl: bool) -> ParseResult<Operator> {
        self.span_and_trace(&format!("parse_operator<{:?}>", kind), |parser| {
            parser.trace_debug(format!("parsing operator [is_decl={}]", is_decl));
            // operators are kept ambiguous during lexing therefore a single 'real' operator
            // may be formed by combining multiple single operator tokens. this is handled
            // according to the context which contains a record of all registered operators
            // by kind that we can reference to validate the operator.
            let mut op = parser.expect_map("expected operator", |t| match t {
                Token::Operator(s) => Some(s.as_str().to_owned()),
                _ => None,
            })?;

            parser.trace_debug(&format!(
                "peeked token: {}",
                parser.peek_token().pretty_string(&())
            ));

            // while parsing an operator at a non-declaration site, only consume additional
            // tokens if we're sure that it forms a valid operator with the tokens we have
            // so far. for the single token case we dont need to check the operator table.
            // actual validation of the operator is done later.
            while let Some(raw) = parser.peek_token().get_operator() {
                let tmp = format!("{}{}", op, raw);
                if !is_decl {
                    let op = Ustr::from(&tmp);
                    parser.trace_debug(&format!(
                        "checking operator '{}' -> {}",
                        tmp,
                        parser.ctx.operators.contains(op)
                    ));
                    if !parser.ctx.operators.contains(op) {
                        break;
                    }
                }

                // consume the token
                parser.next_token()?;
                op = tmp;
            }

            Ok(Operator::new(Ustr::from(&op), kind))
        })
    }

    fn parse_unit(&mut self) -> ParseResult<Unit> {
        self.span_and_trace("parse_unit", |parser| match parser.next_token()? {
            Token::Identifier(ident) => Ok(Unit::new(ident)),
            _ => Err(SyntaxError::new("expected unit suffix", parser.position()).into()),
        })
    }

    fn parse_ident(&mut self) -> ParseResult<Ident> {
        self.span_and_trace("parse_ident", |parser| match parser.next_token()? {
            Token::Identifier(ident) => Ok(Ident::new(ident)),
            _ => Err(SyntaxError::new("expected identifier", parser.position()).into()),
        })
    }

    fn parse_number(&mut self) -> ParseResult<Number> {
        self.span_and_trace("parse_number", |parser| {
            let token = parser.next_token()?;
            match token {
                Token::Integer(raw, radix) => {
                    let integer = Integer::parse_radix(raw, radix)
                        .map_err(|_| SyntaxError::new("invalid integer", parser.position()))?
                        .complete();
                    Ok(Number::integer(integer))
                }
                Token::Float(raw) => {
                    let float = Float::parse(raw)
                        .map_err(|_| SyntaxError::new("invalid float", parser.position()))?
                        .complete(FLOAT_LIT_PRECISION);
                    Ok(Number::float(float))
                }
                _ => Err(SyntaxError::new("expected number", parser.position()).into()),
            }
        })
    }

    fn parse_integer(&mut self) -> ParseResult<Integer> {
        let number = self.parse_number()?;
        match number.kind {
            NumberKind::Integer(v) => Ok(v),
            NumberKind::Float(_) => {
                Err(SyntaxError::new("expected integer", self.position()).into())
            }
        }
    }

    fn parse_string_lit(&mut self) -> ParseResult<StringLit> {
        self.span_and_trace("parse_string_lit", |parser| {
            let token = parser.next_token()?;
            match token {
                Token::String(s) => Ok(StringLit::new(s)),
                _ => Err(SyntaxError::new("expected string literal", parser.position()).into()),
            }
        })
    }

    //

    fn parse_list<F, T>(
        &mut self,
        sep: Token,
        delim: (Token, Token),
        f: F,
    ) -> ParseResult<ListNode<T>>
    where
        T: Spannable + PrettyPrint<()> + std::fmt::Debug,
        F: Fn(&mut Self) -> ParseResult<T>,
    {
        self.span_and_trace("parse_list", |parser| {
            parser.expect(delim.0.clone(), &format!("expected '{}'", delim.0))?;
            parser.consume_any(Token::Space);

            let mut items = vec![f(parser)?];
            parser.consume_any(Token::Space);

            while parser.consume_if(|t| t == &sep).is_some() {
                parser.consume_any(Token::Space);
                items.push(f(parser)?);
                parser.consume_any(Token::Space);
            }

            parser.expect(delim.1.clone(), &format!("expected '{}'", delim.0))?;
            Ok(ListNode::new(items))
        })
    }
}

impl<'a> Parser<'a> {
    fn position(&self) -> SourcePos {
        SourcePos::new(self.source_id, self.pos)
    }

    fn peek_token(&self) -> &Token {
        if self.idx < self.tokens.len() {
            &self.tokens[self.idx].0
        } else {
            &Token::EndOfFile
        }
    }

    fn peek_next(&self) -> (&Token, SourceSpan) {
        if self.idx < self.tokens.len() {
            let (token, span) = &self.tokens[self.idx];
            (token, *span)
        } else {
            (&Token::EndOfFile, SourceSpan::INVALID)
        }
    }

    fn peek_operator(&mut self, kind: OpKind) -> ParseResult<Operator> {
        let idx = self.idx;
        let pos = self.pos;
        let source_id = self.source_id;

        let op = self.parse_operator(kind, /* is_decl= */ false)?;
        self.idx = idx;
        self.pos = pos;
        self.source_id = source_id;
        Ok(op)
    }

    fn next_token(&mut self) -> ParseResult<Token> {
        if self.idx < self.tokens.len() {
            let (token, span) = self.tokens[self.idx].clone();
            self.idx += 1;

            self.pos = span.end;
            self.source_id = span.source_id;

            self.trace_debug(format!(
                "next_token(): consumed {} ({}:{})",
                token.pretty_string(&()),
                span.start,
                span.end
            ));
            Ok(token)
        } else {
            Err(SyntaxError::new("unexpected end of input", self.position()).into())
        }
    }

    fn span<F, T>(&mut self, f: F) -> ParseResult<T>
    where
        F: FnOnce(&mut Self) -> ParseResult<T>,
        T: Spannable,
    {
        let start = self.pos;
        let value = f(self)?;
        let end = self.pos;
        let source_id = self.source_id;
        Ok(value.with_span(SourceSpan::new(source_id, start, end)))
    }

    fn spanned<F, T>(&mut self, f: F) -> ParseResult<Spanned<T>>
    where
        F: FnOnce(&mut Self) -> ParseResult<T>,
    {
        let start = self.pos;
        let value = f(self)?;
        let end = self.pos;
        let span = SourceSpan::new(self.source_id, start, end);
        Ok(span.into_spanned(value))
    }

    fn trace<F, T: PrettyPrint<()>>(&mut self, msg: &str, f: F) -> ParseResult<T>
    where
        F: FnOnce(&mut Self) -> ParseResult<T>,
        T: std::fmt::Debug,
    {
        let tab = TABWIDTH.repeat(self.trace_level);
        if self.trace_on {
            eprintln!(
                "[TRACE] {{{}}} {}{}: {}",
                self.trace_level,
                tab,
                msg,
                self.peek_token().pretty_string(&())
            );
        }

        self.trace_level += 1;
        let result = f(self);
        self.trace_level -= 1;

        if self.trace_on {
            eprintln!(
                "[TRACE] {{{}}} {}{}: Result: {}",
                self.trace_level,
                tab,
                msg,
                result.pretty_string(&())
            );
        }
        result
    }

    fn span_and_trace<F, T: PrettyPrint<()>>(&mut self, msg: &str, f: F) -> ParseResult<T>
    where
        F: FnOnce(&mut Self) -> ParseResult<T>,
        T: Spannable + std::fmt::Debug,
    {
        self.span(|parser| parser.trace(msg, f))
    }

    fn trace_debug<S: AsRef<str>>(&self, msg: S) {
        let tab = TABWIDTH.repeat(self.trace_level);
        if self.trace_on {
            eprintln!("[TRACE] {{{}}} {}{}", self.trace_level, tab, msg.as_ref());
        }
    }

    fn consume_one(&mut self, token: Token) -> Option<Token> {
        if self.peek_token() == &token {
            Some(self.next_token().expect("expected token"))
        } else {
            None
        }
    }

    fn consume_any(&mut self, token: Token) -> bool {
        let mut consumed = false;
        while self.peek_token() == &token {
            consumed = true;
            self.next_token().expect("expected token");
        }
        consumed
    }

    fn consume_if<F>(&mut self, f: F) -> Option<Token>
    where
        F: Fn(&Token) -> bool,
    {
        if f(&self.peek_token()) {
            Some(self.next_token().expect("expected token"))
        } else {
            None
        }
    }

    fn expect(&mut self, token: Token, msg: &str) -> ParseResult<Token> {
        let next = self.peek_token();
        if next == &token {
            Ok(self.next_token()?)
        } else {
            Err(SyntaxError::new(msg, self.position()).into())
        }
    }

    fn expect_map<F, T>(&mut self, msg: &str, f: F) -> ParseResult<T>
    where
        F: Fn(&Token) -> Option<T>,
    {
        let token = self.peek_token();
        match f(token) {
            Some(value) => {
                self.next_token()?;
                Ok(value)
            }
            None => Err(SyntaxError::new(msg, self.position()).into()),
        }
    }
}

fn is_dim_expr(t: &Token) -> bool {
    t.is_identifier() || t.is_number() || t.is_operator() || matches!(t, Token::LDelim("("))
}

fn is_prefix_op(op: &Token, ctx: &rt::Module) -> bool {
    matches!(op, &Token::Operator(op) if ctx.operators.does_start(OpKind::Prefix, op))
}

fn is_postfix_op(op: &Token, ctx: &rt::Module) -> bool {
    matches!(op, &Token::Operator(op) if ctx.operators.does_start(OpKind::Postfix, op))
}

fn is_infix_op(op: &Token, ctx: &rt::Module) -> bool {
    matches!(op, &Token::Operator(op)) //  if ctx.operators.does_start(OpKind::Infix, op)
}

fn is_unit_suffix(suffix: &Token, ctx: &rt::Module) -> bool {
    matches!(suffix, &Token::Identifier(suffix) if ctx.units.resolve_suffix(suffix).is_some())
}
