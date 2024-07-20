#[associativity="right"]
#[precedence=0]
prefix operator (+)(num) = builtin::pos
prefix operator (-)(num) = builtin::neg
prefix operator (!)(num) = builtin::not
prefix operator (~)(num) = builtin::bit_not
// ------------------------
#[associativity="left"]
#[precedence=1]
infix operator (+)(num,num) = builtin::add
infix operator (-)(num,num) = builtin::sub
infix operator (|)(num,int) = builtin::bit_or
#[precedence=2]
infix operator (*)(num,num) = builtin::mul
infix operator (/)(num,num) = builtin::div
//infix operator (%)(num,num) = builtin::mod
infix operator (&)(num,int) = builtin::bit_and
// ------------------------
#[precedence=3]
infix operator (^)(num,num) = builtin::pow
// ------------------------
