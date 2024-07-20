// types:
//   any
//   int
//   float
//   num

#[associativity="left"]
#[precedence=0]
infix operator (==)(int,int) = builtin::eq
infix operator (!=)(int,int) = builtin::ne
#[precedence=1]
infix operator (<)(int,int) = builtin::lt
infix operator (>)(int,int) = builtin::gt
infix operator (<=)(int,int) = builtin::le
infix operator (>=)(int,int) = builtin::ge
#[precedence=2]
infix operator (||)(int,int) = builtin::or
#[precedence=3]
infix operator (&&)(int,int) = builtin::and
#[precedence=4]
infix operator (<<)(num,int) = builtin::bit_shl
infix operator (>>)(num,int) = builtin::bit_shr
// ------------------------
#[associativity="right"]
#[precedence=5]
prefix operator (+)(num) = builtin::pos
prefix operator (-)(num) = builtin::neg
prefix operator (!)(num) = builtin::not
prefix operator (~)(num) = builtin::bit_not
// ------------------------
#[associativity="left"]
#[precedence=6]
infix operator (+)(num,num) = builtin::add
infix operator (-)(num,num) = builtin::sub
infix operator (|)(num,int) = builtin::bit_or
#[precedence=7]
infix operator (*)(num,num) = builtin::mul
infix operator (/)(num,num) = builtin::div
//infix operator (%)(num,num) = builtin::mod
infix operator (&)(num,int) = builtin::bit_and
// ------------------------
#[precedence=8]
infix operator (^)(num,num) = builtin::pow
