import std

base unit second{s} = T
unit minute{min} [T] = 60
unit hour{hr} [T] = 3600
unit day{dy} [T] = 86400
unit week{wk} [T] = 604800

#[associativity="left"]
#[precedence=2]
infix operator (*) = builtin::mul

fn min(a, b) { if a < b { a } else { b } }
