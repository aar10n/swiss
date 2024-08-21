// Sets the associativity for operator declarations.
//   left  - Left associative.
//   right - Right associative.
#[associativity="left"]

// Sets the precedence for operator declarations. <isize>
#[precedence=0]

// Sets the precision used for floating point calculations. <u32>
#[float_precision=0]

// Controls how floats are converted to integers.
//   trunc  - Truncate towards zero.
//   round  - Round to the nearest integer.
#[float_conversion="trunc"]

// Controls type coercion.
//   auto  - Coerce types automatically.
//   never - Never coerce types.
#[coercion="auto"]

// Controls the behavior of type coercion in most binary operators.
//   left          - Coerce the rhs to the type of the lhs.
//   right         - Coerce the lhs to the type of the rhs.
//   float_or_left - If any side is a float, coerce the other side to it. (or left)
//   int_or_left   - If any side is an int, coerce the other side to it. (or left)
#[binary_coercion="float_or_left"]

// Controls from where the unit for results of quantity operations should be taken.
//   left  - Take the unit from the left side.
//   right - Take the unit from the right side.
#[unit_preference="left"]
