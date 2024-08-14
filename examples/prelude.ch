//
// MARK: Operators
//

// types:
//   any
//   bool
//   float
//   int
//   str
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


//
// MARK: Units
//

// base units:
//  second    | s  | time
//  meter     | m  | length
//  kilogram  | kg | mass
//  ampere    | A  | current
//  kelvin    | K  | temperature
//
//  hertz     | Hz | frequency
//  volt      | V  | voltage
//  farad     | F  | capacitance
//  ohm       | Ω  | resistance
//  henry     | H  | inductance
//  degree C  | °C | temperature

dimension T // time
dimension L // length
dimension M // mass
dimension A // current
dimension Θ // temperature
dimension N // amount of substance
dimension J // luminous intensity
dimension V = T^-3 L^2 M A^-1
dimension F = M^-1 L^-2 T^4 A^2
dimension Ω = M L^2 T^-3 A^-2
dimension H = L^2 M T^-2 A^-2

base unit second{s} = T
unit picosecond{ps} [T] = 1.2e-12
unit nanosecond{ns} [T] = 1e-9
unit microsecond{us} [T] = 1e-6
unit millisecond{ms} [T] = 1e-3
unit minute{min} [T] = 60
unit hour{h} [T] = 3600
unit day{d} [T] = 86400
unit week{wk} [T] = 604800
unit month{mo} [T] = 2629746
unit year{yr} [T] = 31556952

base unit meter{m} = L
unit millimeter{mm} [L] = 1e-3
unit centimeter{cm} [L] = 1e-2
unit kilometer{km} [L] = 1e3
unit inch{in} [L] = 0.0254
unit foot{ft} [L] = 0.3048
unit yard{yd} [L] = 0.9144
unit mile{mi} [L] = 1609.34

base unit kilogram{kg} = M
unit gram{g} [M] = 1e-3
unit tonne{t} [M] = 1e3

base unit ampere{I} = A
unit milliampere{mA} [A] = 1e-3
unit microampere{μA,uA} [A] = 1e-6

base unit kelvin{K} = Θ
unit degreeC{dC} [Θ] = 1
unit degreeF{dF} [Θ] = 5/9

base unit hertz{Hz} = 1/T
unit kilohertz{kHz} [1/T] = 1e3
unit megahertz{MHz} [1/T] = 1e6
unit gigahertz{GHz} [1/T] = 1e9

base unit volt{V} = V
unit millivolt{mV} [V] = 1e-3
unit kilovolt{kV} [V] = 1e3

base unit farad{F} = F
unit millifarad{mF} [F] = 1e-3
unit microfarad{μF,uF} [F] = 1e-6
unit nanofarad{nF} [F] = 1e-9
unit picofarad{pF} [F] = 1e-12

base unit ohm{Ω,R} = Ω
unit milliohm{mΩ,mR} [Ω] = 1e-3
unit kiloohm{kΩ,kR} [Ω] = 1e3
unit megaohm{MΩ,MR} [Ω] = 1e6

base unit henry{H} = H
unit millihenry{mH} [H] = 1e-3
unit microhenry{μH,uH} [H] = 1e-6
unit nanohenry{nH} [H] = 1e-9
unit picohenry{pH} [H] = 1e-12
