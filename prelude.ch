;
; Operators
;

; types:
;   any
;   bool
;   float
;   int
;   str
;   num
;   list
;   tuple[T...] or (T...)
;   unit
;   type

#[associativity="right"]
#[precedence=0]
infix operator (+=)(&num,num) = builtin::add_assign
infix operator (-=)(&num,num) = builtin::sub_assign
infix operator (*=)(&num,num) = builtin::mul_assign
infix operator (/=)(&num,num) = builtin::div_assign
#[associativity="left"]
#[precedence=1]
infix operator (==)(num,num) = builtin::eq
infix operator (!=)(num,num) = builtin::ne
#[precedence=2]
infix operator (<)(num,num) = builtin::lt
infix operator (>)(num,num) = builtin::gt
infix operator (<=)(num,num) = builtin::le
infix operator (>=)(num,num) = builtin::ge
#[precedence=3]
infix operator (||)(num,num) = builtin::or
#[precedence=4]
infix operator (&&)(num,num) = builtin::and
#[precedence=5]
infix operator (<<)(num,num) = builtin::bit_shl
infix operator (>>)(num,num) = builtin::bit_shr
; ------------------------
#[associativity="right"]
#[precedence=6]
prefix operator (+)(num) = builtin::pos
prefix operator (-)(num) = builtin::neg
prefix operator (!)(num) = builtin::not
prefix operator (~)(num) = builtin::bit_not
; ------------------------
#[associativity="left"]
#[precedence=7]
infix operator (+)(num,num) = builtin::add
infix operator (-)(num,num) = builtin::sub
infix operator (|)(num,num) = builtin::bit_or
#[precedence=8]
infix operator (*)(num,num) = builtin::mul
infix operator (/)(num,num) = builtin::div
infix operator (%)(num,num) = builtin::mod
infix operator (&)(num,num) = builtin::bit_and
; ------------------------
#[precedence=9]
infix operator (^)(num,num) = builtin::pow
#[precedence=10]
infix operator (->)(num,unit) = builtin::unit_cast

;
; Units
;

; base units:
;  second    | s  | time
;  meter     | m  | length
;  kilogram  | kg | mass
;  ampere    | A  | current
;  kelvin    | K  | temperature
;
;  hertz     | Hz | frequency
;  volt      | V  | voltage
;  farad     | F  | capacitance
;  ohm       | Ω  | resistance
;  henry     | H  | inductance
;  degree C  | °C | temperature

dimension T                     ; time
dimension L                     ; length
dimension M                     ; mass
dimension A                     ; current
dimension Θ                     ; temperature
dimension N                     ; amount of substance
dimension J                     ; luminous intensity
dimension V = T^-3 L^2 M A^-1   ; voltage
dimension F = M^-1 L^-2 T^4 A^2 ; capacitance
dimension Ω = M L^2 T^-3 A^-2   ; resistance
dimension H = L^2 M T^-2 A^-2   ; inductance
dimension Angle                 ; angle (dimensionless but tracked)
dimension Data                  ; data storage (dimensionless but tracked)

base unit second{s} = T
unit picosecond{ps} [T] = 1.2e-12
unit nanosecond{ns} [T] = 1e-9
unit microsecond{us} [T] = 1e-6
unit millisecond{ms} [T] = 1e-3
unit minute{min} [T] = 60
unit hour{h,hr} [T] = 3600
unit day{day} [T] = 86400
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
unit degreeC{°C,dC} [Θ] = {
  fn display_name() { "°C" }
  fn to_base(c) { c + 273.15 }
  fn from_base(k) { k - 273.15 }
}
unit degreeF{°F,dF} [Θ] = {
  fn display_name() { "°F" }
  fn to_base(f) { (f - 32) * (5/9) + 273.15 }
  fn from_base(k) { (k - 273.15) * (9/5) + 32 }
}

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

base unit radian{rad} = Angle
unit degree{deg,°} [Angle] = {
  fn to_base(x) { x * pi / 180 }
  fn from_base(x) { x * 180 / pi }
}
unit gradian{grad,gon} [Angle] = {
  fn to_base(x) { x * pi / 200 }
  fn from_base(x) { x * 200 / pi }
}
unit turn{turn} [Angle] = {
  fn to_base(x) { x * 2 * pi }
  fn from_base(x) { x / (2 * pi) }
}

base unit byte{B,byte} = Data
unit kilobyte{KB} [Data] = 1024
unit megabyte{MB} [Data] = 1024 * 1024
unit gigabyte{GB} [Data] = 1024 * 1024 * 1024
unit terabyte{TB} [Data] = 1024 * 1024 * 1024 * 1024
unit kibibyte{KiB} [Data] = 1024
unit mebibyte{MiB} [Data] = 1024 * 1024
unit gibibyte{GiB} [Data] = 1024 * 1024 * 1024
unit tebibyte{TiB} [Data] = 1024 * 1024 * 1024 * 1024

;
; Constants
;

const pi = 3.14159265358979323846
const e = 2.71828182845904523536
const phi = 1.61803398874989484820

;
; Functions
;

fn len(v: any) { builtin::len(v) }
fn print(v...) { builtin::print(v...) }

fn abs(x: num) { if x < 0 { -x } else { x } }
fn sign(x: num) { if x < 0 { -1 } else { if x > 0 { 1 } else { 0 } } }

fn ln(x: num) { builtin::ln(x) }
fn log2(x: num) { builtin::log2(x) }
fn log10(x: num) { builtin::log10(x) }

fn pow(x: num, y: num) { builtin::pow(x, y) }
fn sqrt(x: num) { builtin::sqrt(x) }
fn cbrt(x: num) { builtin::cbrt(x) }
fn exp(x: num) { e ^ x }
fn exp2(x: num) { 2 ^ x }
fn exp10(x: num) { 10 ^ x }

fn floor(x: num) { builtin::floor(x) }
fn ceil(x: num) { builtin::ceil(x) }
fn round(x: num) { builtin::round(x) }

fn sin(x: [rad]) { builtin::sin(x) }
fn cos(x: [rad]) { builtin::cos(x) }
fn tan(x: [rad]) { builtin::tan(x) }
fn asin(x: num) { builtin::asin(x) rad }
fn acos(x: num) { builtin::acos(x) rad }
fn atan(x: num) { builtin::atan(x) rad }
fn atan2(y: num, x: num) { builtin::atan2(y, x) rad }
fn sinh(x: num) { builtin::sinh(x) }
fn cosh(x: num) { builtin::cosh(x) }
fn tanh(x: num) { builtin::tanh(x) }
fn asinh(x: num) { builtin::asinh(x) }
fn acosh(x: num) { builtin::acosh(x) }
fn atanh(x: num) { builtin::atanh(x) }

fn clamp(x: num, min: num, max: num) {
  if x < min {
    min
  } else {
    if x > max {
      max
    } else {
      x
    }
  }
}

fn align(x: num, a: num) {
  if a == 0 {
    x
  } else {
    (x + a - 1) & ~(a - 1)
  }
}

fn align_down(x: num, a: num) {
  if a == 0 {
    x
  } else {
    x & ~(a - 1)
  }
}

fn contains(l: list, v: any) {
  for item := range l {
    print(item, v)
    if item == v {
      return true
    } else {
      continue
    }
  }
  false
}

fn min(vals...) {
  r = inf
  for v := range vals {
    r = if v < r { v } else { r }
  }
  r
}

fn max(vals...) {
  r = -inf
  for v := range vals {
    r = if v > r { v } else { r }
  }
  r
}

fn sum(vals...) {
  r = 0
  for v := range vals {
    r += v
  }
  r
}

fn avg(vals...) {
  sum(vals...) / len(vals)
}
