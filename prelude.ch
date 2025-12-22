;
; Runtime Config
;

; Sets the precision used for floating point calculations. N <u32>
;   (0 > N <= 1024)
#[float_precision=53]

; Sets the number of significant figures used when displaying floats. Option<N u32>
;   (0 > N <= 1024) | () = auto
#[significant_figures=()]

; Controls how floats are converted to integers.
;   trunc  - Truncate towards zero.
;   round  - Round to the nearest integer.
#[float_conversion="round"]

; Controls type coercion.
;   auto  - Coerce types automatically.
;   never - Never coerce types.
#[coercion="auto"]

; Controls the behavior of type coercion in most binary operators.
;   left          - Coerce the rhs to the type of the lhs.
;   right         - Coerce the lhs to the type of the rhs.
;   float_or_left - If any side is a float, coerce the other side to it. (or left)
;   int_or_left   - If any side is an int, coerce the other side to it. (or left)
#[binary_coercion="float_or_left"]

; Controls from where the unit for results of quantity operations should be taken.
;   left  - Take the unit from the left side.
;   right - Take the unit from the right side.
#[unit_preference="left"]


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
infix operator (//)(any,fn) = builtin::format_apply
#[precedence=1]
infix operator (+=)(&num,num) = builtin::add_assign
infix operator (-=)(&num,num) = builtin::sub_assign
infix operator (*=)(&num,num) = builtin::mul_assign
infix operator (/=)(&num,num) = builtin::div_assign
#[associativity="left"]
#[precedence=2]
infix operator (==)(num,num) = builtin::eq
infix operator (!=)(num,num) = builtin::ne
#[precedence=3]
infix operator (<)(num,num) = builtin::lt
infix operator (>)(num,num) = builtin::gt
infix operator (<=)(num,num) = builtin::le
infix operator (>=)(num,num) = builtin::ge
#[precedence=4]
infix operator (||)(num,num) = builtin::or
#[precedence=5]
infix operator (&&)(num,num) = builtin::and
#[precedence=6]
infix operator (<<)(num,num) = builtin::bit_shl
infix operator (>>)(num,num) = builtin::bit_shr
; ------------------------
#[associativity="right"]
#[precedence=7]
prefix operator (+)(num) = builtin::pos
prefix operator (-)(num) = builtin::neg
prefix operator (!)(num) = builtin::not
prefix operator (~)(num) = builtin::bit_not
; ------------------------
#[associativity="left"]
#[precedence=8]
infix operator (+)(num,num) = builtin::add
infix operator (-)(num,num) = builtin::sub
infix operator (|)(num,num) = builtin::bit_or
#[precedence=9]
infix operator (*)(num,num) = builtin::mul
infix operator (/)(num,num) = builtin::div
infix operator (%)(num,num) = builtin::mod
infix operator (&)(num,num) = builtin::bit_and
; ------------------------
#[precedence=10]
infix operator (^)(num,num) = builtin::pow
#[precedence=11]
infix operator (->)(num,unit) = builtin::unit_cast
#[precedence=12]
infix operator ([])(any,any) = builtin::index
infix operator (.)(any,any) = builtin::method_call

;
; Dimensions and Units
;

; ============================================
; Base SI Dimensions
; ============================================

dimension T{time}
dimension L{length}
dimension M{mass}
dimension A{current}
dimension Θ{temperature}
dimension N{amount}
dimension J{luminous_intensity}

; ============================================
; Special Dimensions (dimensionless but tracked)
; ============================================

dimension Angle{angle}
dimension Data{data_storage}

; ============================================
; Base SI Units
; ============================================

base unit second{s} = T
base unit meter{m} = L
base unit kilogram{kg} = M
base unit ampere{I} = A
base unit kelvin{K} = Θ

; ============================================
; Time Units
; ============================================

unit picosecond{ps} = 1e-12 s
unit nanosecond{ns} = 1e-9 s
unit microsecond{us} = 1e-6 s
unit millisecond{ms} = 1e-3 s
unit minute{min} = 60 s
unit hour{h,hr} = 3600 s
unit day{day} = 86400 s
unit week{wk} = 604800 s
unit month{mo} = 2629746 s
unit year{yr} = 31556952 s

; ============================================
; Length Units
; ============================================

unit millimeter{mm} = 1e-3 m
unit centimeter{cm} = 1e-2 m
unit kilometer{km} = 1e3 m

; Imperial/US customary
unit inch{in} = 0.0254 m
unit foot{ft} = 0.3048 m
unit yard{yd} = 0.9144 m
unit mile{mi} = 1609.34 m

; ============================================
; Mass Units
; ============================================

unit gram{g} = 1e-3 kg
unit tonne{t} = 1e3 kg

; ============================================
; Current Units
; ============================================

unit milliampere{mA} = 1e-3 I
unit microampere{μA,uA} = 1e-6 I

; ============================================
; Temperature Units
; ============================================

unit degreeC{°C,dC} [Θ] = {
  fn display_name() { "°C\x1B" }
  fn to_base(c) { c + 273.15 }
  fn from_base(k) { k - 273.15 }
}

unit degreeF{°F,dF} [Θ] = {
  fn display_name() { "°F" }
  fn to_base(f) { (f - 32) * (5/9) + 273.15 }
  fn from_base(k) { (k - 273.15) * (9/5) + 32 }
}

; ============================================
; Frequency Units (1/time)
; ============================================

base unit hertz{Hz} = 1/T
unit kilohertz{kHz} = 1e3 Hz
unit megahertz{MHz} = 1e6 Hz
unit gigahertz{GHz} = 1e9 Hz

; ============================================
; Electrical Units (with labeled dimensions)
; ============================================

dimension V{voltage} = T^-3 L^2 M A^-1
dimension F{capacitance} = M^-1 L^-2 T^4 A^2
dimension Ω{resistance} = M L^2 T^-3 A^-2
dimension H{inductance} = L^2 M T^-2 A^-2

base unit volt{V} = V
unit millivolt{mV} = 1e-3 V
unit kilovolt{kV} = 1e3 V

base unit farad{F} = F
unit millifarad{mF} = 1e-3 F
unit microfarad{μF,uF} = 1e-6 F
unit nanofarad{nF} = 1e-9 F
unit picofarad{pF} = 1e-12 F

base unit ohm{Ω,R} = Ω
unit milliohm{mΩ,mR} = 1e-3 Ω
unit kiloohm{kΩ,kR} = 1e3 Ω
unit megaohm{MΩ,MR} = 1e6 Ω

base unit henry{H} = H
unit millihenry{mH} = 1e-3 H
unit microhenry{μH,uH} = 1e-6 H
unit nanohenry{nH} = 1e-9 H
unit picohenry{pH} = 1e-12 H

; ============================================
; Angle Units
; ============================================

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

; ============================================
; Data Storage Units
; ============================================

base unit byte{B,byte} = Data
unit kilobyte{KB} [Data] = 1024
unit megabyte{MB} [Data] = 1048576
unit gigabyte{GB} [Data] = 1073741824
unit terabyte{TB} [Data] = 1099511627776
unit kibibyte{KiB} [Data] = 1024
unit mebibyte{MiB} [Data] = 1048576
unit gibibyte{GiB} [Data] = 1073741824
unit tebibyte{TiB} [Data] = 1099511627776

;
; Constants
;

const pi = 3.14159265358979323846
const e = 2.71828182845904523536
const phi = 1.61803398874989484820

;
; General Functions
;

#[builtin]
fn dir(x: any?) { builtin::dir(x) }
#[builtin]
fn len(v: any) { builtin::len(v) }
#[builtin]
fn print(v...) { builtin::print(v...) }
#[builtin]
fn reverse(v: any) { builtin::reverse(v) }
#[builtin]
fn delete(obj: object, key: str) { builtin::delete(obj, key) }
#[builtin]
fn append(list: list, item: any) { builtin::append(list, item) }
#[builtin]
fn open(path: str) { builtin::open(path) }

#[builtin]
fn write(io, v: any) { builtin::write(io, v) }
#[builtin]
fn writeln(io, v: any) { builtin::writeln(io, v) }

#[builtin]
fn json_encode(value) { builtin::json_encode(value) }
#[builtin]
fn json_decode(text) { builtin::json_decode(text) }

; Formatters

fn fmt_plain(v: any, io) {
  write(io, builtin::to_string(v))
}

fn fmt_stdout(v: any, io) {
  write(io, "\x1B[32mRESULT:\x1B[0m ")
  write(io, builtin::to_string(v))
}
#[default_formatter=fmt_stdout]

;
; Math Functions
;

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

;
; Collection Functions
;

fn contains(l: list, v: any) {
  for item := range l {
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
