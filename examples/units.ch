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
dimension I // current
dimension Θ // temperature
dimension N // amount of substance
dimension J // luminous intensity

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

base unit ampere{A} = I
unit milliampere{mA} [I] = 1e-3
unit microampere{μA,uA} [I] = 1e-6

base unit kelvin{K} = Θ
unit degreeC{dC} [Θ] = 1
unit degreeF{dF} [Θ] = 0.55555556 // 5/9

base unit hertz{Hz} = 1/T
unit kilohertz{kHz} [1/T] = 1e3
unit megahertz{MHz} [1/T] = 1e6
unit gigahertz{GHz} [1/T] = 1e9

dimension V = T^-3 L^2 M I^-1
base unit volt{V} = V
unit millivolt{mV} [V] = 1e-3
unit kilovolt{kV} [V] = 1e3

dimension F = M^-1 L^-2 T^4 I^2
base unit farad{F} = F
unit millifarad{mF} [F] = 1e-3
unit microfarad{μF,uF} [F] = 1e-6
unit nanofarad{nF} [F] = 1e-9
unit picofarad{pF} [F] = 1e-12

dimension Ω = M L^2 T^-3 I^-2
base unit ohm{Ω,R} = Ω
unit milliohm{mΩ,mR} [Ω] = 1e-3
unit kiloohm{kΩ,kR} [Ω] = 1e3
unit megaohm{MΩ,MR} [Ω] = 1e6

dimension H = L^2 M T^-2 I^-2
base unit henry{H} = H
unit millihenry{mH} [H] = 1e-3
unit microhenry{μH,uH} [H] = 1e-6
unit nanohenry{nH} [H] = 1e-9
unit picohenry{pH} [H] = 1e-12

/*
base unit bit{b} = 1
unit kilobit{kb} [b] = 1000
unit megabit{Mb} [b] = 1000000
unit gigabit{Gb} [b] = 1000000000
unit terabit{Tb} [b] = 1000000000000

// IEC
base unit byte{B} = 8
unit kibibyte{K,KiB} [B] = 1024 // 1 << 10
unit mebibyte{M,MiB} [B] = 1048576 // 1 << 20
unit gibibyte{G,GiB} [B] = 1073741824 // 1 << 30
unit tebibyte{T,TiB} [B] = 1099511627776 // 1 << 40
unit pebibyte{P,PiB} [B] = 1125899906842624 // 1 << 50

// SI
unit kilobyte{kB} [B] = 1e3
unit megabyte{MB} [B] = 1e6
unit gigabyte{GB} [B] = 1e9
unit terabyte{TB} [B] = 1e12
unit petabyte{PB} [B] = 1e15
*/
