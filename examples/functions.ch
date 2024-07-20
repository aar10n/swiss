// types:
//   any   - any type
//   int   - arbitrary precision integer
//   float - arbitrary precision floating point
//   num   - a numeric type (int or float)
//
// 
// typed parameters:
//   <param>:<type>
//
// dimension parameters:
//   <param>[<dim>]
//

fn min(a, b) { if a < b { a } else { b } }
fn max(a, b) { if a > b { a } else { b } }

fn bit_and(a:int, b:int) {
  builtin::bit_and(a, b)
}


fn ohms_law(V:[V], I:[A]) { V / I }
fn ohms_law(V[V], R[Ω]) [I] { V / R }
fn ohms_law(I[A], R[Ω]) [V] { I * R }

fn ohms_law(V[V], I[A]) [R] { V / I }
fn ohms_law(V[V], R[Ω]) [I] { V / R }
fn ohms_law(I[A], R[Ω]) [V] { I * R }

fn voltage_divider(Vs[V], R1[Ω], R2[Ω]) { Vs * R2 / (R1 + R2) }
