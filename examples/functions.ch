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
//   <param>:[<dim>]
//

fn min(a, b) { if a < b { a } else { b } }
fn max(a, b) { if a > b { a } else { b } }

fn min(vals...) {
  r = 0
  for v in vals {
    r += v
  }
  r
}

fn min(vals...) {
  m = Inf
  for v in vals {
    if v < m {
      m = v
    }
  }
  m
}

fn min(vals...) {
  reduce(vals, Inf, |v, acc| if v < acc { v } else { acc })
}

fn reduce(vals, init, f) {
  acc = init
  for v in vals {
    acc = f(v, acc)
  }
  acc
}

fn map(vals, f) {
  
}

max(1, 2)
