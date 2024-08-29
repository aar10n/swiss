fn min(vals...) {
  builtin::print("min")
  for v := vals {
    builtin::debug(v)
  }
}

min(1, 2, 3)
min()
