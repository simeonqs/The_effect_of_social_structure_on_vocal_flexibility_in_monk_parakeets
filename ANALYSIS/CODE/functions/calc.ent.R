calc.ent = function(v = c(1, 1, 1, 3, 4, 4)){
  t = as.numeric(table(v))
  p = t/sum(t)
  return(-sum(p * log(p)))
}