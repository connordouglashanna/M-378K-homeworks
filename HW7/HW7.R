large_n_zscore <- function(x, y, s, d, n, m) {
  num <- (x-y)
  denom <- sqrt(((s^2)/n)+((d^2)/m)) 
  z <- num/denom
  print(z)
}

# 10.17 c)
large_n_zscore(9017, 5853, 7162,1961, 130, 80)
