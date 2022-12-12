large_n_zscore <- function(x, y, s, d, n, m) {
  num <- (x-y)
  denom <- sqrt(((s^2)/n)+((d^2)/m)) 
  z <- num/denom
  print(z)
}

# 10.8 a)

pbinom(4, 15, .1, lower.tail = FALSE) + 
  pbinom(3, 15, .1, lower.tail = F)*dbinom(3, 15, .1) +
  pbinom(4, 15, .1, lower.tail = F)*dbinom(2, 15, .1) +
  pbinom(5, 15, .1, lower.tail = F)*dbinom(1, 15, .1) +
  pbinom(6, 15, .1, lower.tail = F)*dbinom(0, 15, .1)

# 10.17 c)
large_n_zscore(9017, 5853, 7162, 1961, 130, 80)