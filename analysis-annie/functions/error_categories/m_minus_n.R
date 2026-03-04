# Subtraction error
# m - n error, NOT n - m error

m_minus_n <- function(m, n) {
  correct <- m * n
  values <- m - n

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
