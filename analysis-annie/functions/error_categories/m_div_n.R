# Division error
# m / n error, NOT n / m error

m_div_n <- function(m, n) {
  correct <- m * n
  if(n == 0) {
    values <- numeric()
  } else {
    values <- m / n
  }

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
