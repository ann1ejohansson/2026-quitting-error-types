# Zero error

zero <- function(m, n) {
  correct <- m * n
  values <- 0

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
