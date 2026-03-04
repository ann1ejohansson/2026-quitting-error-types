# Miss 10 error

miss_10 <- function(m, n) {
  correct <- m * n
  values <- c(correct - 10,
              correct + 10)

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
