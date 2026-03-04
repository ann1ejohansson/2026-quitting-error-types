# Miss 1 error

miss_1 <- function(m, n) {
  correct <- m * n
  values <- c(correct - 1,
              correct + 1)

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
