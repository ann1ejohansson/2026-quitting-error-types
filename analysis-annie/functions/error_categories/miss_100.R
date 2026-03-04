# Miss 100 error

miss_100 <- function(m, n) {
  correct <- m * n
  values <- c(correct - 100,
              correct + 100)

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
