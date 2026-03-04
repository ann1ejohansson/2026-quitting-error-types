# Different unit error
# Generalization of 'same decade'. Only the unit of the response is incorrect.

different_unit <- function(m, n) {
  correct <- m * n

  if(m == 0 | n == 0) {
    values <- 0 + 1:9
  } else {
    minus_unit <- ceiling((abs(correct + 1))/10) * sign(correct) - 1
    values <- (minus_unit * 10) + 0:9
  }

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
