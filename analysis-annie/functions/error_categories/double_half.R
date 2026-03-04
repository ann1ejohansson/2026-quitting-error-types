# Double half error
# Double or half the correct response

double_half <- function(m, n) {
  correct <- m * n
  values <- c(correct / 2,
              correct * 2)

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
