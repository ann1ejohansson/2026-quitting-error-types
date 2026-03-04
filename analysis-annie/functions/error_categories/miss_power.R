# Miss power error
# Response is the correct answer multiplied or divided by a power of ten

miss_power <- function(m, n) {
  correct <- m * n
  values <- c(correct * 10^seq(1, 5, by = 1),
              correct / 10^seq(1, 5, by = 1))

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
