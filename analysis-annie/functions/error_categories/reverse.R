# Reverse error
# The digits of the correct answer are reversed
# Except correct answers that have a 0 as a last digit or answers that have only
#  1 digit (can't be reversed)

reverse <- function(m, n) {
  correct <- m * n

  if(stringr::str_sub(as.character(correct), -1) == "0" |
     length(strsplit(as.character(correct), "")[[1]]) == 1) {
    values <- numeric()
  } else {
    digits <- strsplit(as.character(correct), "")
    values <- as.numeric(paste(digits[[1]][length(digits[[1]]):1],
                                 collapse = ""))
  }

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
