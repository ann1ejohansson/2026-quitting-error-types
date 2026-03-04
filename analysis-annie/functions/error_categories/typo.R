# Typo error
# Ommission or repetition of a digit

typo <- function(m, n) {
  correct <- m * n

  digits <- unlist(strsplit(as.character(correct), split = ""))

  if(length(digits) >= 2) {
    omm_tmp <- sapply(seq_along(digits), function(x) digits[-x])
    ifelse(is.vector(omm_tmp),  # when correct answer exists of 2 digits
           ommission <- omm_tmp,
           ommission <- apply(omm_tmp, 2, function(x) paste(x, collapse = "")))
  } else {
    ommission <- numeric()
  }

  repetition <- numeric()

  for(i in 1:length(digits)) {
    repetition[i] <- paste0(substr(as.character(correct), 0, i-1),
                            digits[i],
                            substr(as.character(correct), i, length(digits)))
  }

  values <- as.numeric(c(ommission, repetition))

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
