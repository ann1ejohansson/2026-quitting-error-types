# Operator relevant error
# Answer to a single-digit multiplication problem
#  Args
#   digits: digits to take into account, e.g., 0:9 or 1:9

operator_relevant <- function(m, n, digits) {
  correct <- m * n
  values <- unique(as.vector(digits%*%t(digits)))

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}
