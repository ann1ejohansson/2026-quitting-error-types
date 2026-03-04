# Operand related error
# Answer to a problem with one matching operand, and one operand that is 1 or 2
#  units smaller/larger (except when it becomes zero of negative)

operand_related <- function(m, n) {
  correct <- m * n

  operands <- dplyr::tibble(
    n = (n - c(1, 2, -1, -2, rep(0, 4))),
    m = (m - c(rep(0, 4), 1, 2, -1, -2))) %>%
    dplyr::filter(n > 0 & m > 0)

  values <- unique(operands$n * operands$m)

  if(any(values == correct)) values <- values[-which(values == correct)]
  if(length(values) < 1) values <- NA
  return(values)
}

# check out when both operands may vary:
# as.vector(c((m-2):(m+2)) %*% t((n-2):(n+2)))[-13]
