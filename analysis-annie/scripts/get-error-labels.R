if (!exists("cfg")) {
  source(here::here("config.R"))
}

error_categories_dir <- here::here(cfg$paths$error_categories_dir)
for (file in list.files(error_categories_dir, full.names = TRUE)) {
  source(file)
}

correct_logs <- logs %>% filter(response == "cor")
errors_logs <- logs %>% 
  filter(!response == "cor" & single_digit == 1)

errors_logs$different_unit <- NA
errors_logs$double_half <- NA
errors_logs$m_div_n <- NA
errors_logs$m_minus_n <- NA
errors_logs$m_plus_n <- NA
errors_logs$miss_1 <- NA
errors_logs$miss_10 <- NA
errors_logs$miss_100 <- NA
errors_logs$miss_power <- NA
errors_logs$operand_related <- NA
errors_logs$operator_relevant <- NA
errors_logs$reverse <- NA
errors_logs$typo <- NA
errors_logs$zero <- NA
errors_logs$late <- 1 * (errors_logs$response == "late")
errors_logs$qm <- 1 * (errors_logs$response == "qm")


for(i in 1:nrow(errors_logs)) {
  answer = errors_logs$answer[i]
  errors_logs$different_unit[i] <- 1 * (answer %in% different_unit(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$double_half[i] <- 1 * (answer %in% double_half(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$m_div_n[i] <- 1 * (answer %in% m_div_n(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$m_minus_n[i] <- 1 * (answer %in% m_minus_n(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$m_plus_n[i] <- 1 * (answer %in% m_plus_n(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$miss_1[i] <- 1 * (answer %in% miss_1(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$miss_10[i] <- 1 * (answer %in% miss_10(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$miss_100[i] <- 1 * (answer %in% miss_100(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$miss_power[i] <- 1 * (answer %in% miss_power(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$operand_related[i] <- 1 * (answer %in% operand_related(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$operator_relevant[i] <- 1 * (answer %in% operator_relevant(m = errors_logs$first[i], n = errors_logs$second[i], digits = 0:9))
  errors_logs$reverse[i] <- 1 * (answer %in% reverse(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$typo[i] <- 1 * (answer %in% typo(m = errors_logs$first[i], n = errors_logs$second[i]))
  errors_logs$zero[i] <- 1 * (answer %in% zero(m = errors_logs$first[i], n = errors_logs$second[i]))
}

misconceptions = c("different_unit", "double_half", "m_div_n", "m_minus_n", "m_plus_n", "miss_1", "miss_10", "miss_100", 
                   "miss_power", "operand_related", "operator_relevant", "reverse", "typo", "zero")
errors_logs$misc <- as.integer(rowSums(errors_logs[misconceptions]) > 0)
