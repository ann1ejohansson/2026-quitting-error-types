# Trace set predictions back to row-level source observations

# setup and load data
source("~/research-collaboration/2026-quitting-error-types/analysis-annie/scripts/00-setup_environment.R")
# load in raw data
# load("~/research-collaboration/data/2025-error-tracing/new_data/set_2025_06_01.Rdata")
# load("~/research-collaboration/data/2025-error-tracing/results_2026-02-23/rdata/id_train.RData")
# logs <- bind_rows(logs, all_lg_logs)
# logs <- logs %>%
#   filter(user_id %in% id_train)


# # bi_unid is a large matrix with dim n_causes * n_errors in the full dataset.
# # colnames are row id’s for each response recorded.
# load("~/research-collaboration/data/2025-error-tracing/results_2026-02-23/rdata/bi_unid.RData")
#
# # ind_unid is a vector with the indices of all instances where an error was mis- or unidentified.
# load("~/research-collaboration/data/2025-error-tracing/results_2026-02-23/rdata/ind_unid.RData")
#
# load in data cleaned for set
load("~/research-collaboration/data/2025-error-tracing/results_2026-02-23/rdata/db_slc_err.RData")

# load in set predictions
#load("~/research-collaboration/data/2025-error-tracing/results_2026-02-24/rdata/set_preds.RData")
load("~/research-collaboration/data/2025-error-tracing/results_2026-02-24/predictions/20260224_112320_train_predictions.Rdata")

# prediction settings
fltr_window_size <- 30
fltr_model <- "set"
fltr_weights <- "cause*error"
fltr_beta <- 1

prdctns_filtered <- prdctns %>%
  filter(
    window_size == fltr_window_size,
    model == fltr_model,
    weights == fltr_weights,
    beta == fltr_beta
  ) %>%
  mutate(answer_id = paste(target_question_clean, target_answer, sep = " = "))

errors <- db_slc_err %>%
  mutate(error_id = as.character(error_id)) %>%
  left_join(prdctns_filtered, by = c("user_id", "created" = "target_created", "answer_id", "item_id" = "target_item_id", "error_id"))

# all unidentified
unid <- errors %>%
  filter(is.na(model))

# all errors predicted by set
set_preds <- errors %>%
  filter(model == "set")


# load in error label functions
devtools::load_all(here("analysis-annie/functions/error_categories"))

unid$different_unit <- NA
unid$double_half <- NA
unid$m_div_n <- NA
unid$m_minus_n <- NA
unid$m_plus_n <- NA
unid$miss_1 <- NA
unid$miss_10 <- NA
unid$miss_100 <- NA
unid$miss_power <- NA
unid$operand_related <- NA
unid$operator_relevant <- NA
unid$reverse <- NA
unid$typo <- NA
unid$zero <- NA


for(i in 1:nrow(unid)) {
  answer = unid$answer[i]
  unid$different_unit[i] <- 1 * (answer %in% different_unit(m = unid$first[i], n = unid$second[i]))
  unid$double_half[i] <- 1 * (answer %in% double_half(m = unid$first[i], n = unid$second[i]))
  unid$m_div_n[i] <- 1 * (answer %in% m_div_n(m = unid$first[i], n = unid$second[i]))
  unid$m_minus_n[i] <- 1 * (answer %in% m_minus_n(m = unid$first[i], n = unid$second[i]))
  unid$m_plus_n[i] <- 1 * (answer %in% m_plus_n(m = unid$first[i], n = unid$second[i]))
  unid$miss_1[i] <- 1 * (answer %in% miss_1(m = unid$first[i], n = unid$second[i]))
  unid$miss_10[i] <- 1 * (answer %in% miss_10(m = unid$first[i], n = unid$second[i]))
  unid$miss_100[i] <- 1 * (answer %in% miss_100(m = unid$first[i], n = unid$second[i]))
  unid$miss_power[i] <- 1 * (answer %in% miss_power(m = unid$first[i], n = unid$second[i]))
  unid$operand_related[i] <- 1 * (answer %in% operand_related(m = unid$first[i], n = unid$second[i]))
  #unid$operator_relevant[i] <- 1 * (answer %in% operator_relevant(m = unid$first[i], n = unid$second[i], digits = 0:9))
  unid$reverse[i] <- 1 * (answer %in% reverse(m = unid$first[i], n = unid$second[i]))
  unid$typo[i] <- 1 * (answer %in% typo(m = unid$first[i], n = unid$second[i]))
  unid$zero[i] <- 1 * (answer %in% zero(m = unid$first[i], n = unid$second[i]))
}

misconceptions = c("different_unit", "double_half", "m_div_n", "m_minus_n", "m_plus_n", "miss_1", "miss_10", "miss_100",
                   "miss_power", "operand_related", "operator_relevant",
                   "reverse", "typo", "zero")
unid$theoretical_cause <- 1 * ifelse(rowSums(unid[misconceptions]) > 0)

unid %>%
  filter(predicted_cause != "operator_relevant") %>%
  ggplot2::ggplot(ggplot2::aes(x = factor(misc), y = response_in_milliseconds)) +
  ggplot2::geom_violin(fill = "grey85", color = "black", alpha = 0.7, trim = FALSE) +
  ggplot2::geom_boxplot(width = 0.18, outlier.alpha = 0.2) +
  ggplot2::labs(
    title = "Response Time by Error Type",
    subtitle = "simple categorizaion of errors according to error labels",
    x = NULL,
    y = "Response time (ms)"
  ) +
  ggplot2::theme_bw()




#
#
#
# source(file.path(getwd(), "scripts", "_bootstrap_analysis_env.R"))
# bootstrap_analysis_env(
#   required_objects = c("db_slc_err", "prdctns"),
#   object_sources = list(
#     db_slc_err = c("__AUTO_RDATA__"),
#     prdctns = c("__LATEST_PREDICTIONS__")
#   )
# )
#
# if (!exists("db_slc_err")) {
#   stop("db_slc_err is not loaded. Run scripts/01ext_preprocess_data.R first or load db_slc_err.RData.")
# }
#
# if (!exists("prdctns")) {
#   stop("prdctns is not loaded. Load a predictions .Rdata file first.")
# }
#
# # prediction settings
# fltr_window_size <- 30
# fltr_model <- "set"
# fltr_weights <- "cause*error"
# fltr_beta <- 1
#
# prdctns_filtered <- prdctns %>%
#   dplyr::filter(
#     window_size == fltr_window_size,
#     model == fltr_model,
#     weights == fltr_weights,
#     beta == fltr_beta
#   ) %>%
#   dplyr::mutate(
#     predicted_cause = purrr::map_chr(
#       prediction_vec,
#       ~ {
#         if (length(.x) == 0) return(NA_character_)
#         nms <- names(.x)
#         if (is.null(nms) || length(nms) == 0) return(NA_character_)
#         nms[[1]]
#       }
#     ),
#     predicted_cause_prob = purrr::map_dbl(
#       prediction_vec,
#       ~ {
#         if (length(.x) == 0) return(NA_real_)
#         as.numeric(.x[[1]])
#       }
#     ),
#     cause_vec_chr = purrr::map(
#       cause_vec,
#       ~ as.character(.x)
#     ),
#     cause_vec_n = purrr::map_int(
#       cause_vec_chr,
#       ~ length(.x)
#     ),
#     cause_vec_first = purrr::map_chr(
#       cause_vec_chr,
#       ~ {
#         if (length(.x) == 0) return(NA_character_)
#         as.character(.x[[1]])
#       }
#     ),
#     cause_vec_label = purrr::map_chr(
#       cause_vec_chr,
#       ~ paste(.x, collapse = "|")
#     ),
#     predicted_cause_second = purrr::map_chr(
#       prediction_vec,
#       ~ {
#         if (length(.x) == 0) return(NA_character_)
#         nms <- names(.x)
#         if (is.null(nms) || length(nms) == 0) return(NA_character_)
#         if (length(nms) >= 2) return(nms[[2]])
#         NA_character_
#       }
#     ),
#     # Plausibility-aware filtering using cause_vec:
#     # if cause_vec has >1 and starts with operator_relevant, select the highest-ranked
#     # plausible non-operator cause from prediction_vec.
#     # otherwise keep top-ranked cause when plausible, else fallback to highest-ranked plausible cause.
#     predicted_cause_filtered = purrr::map2_chr(
#       cause_vec_chr,
#       prediction_vec,
#       ~ {
#         plausible <- .x
#         pv <- .y
#         if (length(pv) == 0) return(NA_character_)
#         ranked <- names(pv)
#         if (is.null(ranked) || length(ranked) == 0) return(NA_character_)
#         top <- ranked[[1]]
#
#         if (length(plausible) > 1 && plausible[[1]] == "operator_relevant") {
#           ranked_plausible_non_op <- ranked[ranked %in% plausible & ranked != "operator_relevant"]
#           if (length(ranked_plausible_non_op) >= 1) return(ranked_plausible_non_op[[1]])
#         }
#
#         if (top %in% plausible) return(top)
#         ranked_plausible <- ranked[ranked %in% plausible]
#         if (length(ranked_plausible) >= 1) return(ranked_plausible[[1]])
#         top
#       }
#     ),
#     filtered_differs_from_top = predicted_cause_filtered != predicted_cause
#   ) %>%
#   dplyr::select(
#     user_id, nth_error, predicted_cause, predicted_cause_prob, predicted_cause_second,
#     cause_vec_n, cause_vec_first, cause_vec_label, predicted_cause_filtered, filtered_differs_from_top,
#     dplyr::any_of(c("target_created", "target_response_in_milliseconds", "target_question_clean", "target_answer", "target_item_id", "target_item", "target_first", "target_second"))
#   ) %>%
#   dplyr::distinct() %>%
#   mutate(answer_id = paste(target_question_clean, target_answer, sep = " = ")) %>%
#   select(-target_response_in_milliseconds, -target_question_clean, -target_answer, -target_item, -target_first, -target_second)
#
# set_preds <- db_slc_err %>%
#   mutate(error_id = as.character(error_id)) %>%
#   left_join(prdctns_filtered, by = c("user_id", "created" = "target_created", "answer_id", "item_id" = "target_item_id"))
#
# set_preds <- set_preds %>%
#   mutate(cause_vec_n = ifelse(is.na(cause_vec_n), 0, cause_vec_n))
#
# set_preds <- set_preds %>%
#   mutate(error_type = ifelse(cause_vec_n == 0, "unidentified", "modeled"))
#
# # -----------------------------------------------------------------------------
# # Summary + simple tests
# # -----------------------------------------------------------------------------
# rt_summary <- set_preds %>%
#   dplyr::group_by(error_type) %>%
#   dplyr::summarise(
#     n = dplyr::n(),
#     mean_rt = mean(response_in_milliseconds, na.rm = TRUE),
#     median_rt = median(response_in_milliseconds, na.rm = TRUE),
#     sd_rt = sd(response_in_milliseconds, na.rm = TRUE),
#     q25_rt = quantile(response_in_milliseconds, probs = 0.25, na.rm = TRUE),
#     q75_rt = quantile(response_in_milliseconds, probs = 0.75, na.rm = TRUE),
#     .groups = "drop"
#   )
#
# wilcox_out <- stats::wilcox.test(
#   response_in_milliseconds ~ error_type,
#   data = set_preds,
#   exact = FALSE
# )
#
# ttest_out <- stats::t.test(
#   response_in_milliseconds ~ error_type,
#   data = set_preds
# )
#
# print(rt_summary)
# print(wilcox_out)
# print(ttest_out)
#
# rt_cause_summary <- set_preds %>%
#   dplyr::filter(error_type == "modeled", !is.na(predicted_cause)) %>%
#   dplyr::group_by(predicted_cause_filtered) %>%
#   dplyr::summarise(
#     n = dplyr::n(),
#     mean_rt = mean(response_in_milliseconds, na.rm = TRUE),
#     median_rt = median(response_in_milliseconds, na.rm = TRUE),
#     sd_rt = sd(response_in_milliseconds, na.rm = TRUE),
#     q25_rt = quantile(response_in_milliseconds, probs = 0.25, na.rm = TRUE),
#     q75_rt = quantile(response_in_milliseconds, probs = 0.75, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   dplyr::rename(predicted_cause = predicted_cause_filtered) %>%
#   dplyr::arrange(dplyr::desc(n))
# print(rt_cause_summary)
#
#
# # Plots
# fig_rt <- set_preds %>%
#   ggplot(aes(x = error_type, y = response_in_milliseconds)) +
#   geom_violin(fill = "grey85", color = "black", alpha = 0.7, trim = FALSE) +
#   geom_boxplot(width = 0.18, outlier.alpha = 0.2) +
#   labs(
#     title = "Response Time by Error Type",
#     subtitle = "modeled = SET-predicted; unidentified = not predicted (unidentified/misidentified)",
#     x = NULL,
#     y = "Response time (ms)"
#   ) +
#   theme_bw()
#
# set_preds %>%
#   ggplot() +
#   geom_histogram(aes(x = response_in_milliseconds, fill = error_type),
#                  position = "identity", alpha = 0.5) +
#   scale_fill_viridis_d() +
#   theme_bw()
#
# fig_rt_cause <- set_preds %>%
#   filter(error_type == "modeled") %>%
#   ggplot(aes(x = predicted_cause_filtered, y = response_in_milliseconds)) +
#   geom_violin(fill = "grey85", color = "black",
#                        alpha = 0.7, trim = FALSE) +
#   geom_boxplot(width = 0.18, outlier.alpha = 0.2) +
#   labs(
#     title = "Response Time by Predicted Cause (Filtered)",
#     subtitle = "Filtered rule uses 2nd prediction when cause_vec starts with operator_relevant and has >1 cause",
#     x = "Predicted cause (filtered)",
#     y = "Response time (ms)"
#   ) +
#   theme_bw()
#
#
#
#
#
# plot(y = set_preds$response_in_milliseconds, x = set_preds$predicted_cause_prob)
# ggplot(set_preds, aes(x = predicted_cause_prob, y = response_in_milliseconds)) +
#   geom_point(alpha = 0.1) +
#   geom_smooth(method = "lm") +
#   theme_bw() +
#   labs(
#     title = "Response Time vs. Predicted Cause Probability",
#     x = "Predicted cause probability",
#     y = "Response time (ms)"
#   )
#
