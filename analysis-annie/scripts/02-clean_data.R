# exclusion parameters
if (!exists("cfg")) {
  source(here::here("config.R"))
}

if (!exists("min_item_count")) {
  min_item_count <- cfg$analysis$min_item_count
}

if (!exists("min_play_count")) {
  min_play_count <- cfg$analysis$min_play_count
}

if (!exists("logs")) {
  stop("run script 01-load-data.R first")
}

if (!exists("id_split")) {
  stop("run script 01-load-data.R first (id_split is missing)")
}

# clean item data ----
items <- items %>%
  # clean JSON string
  mutate(question_clean = str_extract(question, "\\b\\d+,?\\d* x \\b\\d+,?\\d*"),
         hint = str_extract(question, "(?<=<br><small>).*?(?=<\\\\/small>)"),
         hint = ifelse(is.na(hint), "none", hint),
         question_hint = paste(question_clean, hint, sep = "\n")) %>%
  # decimal point: , to .
  mutate(question_clean = gsub(pattern = ",", replacement = ".", x = question_clean, fixed = TRUE)) %>%
  # separate factors in the multiplication question
  separate(question_clean, into = c("first", "second"),
           sep = " x ", convert = TRUE, remove = FALSE) %>%
  mutate(first = as.numeric(first),
         second = as.numeric(second)) %>%
  # i noticed that some json strings include spaces, while some don't.
  # marking these here to check for differences later
  mutate(space_after_type = grepl('"type": "OpenAnswer"', question),
         type = ifelse(grepl("OpenAnswer", question), "OpenAnswer", NA))

items <- as_tibble(items)

# mark which items ids connect to which question
items <- items %>%
  group_by(question_clean) %>%
  mutate(id_string = paste(id, collapse = "-"),
         n_unique_item_id = n_distinct(id)) %>%
  ungroup %>%
  group_by(question_hint) %>%
  mutate(id_string_hint = paste(id, collapse = "-"),
         n_unique_item_id_hint = n_distinct(id)) %>%
  ungroup()

# mark items with duplicates (when accounting for hints)
has_duplicates_v <- items %>%
  group_by(question_clean, hint) %>%
  filter(n_unique_item_id_hint > 1) %>%
  separate_rows(id_string_hint, sep = "-") %>%
  mutate(id_string_hint = as.numeric(id_string_hint)) %>%
  pull(id_string_hint)

items <- items %>%
  mutate(has_duplicates = id %in% has_duplicates_v)

# clean-up
rm(has_duplicates_v)

# save duplicate json strings (this does not account for spaces)
duplicate_json <- items %>%
  group_by(question, ids = id_string_hint) %>%
  count()

# some items have an extra space AFTER question
# e.g. "6 x 0" or "6 x 0 "
space_after_q_v <- items %>%
  filter(grepl(' ", "mediaType"', question)) %>%
  pull(id)

items <- items %>%
  mutate(space_after_q = id %in% space_after_q_v)

save(list = c("duplicate_json", "space_after_q_v"),
     file = cfg$paths$check_json_strings_file)
rm(duplicate_json, space_after_q_v)

# clean log data ----
logs <- logs %>%
  # join item info
  left_join(items %>% select(id, question_clean, first, second),
            by = c("item_id" = "id")) %>%
  # training set
  filter(user_id %in% id_split) %>%
  # factor response types
  mutate(response = factor(
    ifelse(answer == "…", "late",
           ifelse(answer == "¿", "qm",
                  ifelse(correct_answered == 0, "error", "cor"))),
    levels = c("cor", "error", "qm", "late"))) %>%
  # change commas for decimal point in answer
  mutate(comma = 1 * (answer == "0,"), # if student only clicked the ',' button then proceeded
         answer = gsub(",", ".", answer),
         answer = ifelse(comma == 1, ",0", answer),
         answer_id = paste(question_clean, answer, sep = " = "),
         single_digit = (first %in% c(0:9) & second %in% c(0:9)))

# make a string item-id variable, unique to each question_clean
id_strings <- logs %>%
  distinct(question_clean, item_id) %>%
  group_by(question_clean) %>%
  summarise(id_string = paste(item_id, collapse = "-"), .groups = "drop")

logs <- logs %>%
  left_join(id_strings, by = "question_clean")
rm(id_strings)

# data exclusion ----
n_rem_grade <- logs %>%
  filter(grade < 3) %>%
  count()

rem_items <- logs %>%
  group_by(question_clean) %>%
  count() %>%
  filter(n < min_item_count) %>%
  pull(question_clean)


rem_users <- logs %>%
  group_by(user_id) %>%
  count() %>%
  filter(n < min_play_count) %>%
  pull(user_id)

logs <- logs %>%
  filter((!question_clean %in% rem_items & !user_id %in% rem_users) | grade > 2)

items <- items %>%
  filter(!question_clean %in% rem_items)

cat(n_rem_grade$n, "observations from learners in grades 1 and 2 were removed. \n")
cat(length(rem_items), "questions were removed. \n")
cat(length(rem_users), "individuals were removed. \n", "\n")

final_n <- data.frame(n_obs = nrow(logs),
                      n_errors = nrow(logs %>% filter(response == "error")),
                      n_items = length(unique(logs$question_clean)),
                      n_ind = length(unique(logs$user_id)))
save(final_n, file = here("analysis-annie/tables/final_n.R"))

cat("n. observations = ", nrow(logs), "\n")
cat("n. errors = ", nrow(logs %>% filter(response == "error")), "\n")
cat("n. individuals = ", length(unique(logs$user_id)), "\n")
cat("n. items = ", length(unique(logs$question_clean)))

rm(rem_items, rem_users, n_rem_grade)

# create exploration dataframes -----
# rt variables - grouped by question
logs <- logs %>%
  group_by(question_clean) %>%
  mutate(rt_NA = ifelse(response == "late", NA, response_in_milliseconds),
         rt_median = median(rt_NA, na.rm = TRUE)) %>% # remove late responses from median rt calculation
  ungroup() %>%
  mutate(rt_fastslow = ifelse(rt_NA < rt_median, "fast", "slow"))

rt <- logs %>%
  # overall
  group_by(question_clean, single_digit) %>%
  mutate(rt_mean = mean(rt_NA, na.rm = TRUE),
         rt_sd = sd(rt_NA, na.rm = TRUE)) %>%
  ungroup() %>%
  # per response type
  group_by(question_clean, single_digit, response) %>%
  summarise(n = n(),
            rt_totalmean = unique(rt_mean),
            rt_totalsd = unique(rt_sd),
            rt_mean = mean(rt_NA, na.rm = TRUE),
            rt_sd = sd(rt_NA, na.rm = TRUE))

total_n <- logs %>%
  count(question_clean, name = "total_n")

totals <- logs %>%
  count(question_clean, response) %>%
  left_join(rt %>% select(-n), by = c("question_clean", "response")) %>%
  pivot_wider(
    names_from = response,
    values_from = c(n, rt_mean, rt_sd),
    names_glue = "{.value}_{response}"
  ) %>%
  mutate(error_rate = n_error / (n_cor + n_error)) %>%
  left_join(total_n, by = "question_clean") %>%
  select(question_clean, total_n, n_cor, n_error, n_qm, n_late, error_rate,
                rt_mean_cor, rt_mean_error, rt_mean_qm, rt_sd_cor, rt_sd_error, rt_sd_qm,
                rt_totalmean, rt_totalsd)


totals <- totals %>%
  separate(question_clean, into = c("first", "second"),
           sep = " x ", convert = TRUE, remove = FALSE) %>%
  mutate(first  = if_else(first %% 1 == 0, round(first, 1), first),
         second = if_else(second %% 1 == 0, round(second, 1), second),
         single_digit = (first %in% c(0:9) & second %in% c(0:9)))

# all responses per item
resp <- logs %>%
  group_by(answer_id, question_clean,  answer, first, second, response, id_string) %>%
  summarise(n = n(),
            rt_mean_resp = mean(rt_NA, na.rm = T),
            rt_sd_resp = sd(rt_NA, na.rm = T),
            n_fast = sum(rt_fastslow == "fast"),
            n_slow = sum(rt_fastslow == "slow"))

resp_fastslow <- logs %>%
  group_by(answer_id, question_clean,  answer, first, second, response, id_string, rt_fastslow) %>%
  summarise(n = n(),
            rt_mean_resp = mean(rt_NA, na.rm = T),
            rt_sd_resp = sd(rt_NA, na.rm = T))
# resp <- logs %>%
#   group_by(question_clean, answer, first, second, answer_id, response, id_string) %>%
#   count(name = "n") %>%
#   ungroup() %>%
#   mutate(first = as.numeric(first),
#          second = as.numeric(second)) %>%
#   select(answer_id, id_string, response, question_clean, answer, first, second, n)

# # add totals to resp table
resp <- resp %>%
  left_join(totals,
            by = c("question_clean", "first", "second")) %>%
  mutate(filter_error = 1*(response == "error"))

resp_fastslow <- resp_fastslow %>%
  left_join(totals,
            by = c("question_clean", "first", "second"))

resp <- resp %>%
  mutate(filter_error = (response == "error"))

resp_fastslow <- resp_fastslow %>%
  mutate(filter_error = (response == "error"))

rm(total_n, final_n, min_item_count, min_play_count)
