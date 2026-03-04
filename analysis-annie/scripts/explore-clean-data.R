source(here::here("exploration-annie/00-setup.R"))
source(here::here("exploration-annie/00-data.R"))

# clean item data
items <- items %>% 
  #mutate(question_clean = str_extract(question, "\\b\\d+,?\\d* x \\d+\\b")) %>% 
  mutate(question_clean = str_extract(question, "\\b\\d+,?\\d* x \\b\\d+,?\\d*")) %>%
  mutate(question_clean = gsub(pattern = ",", replacement = ".", x = question_clean, fixed = TRUE)) %>% 
  separate(question_clean, into = c("first", "second"), 
           sep = " x ", convert = TRUE, remove = FALSE) %>%
  mutate(first = as.numeric(first),
         second = as.numeric(second))

items <- as_tibble(items)

# set.seed(1234)
# id_train <- sample(id_train, 1000)

logs <- logs %>% 
  bind_rows(all_lg_logs) %>% 
  # join item info
  left_join(items %>% 
              select(id, mirror_item_id, question_clean, first, second) 
            # %>% 
            #   # select only single-digit items 
            #   filter(first %in% c(0:9) & second %in% c(0:9))
            , 
            by = c("item_id" = "id")) %>% 
  # training set
  filter(user_id %in% id_train) %>% 
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

rm(all_lg_logs)

# make a string item-id variable, unique to each question_clean
id_strings <- logs %>%
  distinct(question_clean, item_id) %>% 
  group_by(question_clean) %>% 
  summarise(id_string = paste(item_id, collapse = "-"), .groups = "drop")

logs <- logs %>%
  left_join(id_strings, by = "question_clean")

# response time variables ----
# fast vs. slow response - log level
logs <- logs %>% 
  group_by(question_clean) %>% 
  mutate(rt_NA = ifelse(response == "late", NA, response_in_milliseconds),
         rt_median = median(rt_NA, na.rm = TRUE)) %>% # remove late responses from median rt calculation
  ungroup() %>% 
  mutate(rt_fastslow = ifelse(rt_NA < rt_median, "fast", "slow"))

# rt variables - grouped by question
rt <- logs %>% 
  filter(response != "late") %>% 
  # overall
  group_by(question_clean) %>% 
  mutate(rt_mean = mean(rt_NA, na.rm = TRUE),
         rt_sd = sd(rt_NA, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # per response type
  group_by(question_clean, response) %>% 
  summarise(n = n(),
            rt_totalmean = unique(rt_mean), 
            rt_totalsd = unique(rt_sd),
            rt_mean = mean(rt_NA, na.rm = TRUE), 
            rt_sd = sd(rt_NA, na.rm = TRUE)) 

logs <- as_tibble(logs)
head(logs)

n_rem_grade <- logs %>% 
  filter(grade < 3) %>% 
  count()

n_rem_items <- 200
rem_items <- logs %>% 
  group_by(question_clean) %>% 
  count() %>% 
  filter(n < n_rem_items) %>% 
  pull(question_clean)

n_rem_user <- 200
rem_users <- logs %>% 
  group_by(user_id) %>% 
  count() %>% 
  filter(n < n_rem_user) %>% 
  pull(user_id)


logs <- logs %>% 
  filter((!question_clean %in% rem_items & !user_id %in% rem_users) | grade > 2)

items <- items %>% 
  filter(!question_clean %in% rem_items)

total_n <- logs %>%
  count(question_clean, name = "total_n")

totals <- logs %>%
  count(question_clean, response) %>%
  left_join(rt, by = c("question_clean", "response")) %>% 
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