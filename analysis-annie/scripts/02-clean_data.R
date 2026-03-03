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

items <- items %>%
  #mutate(question_clean = str_extract(question, "\\b\\d+,?\\d* x \\d+\\b")) %>%
  mutate(question_clean = str_extract(question, "\\b\\d+,?\\d* x \\b\\d+,?\\d*")) %>%
  mutate(question_clean = gsub(pattern = ",", replacement = ".", x = question_clean, fixed = TRUE)) %>%
  separate(question_clean, into = c("first", "second"),
           sep = " x ", convert = TRUE, remove = FALSE) %>%
  mutate(first = as.numeric(first),
         second = as.numeric(second))

items <- as_tibble(items)

# clean log data ----
logs <- logs %>%
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

# make a string item-id variable, unique to each question_clean
id_strings <- logs %>%
  distinct(question_clean, item_id) %>%
  group_by(question_clean) %>%
  summarise(id_string = paste(item_id, collapse = "-"), .groups = "drop")

logs <- logs %>%
  left_join(id_strings, by = "question_clean")

# data exclusion ----
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

cat(n_rem_grade$n, "observations from learners in grades 1 and 2 were removed. \n")
cat(length(rem_items), "questions were removed. \n")
cat(length(rem_users), "individuals were removed. \n", "\n")

final_n <- data.frame(n_obs = nrow(logs),
                      n_errors = nrow(logs %>% filter(response == "error")),
                      n_items = length(unique(logs$question_clean)),
                      n_ind = length(unique(logs$user_id)))
save(final_n, file = here("analysis-annie/tables/final_n.R"))
