if (!exists("cfg")) {
  source(here::here("config.R"))
}

split <- cfg$analysis$split

if (!split %in% c("train", "test")) {
  stop("split must be 'train' or 'test'")
}

# load data ----
load(cfg$paths$raw_data_file)

if (split == "train") {
  load(cfg$paths$train_ids_file)
  if (!exists("id_train")) {
    stop("train ids file must load an object named 'id_train'")
  }
  id_split <- id_train
} else {
  if (is.null(cfg$paths$test_ids_file)) {
    stop("cfg$paths$test_ids_file is NULL while split == 'test'")
  }
  load(cfg$paths$test_ids_file)
  if (exists("id_test")) {
    id_split <- id_test
  } else if (exists("id_train")) {
    id_split <- id_train
  } else {
    stop("test ids file must load an object named 'id_test' (or 'id_train')")
  }
}

# join domain and learning goal data
logs <- bind_rows(logs, all_lg_logs)

## save initial n ----
init_n <- data.frame(n_obs = nrow(logs),
                     n_errors = nrow(logs[logs$correct_answered == 0,]),
                     n_items = length(unique(logs$item_id)),
                     n_ind = length(unique(logs$user_id)))
save(init_n, file = here("analysis-annie/tables/init_n.Rdata"))
rm(all_lg_logs, init_n, lgs)
