cfg <- list(
  paths = list(
    raw_data_file = path.expand("~/research-collaboration/data/2025-error-tracing/new_data/set_2026_02_27.Rdata"),
    train_ids_file = path.expand("~/research-collaboration/2025-error-tracing/rdata/ext/id_train.RData"),
    test_ids_file = NULL,
    set_preds_file = path.expand("~/research-collaboration/data/2026-quitting-error-types/set-predictions/results_2026_02-23/rdata/set_preds.RData"),
    check_json_strings_file = path.expand("~/research-collaboration/data/2026-quitting-error-types/data-checks/check-json-strings.RData"),
    error_categories_dir = "analysis-annie/functions/error_categories"
  ),
  analysis = list(
    split = "train",
    min_item_count = 200,
    min_play_count = 200
  ),
  project = list(
    output_dirs = c("models", "figures", "tables")
  )
)
