# load data ----
load("~/research-collaboration/data/2025-error-tracing/new_data/set_2025_06_01.Rdata")
load("~/research-collaboration/2025-error-tracing/rdata/ext/id_train.RData") # train ids

# join domain and learning goal data
logs <- bind_rows(logs, all_lg_logs)

## save initial n ----
init_n <- data.frame(n_obs = nrow(logs),
                     n_errors = nrow(logs[logs$correct_answered == 0,]),
                     n_items = length(unique(logs$item_id)),
                     n_ind = length(unique(logs$user_id)))
save(init_n, file = here("analysis-annie/tables/init_n.R"))
rm(all_lg_logs, init_n, items, lgs)
