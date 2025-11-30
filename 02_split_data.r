## R/02_split_data.R

source("R/00_setup_packages.R")

input_path <- here::here("data", "interim", "default_clean.rds")
if (!file.exists(input_path)) stop("Input file mancante.")
default_clean <- readRDS(input_path)

target_var <- "default_payment_next_month"

# 1. Split Stratificato
set.seed(1234)
train_prop <- 0.7 

split_obj <- rsample::initial_split(
  default_clean,
  prop   = train_prop,
  strata = !!rlang::sym(target_var)
)

train_data <- rsample::training(split_obj)
test_data  <- rsample::testing(split_obj)

# 2. Calcolo statistiche prevalenza
calc_prev <- function(df, split_name) {
  df %>%
    dplyr::count(.data[[target_var]]) %>%
    dplyr::mutate(prop = n / sum(n), split = split_name)
}

prev_stats <- dplyr::bind_rows(
  calc_prev(train_data, "train"),
  calc_prev(test_data,  "test")
)

# 3. Salvataggio
dir.create(here::here("data", "interim"), recursive = TRUE, showWarnings = FALSE)
saveRDS(train_data, here::here("data", "interim", "train.rds"))
saveRDS(test_data,  here::here("data", "interim", "test.rds"))

dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(prev_stats, here::here("outputs", "tables", "prevalence_splits.csv"))

message("Split Train/Test completato.")
