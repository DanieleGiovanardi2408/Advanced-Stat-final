## R/08_eval_calibration.R

source("R/00_setup_packages.R")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
test_fe  <- readRDS(here::here("data", "processed", "test_fe.rds"))
target_var <- "default_payment_next_month"

models <- c("logit_full_platt", "rf_full_platt")

# Funzione Brier Score
brier_score <- function(y, p) mean((p - y)^2)

# Calcolo Metriche Calibrazione
metrics_list <- list()
for (m in models) {
  p_col <- paste0("p_", m)
  
  bs_train <- brier_score(train_fe[[target_var]], train_fe[[p_col]])
  bs_test  <- brier_score(test_fe[[target_var]], test_fe[[p_col]])
  
  metrics_list[[m]] <- tibble::tibble(
    model = m, 
    brier_train = bs_train, 
    brier_test = bs_test
  )
}
calib_df <- bind_rows(metrics_list)

# Calibration Plot (Test Set)
# Creazione bin per plot
get_cal_bins <- function(df, p_col, m_label, bins = 10) {
  df %>%
    mutate(bin = ntile(.data[[p_col]], bins)) %>%
    group_by(bin) %>%
    summarise(
      mean_pred = mean(.data[[p_col]]),
      mean_obs  = mean(.data[[target_var]]),
      n = n()
    ) %>%
    mutate(model = m_label)
}

plot_data <- bind_rows(
  get_cal_bins(test_fe, "p_logit_full_platt", "Logit"),
  get_cal_bins(test_fe, "p_rf_full_platt", "Random Forest")
)

p_cal <- ggplot(plot_data, aes(x = mean_pred, y = mean_obs, color = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  geom_point() + geom_line() +
  labs(title = "Calibration Plot (Test Set)", x = "Predicted Probability", y = "Observed Frequency") +
  xlim(0, 1) + ylim(0, 1)

# Output
write_csv(calib_df, here::here("outputs", "tables", "calibration_metrics.csv"))
ggsave(here::here("outputs", "figures", "calibration_plot.png"), p_cal, width = 6, height = 6)

message("Valutazione calibrazione completata.")