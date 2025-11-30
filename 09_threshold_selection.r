## R/09_threshold_selection.R

source("R/00_setup_packages.R")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
test_fe  <- readRDS(here::here("data", "processed", "test_fe.rds"))
target_var <- "default_payment_next_month"

models <- c("logit_full_platt", "rf_full_platt")

# Funzione calcolo metriche per soglia
calc_confusion <- function(y, p, th) {
  pred <- ifelse(p >= th, 1, 0)
  TP <- sum(pred == 1 & y == 1)
  FP <- sum(pred == 1 & y == 0)
  FN <- sum(pred == 0 & y == 1)
  TN <- sum(pred == 0 & y == 0)
  
  list(TP=TP, FP=FP, FN=FN, TN=TN, 
       sens = TP/(TP+FN), spec = TN/(TN+FP), 
       youden = (TP/(TP+FN)) + (TN/(TN+FP)) - 1,
       cost3 = 3*FN + FP, cost5 = 5*FN + FP)
}

# Griglia di ricerca
thresholds <- seq(0.01, 0.99, by = 0.01)

results_list <- list()
test_evals   <- list()

for (m in models) {
  p_col <- paste0("p_", m)
  y_train <- as.numeric(train_fe[[target_var]])
  p_train <- train_fe[[p_col]]
  
  # 1. Ottimizzazione su Train
  metrics_grid <- purrr::map_dfr(thresholds, function(th) {
    c(threshold = th, calc_confusion(y_train, p_train, th))
  })
  
  best_J  <- metrics_grid[which.max(metrics_grid$youden), ]
  best_C3 <- metrics_grid[which.min(metrics_grid$cost3), ]
  best_C5 <- metrics_grid[which.min(metrics_grid$cost5), ]
  
  results_list[[m]] <- bind_rows(
    mutate(best_J,  criteria = "Youden", model = m),
    mutate(best_C3, criteria = "Cost_3:1", model = m),
    mutate(best_C5, criteria = "Cost_5:1", model = m)
  )
  
  # 2. Valutazione su Test con soglie ottime
  y_test <- as.numeric(test_fe[[target_var]])
  p_test <- test_fe[[p_col]]
  
  eval_J  <- c(calc_confusion(y_test, p_test, best_J$threshold), criteria="Youden", threshold=best_J$threshold)
  eval_C3 <- c(calc_confusion(y_test, p_test, best_C3$threshold), criteria="Cost_3:1", threshold=best_C3$threshold)
  eval_C5 <- c(calc_confusion(y_test, p_test, best_C5$threshold), criteria="Cost_5:1", threshold=best_C5$threshold)
  
  test_evals[[m]] <- bind_rows(as_tibble(eval_J), as_tibble(eval_C3), as_tibble(eval_C5)) %>% mutate(model = m)
  
  # Plot Costi vs Soglia
  p_costs <- ggplot(metrics_grid, aes(x=threshold)) +
    geom_line(aes(y=cost3, color="Cost 3:1")) +
    geom_line(aes(y=cost5, color="Cost 5:1")) +
    labs(title = paste("Cost Minimization -", m), y = "Total Cost") +
    theme_minimal()
  
  ggsave(here::here("outputs", "figures", paste0("thresh_costs_", m, ".png")), p_costs, width = 6, height = 4)
}

# Export
dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(bind_rows(results_list), here::here("outputs", "tables", "threshold_selection_train.csv"))
write_csv(bind_rows(test_evals),   here::here("outputs", "tables", "threshold_performance_test.csv"))

message("Selezione soglie completata.")
