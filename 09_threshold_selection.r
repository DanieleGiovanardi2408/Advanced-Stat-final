## R/09_threshold_selection.R

source("R/00_setup_packages.R")

message("--- STEP 09: Selezione Soglie (Thresholds) ---")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
test_fe  <- readRDS(here::here("data", "processed", "test_fe.rds"))
target_var <- "default_payment_next_month"

models <- c("logit_full_platt", "rf_full_platt")

# Funzione calcolo metriche per soglia
calc_confusion <- function(y, p, th) {
  pred <- ifelse(p >= th, 1, 0)
  TP <- sum(pred == 1 & y == 1, na.rm = TRUE)
  FP <- sum(pred == 1 & y == 0, na.rm = TRUE)
  FN <- sum(pred == 0 & y == 1, na.rm = TRUE)
  TN <- sum(pred == 0 & y == 0, na.rm = TRUE)
  
  # Evitiamo divisioni per zero
  sens <- if((TP+FN)>0) TP/(TP+FN) else 0
  spec <- if((TN+FP)>0) TN/(TN+FP) else 0
  
  list(TP=TP, FP=FP, FN=FN, TN=TN, 
       sens = sens, spec = spec, 
       youden = sens + spec - 1,
       cost3 = 3*FN + FP, cost5 = 5*FN + FP)
}

# Griglia di ricerca
thresholds <- seq(0.01, 0.99, by = 0.01)

results_list <- list()
test_evals   <- list()

for (m in models) {
  message(paste("Analisi soglie per:", m))
  
  p_col <- paste0("p_", m)
  y_train <- as.numeric(as.character(train_fe[[target_var]])) # Assicura numerico
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
  y_test <- as.numeric(as.character(test_fe[[target_var]]))
  p_test <- test_fe[[p_col]]
  
  eval_J  <- c(calc_confusion(y_test, p_test, best_J$threshold), criteria="Youden", threshold=best_J$threshold)
  eval_C3 <- c(calc_confusion(y_test, p_test, best_C3$threshold), criteria="Cost_3:1", threshold=best_C3$threshold)
  eval_C5 <- c(calc_confusion(y_test, p_test, best_C5$threshold), criteria="Cost_5:1", threshold=best_C5$threshold)
  
  test_evals[[m]] <- bind_rows(as_tibble(eval_J), as_tibble(eval_C3), as_tibble(eval_C5)) %>% mutate(model = m)
  
  # --- FIX GRAFICO ---
  # Preparazione dati "long" per ggplot (metodo più robusto)
  plot_data <- metrics_grid %>%
    select(threshold, cost3, cost5) %>%
    pivot_longer(cols = c("cost3", "cost5"), names_to = "Cost_Type", values_to = "Total_Cost")
  
  p_costs <- ggplot(plot_data, aes(x = threshold, y = Total_Cost, color = Cost_Type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("cost3" = "blue", "cost5" = "red"), 
                       labels = c("cost3" = "Ratio 3:1", "cost5" = "Ratio 5:1")) +
    labs(title = paste("Minimizzazione Costi -", m), 
         x = "Threshold", y = "Total Cost", color = "Scenario") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Salvataggio con sfondo bianco forzato
  ggsave(here::here("outputs", "figures", paste0("thresh_costs_", m, ".png")), 
         plot = p_costs, width = 6, height = 4, bg = "white")
}

# Export Tabelle
dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(bind_rows(results_list), here::here("outputs", "tables", "threshold_selection_train.csv"))
write_csv(bind_rows(test_evals),   here::here("outputs", "tables", "threshold_performance_test.csv"))

message("Step 09 completato")

# Export
dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(bind_rows(results_list), here::here("outputs", "tables", "threshold_selection_train.csv"))
write_csv(bind_rows(test_evals),   here::here("outputs", "tables", "threshold_performance_test.csv"))

message("Selezione soglie completata.")
