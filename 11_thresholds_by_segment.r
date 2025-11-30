## R/11_thresholds_by_segment.R

source("R/00_setup_packages.R")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
test_fe  <- readRDS(here::here("data", "processed", "test_fe.rds"))
target_var <- "default_payment_next_month"

models <- c("logit_full_platt", "rf_full_platt")
segments <- sort(unique(train_fe$segment_k4))
thresholds <- seq(0.01, 0.99, by = 0.01)

results_seg <- list()

# Helper function
get_metrics <- function(y, p, th) {
  pred <- ifelse(p >= th, 1, 0)
  TP <- sum(pred==1 & y==1); FN <- sum(pred==0 & y==1); FP <- sum(pred==1 & y==0); TN <- sum(pred==0 & y==0)
  list(J = (TP/(TP+FN)) + (TN/(TN+FP)) - 1, cost3 = 3*FN + FP, cost5 = 5*FN + FP)
}

for (m in models) {
  p_col <- paste0("p_", m)
  
  for (seg in segments) {
    # Train subset
    tr_sub <- train_fe %>% filter(segment_k4 == seg)
    y_tr <- as.numeric(tr_sub[[target_var]])
    p_tr <- tr_sub[[p_col]]
    
    # Grid search su Train
    grid_res <- map_dfr(thresholds, function(th) {
      c(th=th, get_metrics(y_tr, p_tr, th))
    })
    
    best_th_J  <- grid_res$th[which.max(grid_res$J)]
    best_th_C3 <- grid_res$th[which.min(grid_res$cost3)]
    
    # Test Evaluation
    ts_sub <- test_fe %>% filter(segment_k4 == seg)
    y_ts <- as.numeric(ts_sub[[target_var]])
    p_ts <- ts_sub[[p_col]]
    
    # Performance con soglia Youden
    mets_J <- get_metrics(y_ts, p_ts, best_th_J)
    
    results_seg[[paste(m, seg)]] <- tibble(
      model = m, segment = seg, 
      th_Youden = best_th_J, th_Cost3 = best_th_C3,
      test_J_score = mets_J$J, test_cost3 = get_metrics(y_ts, p_ts, best_th_C3)$cost3
    )
  }
}

final_df <- bind_rows(results_seg)
dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(final_df, here::here("outputs", "tables", "segment_threshold_performance.csv"))

message("Analisi soglie per segmento completata.")