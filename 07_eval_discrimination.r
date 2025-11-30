## R/07_eval_discrimination.R

source("R/00_setup_packages.R")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
test_fe  <- readRDS(here::here("data", "processed", "test_fe.rds"))
target_var <- "default_payment_next_month"

models <- c("logit_full_platt", "rf_full_platt")

# Funzione calcolo metriche (ROC, Gini, Accuracy)
calc_metrics <- function(df, p_col, label) {
  y <- as.numeric(df[[target_var]])
  p <- df[[p_col]]
  
  roc_obj <- pROC::roc(y, p, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  gini    <- 2 * auc_val - 1
  
  # Cutoff 0.5 stats
  pred_class <- ifelse(p >= 0.5, 1, 0)
  acc <- mean(pred_class == y)
  
  tibble::tibble(
    model = label, dataset = ifelse(nrow(df) == nrow(train_fe), "train", "test"),
    AUC = auc_val, Gini = gini, Accuracy = acc
  )
}

res_list <- list()
for (m in models) {
  p_col <- paste0("p_", m)
  res_list[[paste0(m, "_train")]] <- calc_metrics(train_fe, p_col, m)
  res_list[[paste0(m, "_test")]]  <- calc_metrics(test_fe, p_col, m)
}

metrics_df <- bind_rows(res_list)

# Plot ROC Comparativa (Test Set)
roc_logit <- pROC::roc(test_fe[[target_var]], test_fe$p_logit_full_platt, quiet = TRUE)
roc_rf    <- pROC::roc(test_fe[[target_var]], test_fe$p_rf_full_platt, quiet = TRUE)

p_roc <- ggroc(list(Logit = roc_logit, RF = roc_rf), size = 1) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey") +
  labs(title = "ROC Curves - Test Set", color = "Model") +
  theme_minimal()

# Output
dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)

write_csv(metrics_df, here::here("outputs", "tables", "discrimination_metrics.csv"))
ggsave(here::here("outputs", "figures", "roc_comparison.png"), p_roc, width = 6, height = 6)

message("Valutazione discriminazione completata.")

