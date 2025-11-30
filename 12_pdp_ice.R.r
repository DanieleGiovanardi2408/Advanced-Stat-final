## R/12_pdp_ice.R

source("R/00_setup_packages.R")
if (!requireNamespace("pdp", quietly = TRUE)) install.packages("pdp")
library(pdp)

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
logit_mod <- readRDS(here::here("models", "logit_full.rds"))
rf_mod    <- readRDS(here::here("models", "rf_full.rds"))

# Setup dati per PDP (rimozione colonne tecniche)
excl <- c("row_id", grep("^p_", names(train_fe), value = TRUE))
train_pdp <- train_fe %>% select(-any_of(excl)) %>% sample_n(min(500, nrow(.))) # Downsampling per velocità

# Selezione variabili (Top 4 da RF importance se esiste, altrimenti default)
imp_path <- here::here("outputs", "tables", "rf_importance.csv")
if(file.exists(imp_path)) {
  top_vars <- read_csv(imp_path, show_col_types = FALSE)$variable[1:4]
} else {
  top_vars <- c("pay_0", "limit_bal_log", "bill_trend", "pay_ratio_mean")
}

# Funzioni wrapper predizione
pred_logit <- function(obj, newdata) predict(obj, newdata, type="response")
pred_rf    <- function(obj, newdata) predict(obj, data=newdata)$predictions[,"1"]

dir.create(here::here("outputs", "figures", "pdp"), recursive = TRUE, showWarnings = FALSE)

for (v in top_vars) {
  # Logit PDP
  p1 <- partial(logit_mod, pred.var = v, train = train_pdp, pred.fun = pred_logit) %>%
    autoplot() + labs(title = paste("Logit PDP -", v))
  
  # RF PDP
  p2 <- partial(rf_mod, pred.var = v, train = train_pdp, pred.fun = pred_rf) %>%
    autoplot() + labs(title = paste("RF PDP -", v))
  
  ggsave(here::here("outputs", "figures", "pdp", paste0("pdp_logit_", v, ".png")), p1, width=5, height=4)
  ggsave(here::here("outputs", "figures", "pdp", paste0("pdp_rf_", v, ".png")), p2, width=5, height=4)
}

message("Grafici PDP salvati in outputs/figures/pdp/.")

