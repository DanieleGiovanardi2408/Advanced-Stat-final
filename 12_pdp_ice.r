## R/12_pdp_ice.R

source("R/00_setup_packages.R")
if (!requireNamespace("pdp", quietly = TRUE)) install.packages("pdp")
library(pdp)

# Pulizia memoria preventiva
gc()

message("--- STEP 12: Partial Dependence Plots ---")

# Caricamento dati
train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
logit_mod <- readRDS(here::here("models", "logit_full.rds"))
rf_mod    <- readRDS(here::here("models", "rf_full.rds"))

# 1. Configurazione Campione (Safe Mode)
# Usiamo 100 righe per avere grafici chiari ma veloci
set.seed(1234)
excl_cols <- c("row_id", grep("^p_", names(train_fe), value = TRUE))

if(nrow(train_fe) > 100) {
  train_pdp <- train_fe %>% 
    dplyr::select(-dplyr::any_of(excl_cols)) %>% 
    dplyr::sample_n(100)
} else {
  train_pdp <- train_fe %>% 
    dplyr::select(-dplyr::any_of(excl_cols))
}

# 2. Definizione Variabili (Solo le 2 più importanti per evitare crash)
top_vars <- c("pay_0", "limit_bal_log")

# 3. Funzioni Wrapper (CORRETTE PER PDP)
# Devono chiamarsi TASSATIVAMENTE 'object' e 'newdata'
pred_logit_fn <- function(object, newdata) {
  predict(object, newdata = newdata, type = "response")
}

pred_rf_fn <- function(object, newdata) {
  predict(object, data = newdata)$predictions[, "1"]
}

dir.create(here::here("outputs", "figures", "pdp"), recursive = TRUE, showWarnings = FALSE)

# 4. Loop Generazione Grafici
for (v in top_vars) {
  # Controllo che la variabile esista
  if(!v %in% names(train_pdp)) next
  
  message(paste("Generazione PDP per:", v, "..."))
  
  # LOGIT
  tryCatch({
    p1 <- partial(logit_mod, pred.var = v, train = train_pdp, pred.fun = pred_logit_fn) %>%
      autoplot() + 
      labs(title = paste("Logit PDP -", v), y = "Prob. Default") +
      theme_minimal()
    
    ggsave(here::here("outputs", "figures", "pdp", paste0("pdp_logit_", v, ".png")), 
           plot = p1, width=5, height=4, bg = "white")
  }, error = function(e) message(paste("Errore Logit:", e$message)))
  
  # RANDOM FOREST
  tryCatch({
    p2 <- partial(rf_mod, pred.var = v, train = train_pdp, pred.fun = pred_rf_fn) %>%
      autoplot() + 
      labs(title = paste("RF PDP -", v), y = "Prob. Default") +
      theme_minimal()
    
    ggsave(here::here("outputs", "figures", "pdp", paste0("pdp_rf_", v, ".png")), 
           plot = p2, width=5, height=4, bg = "white")
  }, error = function(e) message(paste("Errore RF:", e$message)))
  
  gc() # Pulisce RAM
}

message("Step 12 Completato.")


