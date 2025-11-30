## R/12_pdp_ice.R

source("R/00_setup_packages.R")
if (!requireNamespace("pdp", quietly = TRUE)) install.packages("pdp")
library(pdp)

# Pulizia memoria aggressiva prima di iniziare
gc()

message("Caricamento modelli per PDP...")
train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
logit_mod <- readRDS(here::here("models", "logit_full.rds"))
rf_mod    <- readRDS(here::here("models", "rf_full.rds"))

# --- MODIFICA CRITICA PER STABILITÀ ---
# Riduciamo il campione a 50 righe (invece di 500 o tutto il dataset).
# Questo previene il crash "R Session Aborted" su computer con RAM standard.
set.seed(1234)
excl <- c("row_id", grep("^p_", names(train_fe), value = TRUE))
train_pdp <- train_fe %>% 
  dplyr::select(-dplyr::any_of(excl)) %>% 
  dplyr::sample_n(min(50, nrow(.))) 

message("Campione ridotto a ", nrow(train_pdp), " righe per stabilità.")

# Selezione variabili (Top 3 per risparmiare tempo)
imp_path <- here::here("outputs", "tables", "rf_importance.csv")
if(file.exists(imp_path)) {
  top_vars <- read_csv(imp_path, show_col_types = FALSE)$variable[1:3]
} else {
  top_vars <- c("pay_0", "limit_bal_log", "bill_trend")
}

# Funzioni wrapper predizione
pred_logit <- function(obj, newdata) predict(obj, newdata, type="response")
pred_rf    <- function(obj, newdata) predict(obj, data=newdata)$predictions[,"1"]

dir.create(here::here("outputs", "figures", "pdp"), recursive = TRUE, showWarnings = FALSE)

# Loop protetto da tryCatch per evitare che un errore fermi l'intera pipeline
for (v in top_vars) {
  message(paste("Generazione PDP per:", v, "..."))
  
  tryCatch({
    # Logit PDP
    p1 <- partial(logit_mod, pred.var = v, train = train_pdp, pred.fun = pred_logit) %>%
      autoplot() + labs(title = paste("Logit PDP -", v))
    
    ggsave(here::here("outputs", "figures", "pdp", paste0("pdp_logit_", v, ".png")), p1, width=5, height=4)
    
    # RF PDP (La parte più pesante)
    p2 <- partial(rf_mod, pred.var = v, train = train_pdp, pred.fun = pred_rf) %>%
      autoplot() + labs(title = paste("RF PDP -", v))
    
    ggsave(here::here("outputs", "figures", "pdp", paste0("pdp_rf_", v, ".png")), p2, width=5, height=4)
    
  }, error = function(e) {
    message(paste("ATTENZIONE: Saltato PDP per variabile", v, "- Errore o Memoria Insufficiente."))
    message("L'errore era: ", e$message)
  })
  
  # Pulisce memoria dopo ogni ciclo
  gc()
}

message("Grafici PDP salvati (o saltati in sicurezza).")


