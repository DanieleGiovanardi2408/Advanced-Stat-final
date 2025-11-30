## run_pipeline.R
## =========================================================================
## MASTER PIPELINE: Credit Default Prediction Project
## Versione: CLEAN EXIT (Previene crash interfaccia RStudio)
## =========================================================================

# Funzione per pulire la memoria
clean_mem <- function() {
  gc(verbose = FALSE)
}

# --- 1. SETUP E DATA INGESTION ---
message("\n[1/7] Inizio Setup e Pulizia Dati...")
source("R/00_setup_packages.R")
source("R/01_load_and_clean.R")
clean_mem()

# --- 2. SPLIT E PREPROCESSING ---
message("\n[2/7] Splitting e Feature Engineering...")
source("R/02_split_data.R")
source("R/03_feature_engineering.R")
if(exists("default_raw")) rm(default_raw) # Rimuovi dati grezzi
clean_mem()

# --- 3. TRAINING MODELLI ---
message("\n[3/7] Training Modelli (Logit & Random Forest)...")
source("R/04_logit_fit_predict.R")
source("R/04b_multicollinearity_logit_full.R")
clean_mem()

message("...Running Random Forest...")
source("R/05_rf_fit_predict.R")
# TRUCCO ANTI-CRASH: Rimuoviamo il modello pesante subito dopo averlo salvato
if(exists("rf_mod")) rm(rf_mod)
clean_mem()

# --- 4. CALIBRAZIONE ---
message("\n[4/7] Calibrazione Probabilità...")
source("R/06_calibration_platt.R")
clean_mem()

# --- 5. VALUTAZIONE PERFORMANCE ---
message("\n[5/7] Valutazione Performance...")
source("R/07_eval_discrimination.R")
source("R/08_eval_calibration.R")
clean_mem()

# --- 6. OTTIMIZZAZIONE SOGLIE E SEGMENTAZIONE ---
message("\n[6/7] Analisi Soglie e Segmentazione...")
source("R/09_threshold_selection.R")
source("R/10_kmeans_segments.R")
source("R/11_thresholds_by_segment.R")
clean_mem()

# --- 7. INTERPRETABILITÀ E ROBUSTEZZA ---
message("\n[7/7] Interpretabilità e Controlli Finali...")

# Gestione nome file (che sia .R o .r o .R.r)
pdp_files <- list.files("R", pattern = "12_pdp_ice", full.names = TRUE)
if(length(pdp_files) > 0) {
  source(pdp_files[1]) 
} else {
  message("Script 12 saltato (file non trovato).")
}
clean_mem()

source("R/13_logit_weights_eval.R")
source("R/14_logit_imbalance_check.R")

message("\n========================================================")
message(" PIPELINE COMPLETATA CON SUCCESSO! ")
message(" I risultati sono in 'outputs/'.")
message(" Ora pulisco la memoria per evitare blocchi...")
message("========================================================")

# PULIZIA FINALE AGGRESSIVA PER EVITARE CRASH UI
rm(list = ls()) 
gc()
