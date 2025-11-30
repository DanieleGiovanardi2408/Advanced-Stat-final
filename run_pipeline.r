## run_pipeline.R
## =========================================================================
## Master Pipeline per Credit Default Prediction
## Esegue tutti gli step dall'ingestione dati alla valutazione finale.
## =========================================================================

# 1. Setup Iniziale e Pulizia
cat("\n--- STEP 1: LOAD & CLEAN ---\n")
source("R/00_setup_packages.R")
source("R/01_load_and_clean.R")
source("R/02_split_data.R")

# 2. Feature Engineering
cat("\n--- STEP 2: FEATURE ENGINEERING ---\n")
source("R/03_feature_engineering.R")

# 3. Modellistica (Fit & Predict)
cat("\n--- STEP 3: MODEL FITTING ---\n")
source("R/04_logit_fit_predict.R")
source("R/05_rf_fit_predict.R")

# 4. Diagnostica e Calibrazione
cat("\n--- STEP 4: DIAGNOSTICS & CALIBRATION ---\n")
source("R/04b_multicollinearity_logit_full.R")
source("R/06_calibration_platt.R")

# 5. Valutazione Performance (Generale)
cat("\n--- STEP 5: EVALUATION ---\n")
source("R/07_eval_discrimination.R")
source("R/08_eval_calibration.R")

# 6. Ottimizzazione Soglie e Segmentazione
cat("\n--- STEP 6: SEGMENTATION & THRESHOLDS ---\n")
source("R/09_threshold_selection.R")
source("R/10_kmeans_segments.R")
source("R/11_thresholds_by_segment.R")

# 7. Interpretabilità e Robustezza
cat("\n--- STEP 7: EXPLAINABILITY & CHECKS ---\n")
source("R/12_pdp_ice.R")
source("R/13_logit_weights_eval.R")
source("R/14_logit_imbalance_check.R")

cat("\n======================================================\n")
cat(" PIPELINE COMPLETATA CON SUCCESSO. \n")
cat(" Controllare la cartella 'outputs' per tabelle e grafici.\n")
cat("======================================================\n")