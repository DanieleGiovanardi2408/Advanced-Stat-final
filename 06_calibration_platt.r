## R/06_calibration_platt.R

source("R/00_setup_packages.R")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
test_fe  <- readRDS(here::here("data", "processed", "test_fe.rds"))
target_var <- "default_payment_next_month"

# Funzione Platt Scaling
calibrate_platt <- function(train, test, prob_col, label) {
  # Clipping
  p_raw_train <- pmin(pmax(train[[prob_col]], 1e-6), 1 - 1e-6)
  p_raw_test  <- pmin(pmax(test[[prob_col]], 1e-6), 1 - 1e-6)
  
  # Logit trasformazione
  df_cal <- data.frame(y = train[[target_var]], logit_p = qlogis(p_raw_train))
  
  # Fit Logistic Calibration
  cal_mod <- glm(y ~ logit_p, data = df_cal, family = binomial)
  
  # Apply Calibration
  train[[paste0(prob_col, "_platt")]] <- predict(cal_mod, newdata = data.frame(logit_p = qlogis(p_raw_train)), type = "response")
  test[[paste0(prob_col, "_platt")]]  <- predict(cal_mod, newdata = data.frame(logit_p = qlogis(p_raw_test)), type = "response")
  
  list(train = train, test = test, mod = cal_mod)
}

# Calibrazione Logit Full
res_cal_logit <- calibrate_platt(train_fe, test_fe, "p_logit_full", "logit_full")
train_fe <- res_cal_logit$train
test_fe  <- res_cal_logit$test

# Calibrazione RF (Usa OOB preds per training del calibratore, ma applica a full preds)
# Nota: qui usiamo p_rf_oob per fittare la calibrazione, ma applichiamo la trasformazione a p_rf_full
cal_mod_rf <- glm(train_fe[[target_var]] ~ qlogis(pmin(pmax(train_fe$p_rf_oob, 1e-6), 1-1e-6)), family = binomial)

train_fe$p_rf_full_platt <- predict(cal_mod_rf, newdata = data.frame(p_rf_oob = qlogis(pmin(pmax(train_fe$p_rf_full, 1e-6), 1-1e-6))), type = "response")
# Correzione nome variabile nel newdata per coerenza con formula implicita glm o ricostruzione esplicita
# Ricostruzione esplicita più sicura:
coef_rf <- coef(cal_mod_rf)
f_cal <- function(p) plogis(coef_rf[1] + coef_rf[2] * qlogis(pmin(pmax(p, 1e-6), 1-1e-6)))

train_fe$p_rf_full_platt <- f_cal(train_fe$p_rf_full)
test_fe$p_rf_full_platt  <- f_cal(test_fe$p_rf_full)

# Salvataggio
dir.create(here::here("models"), recursive = TRUE, showWarnings = FALSE)
saveRDS(res_cal_logit$mod, here::here("models", "cal_platt_logit.rds"))
saveRDS(cal_mod_rf, here::here("models", "cal_platt_rf.rds"))

saveRDS(train_fe, here::here("data", "processed", "train_fe.rds"))
saveRDS(test_fe,  here::here("data", "processed", "test_fe.rds"))

message("Calibrazione Platt eseguita.")