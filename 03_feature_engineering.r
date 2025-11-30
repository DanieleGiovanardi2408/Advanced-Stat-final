## R/03_feature_engineering.R

source("R/00_setup_packages.R")

train <- readRDS(here::here("data", "interim", "train.rds"))
test  <- readRDS(here::here("data", "interim", "test.rds"))

# Funzione di trasformazione
make_features <- function(df) {
  df_fe <- df
  
  # Categoriali come fattori
  df_fe <- df_fe %>%
    dplyr::mutate(
      sex       = factor(sex),
      education = factor(education),
      marriage  = factor(marriage)
    )
  
  # Trasformazioni logaritmiche
  df_fe <- df_fe %>%
    dplyr::mutate(
      limit_bal_log = log1p(limit_bal),
      age_log       = log1p(age)
    )
  
  # Feature cols
  bill_cols    <- paste0("bill_amt", 1:6)
  pay_amt_cols <- paste0("pay_amt", 1:6)
  pay_cols     <- c("pay_0", paste0("pay_", 2:6))
  
  # 1. Utilizzo del credito (Bill / Limit)
  for (k in 1:6) {
    df_fe[[paste0("util_bill", k)]] <- ifelse(
      df_fe[[paste0("bill_amt", k)]] > 0 & df_fe$limit_bal > 0,
      df_fe[[paste0("bill_amt", k)]] / df_fe$limit_bal, 0
    )
  }
  util_mat <- as.matrix(df_fe[paste0("util_bill", 1:6)])
  df_fe$util_mean <- rowMeans(util_mat, na.rm = TRUE)
  df_fe$util_max  <- do.call(pmax, c(as.data.frame(util_mat), list(na.rm = TRUE)))
  df_fe$util_last <- df_fe$util_bill6
  df_fe$high_util_months <- rowSums(util_mat >= 0.9, na.rm = TRUE)
  
  # 2. Ritardi (PAY_x)
  pay_mat <- as.matrix(df_fe[pay_cols])
  df_fe$num_late   <- rowSums(pay_mat > 0, na.rm = TRUE)
  df_fe$max_delay  <- do.call(pmax, c(as.data.frame(pay_mat), list(na.rm = TRUE)))
  df_fe$last_delay <- df_fe$pay_0
  df_fe$ever_severe <- as.integer(df_fe$max_delay >= 2)
  
  # 3. Rapporto Pagamento/Fattura
  for (k in 1:6) {
    df_fe[[paste0("pay_ratio", k)]] <- ifelse(
      df_fe[[paste0("bill_amt", k)]] > 0,
      pmin(df_fe[[paste0("pay_amt", k)]] / df_fe[[paste0("bill_amt", k)]], 5),
      NA_real_
    )
  }
  ratio_mat <- as.matrix(df_fe[paste0("pay_ratio", 1:6)])
  df_fe$pay_ratio_mean <- rowMeans(ratio_mat, na.rm = TRUE)
  df_fe$last_pay_ratio <- df_fe$pay_ratio6
  df_fe$underpay_months <- rowSums(ratio_mat < 0.2, na.rm = TRUE)
  
  # 4. Indicatori finanziari aggregati
  df_fe$total_bill <- rowSums(as.matrix(df_fe[bill_cols]), na.rm = TRUE)
  df_fe$total_pay  <- rowSums(as.matrix(df_fe[pay_amt_cols]), na.rm = TRUE)
  
  df_fe$pay_to_bill <- ifelse(df_fe$total_bill > 0, df_fe$total_pay / df_fe$total_bill, NA)
  df_fe$pay_to_limit <- ifelse(df_fe$limit_bal > 0, df_fe$total_pay / (df_fe$limit_bal * 6), NA)
  df_fe$bill_trend <- (df_fe$bill_amt6 - df_fe$bill_amt1) / 5
  
  df_fe
}

# Applicazione FE
train_fe <- make_features(train)
test_fe  <- make_features(test)

# Controllo correlazioni
numeric_train <- train_fe %>%
  dplyr::select(where(is.numeric), -row_id, -default_payment_next_month)
cor_mat <- cor(numeric_train, use = "pairwise.complete.obs")

# Salvataggio
dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write.csv(as.data.frame(cor_mat), here::here("outputs", "tables", "feature_correlations.csv"))

dir.create(here::here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
saveRDS(train_fe, here::here("data", "processed", "train_fe.rds"))
saveRDS(test_fe,  here::here("data", "processed", "test_fe.rds"))

message("Feature Engineering completata.")
