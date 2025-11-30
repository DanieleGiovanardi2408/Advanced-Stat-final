## R/04_logit_fit_predict.R

source("R/00_setup_packages.R")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
test_fe  <- readRDS(here::here("data", "processed", "test_fe.rds"))
target_var <- "default_payment_next_month"

# 1. Definizione formule modelli
# M1: Solo demografiche
form_demo <- as.formula(paste(target_var, "~ sex + education + marriage + age_log + limit_bal_log"))

# M2: Demografiche + Core Features
form_core <- as.formula(paste(target_var, "~ sex + education + marriage + age_log + limit_bal_log + 
                              last_delay + ever_severe + util_last + high_util_months + 
                              pay_ratio_mean + underpay_months + bill_trend"))

# M3: Full Features
form_full <- as.formula(paste(target_var, "~ sex + education + marriage + age_log + limit_bal_log + 
                              last_delay + ever_severe + num_late + util_mean + util_max + high_util_months + 
                              pay_ratio_mean + last_pay_ratio + underpay_months + pay_to_bill + pay_to_limit + bill_trend"))

# 2. Funzione Fit & Predict
fit_logit <- function(formula, train, test, label) {
  mod <- glm(formula, data = train, family = binomial(link = "logit"))
  
  # Predizioni (probabilità)
  train[[paste0("p_", label)]] <- predict(mod, newdata = train, type = "response")
  test[[paste0("p_", label)]]  <- predict(mod, newdata = test,  type = "response")
  
  metrics <- tibble::tibble(
    model = label, AIC = AIC(mod), BIC = BIC(mod),
    pseudo_r2 = 1 - mod$deviance / mod$null.deviance
  )
  list(mod = mod, train = train, test = test, metrics = metrics)
}

# 3. Esecuzione modelli
res_demo <- fit_logit(form_demo, train_fe, test_fe, "logit_demo")
res_core <- fit_logit(form_core, res_demo$train, res_demo$test, "logit_core")
res_full <- fit_logit(form_full, res_core$train, res_core$test, "logit_full")

# Aggiornamento dataset
train_fe <- res_full$train
test_fe  <- res_full$test

# 4. Export Risultati
dir.create(here::here("models"), recursive = TRUE, showWarnings = FALSE)
saveRDS(res_demo$mod, here::here("models", "logit_demo.rds"))
saveRDS(res_core$mod, here::here("models", "logit_core.rds"))
saveRDS(res_full$mod, here::here("models", "logit_full.rds"))

# Summary metriche
summary_df <- bind_rows(res_demo$metrics, res_core$metrics, res_full$metrics)
write_csv(summary_df, here::here("outputs", "tables", "logit_summary.csv"))

# Coefficienti del modello Full
coef_df <- broom::tidy(res_full$mod, conf.int = TRUE) %>%
  mutate(odds_ratio = exp(estimate), or_low = exp(conf.low), or_high = exp(conf.high))
write_csv(coef_df, here::here("outputs", "tables", "logit_full_coefficients.csv"))

# Aggiornamento dati processati
saveRDS(train_fe, here::here("data", "processed", "train_fe.rds"))
saveRDS(test_fe,  here::here("data", "processed", "test_fe.rds"))

# Plot Coefficienti (Full)
p_coef <- coef_df %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = odds_ratio, y = reorder(term, odds_ratio))) +
  geom_point() + geom_errorbarh(aes(xmin = or_low, xmax = or_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Odds Ratio - Logit Full", x = "OR (log scale)", y = NULL) +
  scale_x_log10()

dir.create(here::here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
ggsave(here::here("outputs", "figures", "logit_full_coef.png"), p_coef, width = 8, height = 6)

message("Modelli Logit stimati e salvati.")
