## R/13_logit_weights_eval.R

source("R/00_setup_packages.R")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
test_fe  <- readRDS(here::here("data", "processed", "test_fe.rds"))
target_var <- "default_payment_next_month"

# Preparazione dati (solo variabili modello)
excl <- c("row_id", grep("^p_", names(train_fe), value = TRUE))
train_w <- train_fe %>% select(-any_of(excl))
test_w  <- test_fe  %>% select(-any_of(excl))

# Calcolo Pesi
p1 <- mean(as.numeric(as.character(train_w[[target_var]])) == 1)
weights_vec <- ifelse(train_w[[target_var]] == 1, 1/p1, 1/(1-p1))

# Fit Modello Pesato
mod_w <- glm(default_payment_next_month ~ ., data = train_w, family = binomial, weights = weights_vec)

# Predizione
p_test_w <- predict(mod_w, newdata = test_w, type = "response")

# Confronto con Logit Platt Base
y_test <- as.numeric(as.character(test_fe[[target_var]]))
p_base <- test_fe$p_logit_full_platt

res_comp <- tibble(
  model = c("Logit Base (Platt)", "Logit Weighted"),
  AUC = c(as.numeric(pROC::auc(y_test, p_base)), as.numeric(pROC::auc(y_test, p_test_w))),
  Brier = c(mean((y_test - p_base)^2), mean((y_test - p_test_w)^2))
)

dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(res_comp, here::here("outputs", "tables", "logit_weighted_comparison.csv"))

message("Confronto Logit Pesato completato.")
