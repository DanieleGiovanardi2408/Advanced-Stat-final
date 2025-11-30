## R/14_logit_imbalance_check.R

source("R/00_setup_packages.R")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
test_fe  <- readRDS(here::here("data", "processed", "test_fe.rds"))
target_var <- "default_payment_next_month"

# Pulizia dati
excl <- c("row_id", grep("^p_", names(train_fe), value = TRUE))
train_cl <- train_fe %>% select(-any_of(excl))
test_cl  <- test_fe  %>% select(-any_of(excl))

# 1. Unweighted
mod_unw <- glm(default_payment_next_month ~ ., data = train_cl, family = binomial)
p_unw   <- predict(mod_unw, newdata = test_cl, type = "response")

# 2. Weighted (Inverse Frequency)
p1 <- mean(as.numeric(as.character(train_cl[[target_var]])) == 1)
w_vec <- ifelse(train_cl[[target_var]] == 1, 1/p1, 1/(1-p1))
mod_wei <- glm(default_payment_next_month ~ ., data = train_cl, family = binomial, weights = w_vec)
p_wei   <- predict(mod_wei, newdata = test_cl, type = "response")

# 3. Balanced Downsampling
set.seed(123)
idx_1 <- which(train_cl[[target_var]] == 1)
idx_0 <- sample(which(train_cl[[target_var]] == 0), length(idx_1))
train_bal <- train_cl[c(idx_1, idx_0), ]
mod_bal <- glm(default_payment_next_month ~ ., data = train_bal, family = binomial)
p_bal   <- predict(mod_bal, newdata = test_cl, type = "response")

# Valutazione
calc_auc <- function(y, p) as.numeric(pROC::auc(as.numeric(as.character(y)), p))
y_test <- test_cl[[target_var]]

res_imb <- tibble(
  Strategy = c("Unweighted", "Class Weights", "Downsampling"),
  AUC = c(calc_auc(y_test, p_unw), calc_auc(y_test, p_wei), calc_auc(y_test, p_bal))
)

write_csv(res_imb, here::here("outputs", "tables", "imbalance_strategies_comparison.csv"))

message("Check strategie sbilanciamento completato.")


