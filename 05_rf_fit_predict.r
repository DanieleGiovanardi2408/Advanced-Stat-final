## R/05_rf_fit_predict.R

source("R/00_setup_packages.R")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
test_fe  <- readRDS(here::here("data", "processed", "test_fe.rds"))
target_var <- "default_payment_next_month"

# Preparazione dati RF (esclusione probabilità esistenti e row_id)
cols_excl <- c("row_id", grep("^p_", names(train_fe), value = TRUE))
train_rf  <- train_fe %>% select(-all_of(cols_excl))
test_rf   <- test_fe  %>% select(-all_of(cols_excl))

# Conversione target a factor
train_rf[[target_var]] <- factor(train_rf[[target_var]])
test_rf[[target_var]]  <- factor(test_rf[[target_var]], levels = levels(train_rf[[target_var]]))

# Fit Ranger (Random Forest)
set.seed(1234)
rf_mod <- ranger(
  formula         = as.formula(paste(target_var, "~ .")),
  data            = train_rf,
  probability     = TRUE,
  num.trees       = 300,
  mtry            = floor(sqrt(ncol(train_rf) - 1)),
  min.node.size   = 50,
  importance      = "impurity",
  oob.error       = TRUE
)

# Estrazione Probabilità
# OOB Predictions sul train (per calibrazione onesta)
train_fe$p_rf_oob  <- rf_mod$predictions[, "1"]
# Prediction standard
train_fe$p_rf_full <- predict(rf_mod, data = train_rf)$predictions[, "1"]
test_fe$p_rf_full  <- predict(rf_mod, data = test_rf)$predictions[, "1"]

# Importanza Variabili
imp_df <- tibble::tibble(
  variable = names(rf_mod$variable.importance),
  importance = rf_mod$variable.importance
) %>% arrange(desc(importance))

# Salvataggio
dir.create(here::here("models"), recursive = TRUE, showWarnings = FALSE)
saveRDS(rf_mod, here::here("models", "rf_full.rds"))

dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(imp_df, here::here("outputs", "tables", "rf_importance.csv"))

# Salvataggio Dataset Aggiornati
saveRDS(train_fe, here::here("data", "processed", "train_fe.rds"))
saveRDS(test_fe,  here::here("data", "processed", "test_fe.rds"))

# Plot Top 20 Features
p_imp <- imp_df %>% slice(1:20) %>%
  ggplot(aes(x = importance, y = reorder(variable, importance))) +
  geom_col() +
  labs(title = "RF Variable Importance (Top 20)", x = "Impurity", y = NULL)

dir.create(here::here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
ggsave(here::here("outputs", "figures", "rf_importance.png"), p_imp, width = 8, height = 6)

message("Random Forest stimato e salvato.")