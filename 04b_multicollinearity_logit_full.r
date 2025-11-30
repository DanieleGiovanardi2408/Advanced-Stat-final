## R/04b_multicollinearity_logit_full.R

source("R/00_setup_packages.R")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
target_var <- "default_payment_next_month"

# Ricostruzione formula Logit Full per analisi VIF
form_logit_full <- as.formula(paste(target_var, "~ sex + education + marriage + age_log + limit_bal_log + 
                              last_delay + ever_severe + num_late + util_mean + util_max + high_util_months + 
                              pay_ratio_mean + last_pay_ratio + underpay_months + pay_to_bill + pay_to_limit + bill_trend"))

mod_vif <- glm(form_logit_full, data = train_fe, family = binomial(link = "logit"))

# Calcolo VIF
vif_vals <- car::vif(mod_vif)

# Formattazione output VIF
if (is.matrix(vif_vals)) {
  vif_df <- as.data.frame(vif_vals) %>%
    tibble::rownames_to_column("variable") %>%
    rename(GVIF = 2, Df = 3)
} else {
  vif_df <- tibble::tibble(variable = names(vif_vals), VIF = vif_vals)
}

dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(vif_df, here::here("outputs", "tables", "logit_full_vif.csv"))

# Heatmap Correlazione Predittori
pred_vars <- all.vars(form_logit_full)[-1] # rimuove target
num_preds <- train_fe %>% select(all_of(pred_vars)) %>% select(where(is.numeric))
cor_mat   <- cor(num_preds, use = "pairwise.complete.obs")

p_cor <- as.data.frame(cor_mat) %>%
  tibble::rownames_to_column("var1") %>%
  tidyr::pivot_longer(-var1, names_to = "var2", values_to = "corr") %>%
  ggplot(aes(var1, var2, fill = corr)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlazione Predittori Numerici", x = NULL, y = NULL)

dir.create(here::here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
ggsave(here::here("outputs", "figures", "logit_full_cor_heatmap.png"), p_cor, width = 8, height = 7)

message("Analisi multicollinearità completata.")