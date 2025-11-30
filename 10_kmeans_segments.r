## R/10_kmeans_segments.R

source("R/00_setup_packages.R")

train_fe <- readRDS(here::here("data", "processed", "train_fe.rds"))
test_fe  <- readRDS(here::here("data", "processed", "test_fe.rds"))
target_var <- "default_payment_next_month"

# 1. Selezione variabili numeriche (escluse tecniche e probabilità)
excl_cols <- c("row_id", target_var, grep("^p_", names(train_fe), value = TRUE))
feat_cols <- train_fe %>% select(where(is.numeric)) %>% select(-any_of(excl_cols)) %>% names()

X_train <- as.matrix(train_fe[, feat_cols])
X_test  <- as.matrix(test_fe[, feat_cols])

# 2. Preprocessing: Imputazione e Scaling (basato su Train)
train_means <- colMeans(X_train, na.rm = TRUE)
train_sds   <- apply(X_train, 2, sd, na.rm = TRUE)

# Imputazione NA con media
for(j in 1:ncol(X_train)) {
  X_train[is.na(X_train[,j]), j] <- train_means[j]
  X_test[is.na(X_test[,j]), j]   <- train_means[j]
}

X_train_sc <- scale(X_train, center = train_means, scale = train_sds)
X_test_sc  <- scale(X_test,  center = train_means, scale = train_sds)

# 3. Elbow Method
set.seed(1234)
wss <- sapply(2:8, function(k) kmeans(X_train_sc, centers=k, nstart=10)$tot.withinss)

p_elbow <- tibble(k = 2:8, wss = wss) %>%
  ggplot(aes(k, wss)) + geom_line() + geom_point() +
  labs(title = "Elbow Method (Train)", x = "K", y = "WSS")

dir.create(here::here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
ggsave(here::here("outputs", "figures", "kmeans_elbow.png"), p_elbow, width = 6, height = 4)

# 4. Fit K-Means finale (K=4)
k_final <- 4
set.seed(1234)
km_mod <- kmeans(X_train_sc, centers = k_final, nstart = 20)

# Assegnazione cluster
train_fe$segment_k4 <- factor(km_mod$cluster)

# Assegnazione Test (Minimum Distance to Centers)
centers <- km_mod$centers
test_clusters <- apply(X_test_sc, 1, function(row) which.min(colSums((t(centers) - row)^2)))
test_fe$segment_k4 <- factor(test_clusters)

# 5. Profiling Segmenti
prof_train <- train_fe %>% group_by(segment_k4) %>% 
  summarise(n = n(), def_rate = mean(as.numeric(as.character(default_payment_next_month))))

p_prof <- ggplot(prof_train, aes(x=segment_k4, y=def_rate)) + geom_col() +
  labs(title = "Default Rate per Segmento (Train)", y = "Default Rate")

ggsave(here::here("outputs", "figures", "kmeans_profiles.png"), p_prof, width = 6, height = 4)

# Salvataggio
saveRDS(train_fe, here::here("data", "processed", "train_fe.rds"))
saveRDS(test_fe,  here::here("data", "processed", "test_fe.rds"))
write_csv(prof_train, here::here("outputs", "tables", "kmeans_profiles.csv"))

message("Segmentazione K-means completata (K=4).")
