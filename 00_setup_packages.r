## R/00_setup_packages.R

# Lista pacchetti necessari
pkgs <- c(
  "tidyverse",    # data manipulation & plotting
  "janitor",      # cleaning names
  "rsample",      # splitting
  "yardstick",    # metrics
  "pROC",         # ROC/AUC
  "ranger",       # Random Forest
  "broom",        # tidy models
  "car",          # VIF
  "readxl",       # excel reader
  "here"          # file paths
)

# Installazione pacchetti mancanti
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) {
  install.packages(to_install)
}

# Caricamento librerie
invisible(lapply(pkgs, library, character.only = TRUE))

# Impostazioni globali
set.seed(1234)
theme_set(theme_minimal())
options(stringsAsFactors = FALSE)
