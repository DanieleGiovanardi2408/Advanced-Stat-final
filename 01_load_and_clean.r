## R/01_load_and_clean.R

source("R/00_setup_packages.R")

# 1. Lettura dati raw
# Assumiamo il file nella root del progetto
raw_path <- here::here("default of credit card clients.xls")

if (!file.exists(raw_path)) {
  stop("File input non trovato: ", raw_path)
}

# Lettura senza header per gestire le prime righe spurie
default_raw <- readxl::read_excel(raw_path, col_names = FALSE)

# Rimozione colonne vuote
default_raw <- default_raw %>%
  dplyr::select(where(~ !all(is.na(.))))

# 2. Rinomina colonne (Dataset UCI standard)
colnames(default_raw) <- c(
  "id", "limit_bal", "sex", "education", "marriage", "age",
  paste0("pay_", c(0, 2:6)),
  paste0("bill_amt", 1:6),
  paste0("pay_amt", 1:6),
  "default_payment_next_month"
)

# 3. Pulizia righe header e conversione tipi
# Rimozione riga intestazione se presente
if (!is.na(default_raw$id[1]) && as.character(default_raw$id[1]) == "ID") {
  default_raw <- default_raw[-1, ]
}

default_raw <- default_raw %>%
  dplyr::mutate(dplyr::across(everything(), as.numeric))

# 4. Cleaning categoriali e ID
default_clean <- default_raw %>%
  # Raggruppamento livelli minori per Education e Marriage
  dplyr::mutate(
    education = dplyr::case_when(
      education %in% c(0, 5, 6) ~ 4L, 
      TRUE ~ as.integer(education)
    ),
    marriage = dplyr::case_when(
      marriage == 0 ~ 3L,
      TRUE ~ as.integer(marriage)
    )
  ) %>%
  # Creazione row_id interno
  dplyr::mutate(row_id = dplyr::row_number()) %>%
  dplyr::select(-id) %>%
  dplyr::relocate(row_id, .before = 1)

# 5. Salvataggio
out_path <- here::here("data", "interim", "default_clean.rds")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(default_clean, out_path)

message("Dataset pulito salvato in: ", out_path)

