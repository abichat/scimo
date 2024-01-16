## code to prepare `pedcan_expression` dataset goes here

# Datasets came from DepMap Public 23Q4 primary files
# https://depmap.org/portal/download/all/

library(readr)
library(dplyr)
library(stringr)
library(janitor)


df_model_raw <-
  read_csv("data-raw/data/Model.csv") %>%
  clean_names()

df_expr_raw <- read_csv("data-raw/data/OmicsExpressionProteinCodingGenesTPMLogp1.csv")


df_model <-
  df_model_raw %>%
  select(model_id, cell_line = cell_line_name, sex,
         event = primary_or_metastasis,
         disease = oncotree_primary_disease) %>%
  filter(disease %in% c("Ewing Sarcoma", "Rhabdomyosarcoma",
                        "Osteosarcoma", "Embryonal Tumor",
                        "Neuroblastoma")) %>%
  mutate(event = if_else(is.na(event), "Unknown", event)) %>%
  arrange(cell_line)


df_expr <-
  df_expr_raw %>%
  rename_with(~ str_remove_all(., " \\(.*\\)")) %>%
  select(., model_id = ...1, all_of(sort(colnames(.))))


pedcan_expression <-
  df_model %>%
  inner_join(df_expr, by = "model_id") %>%
  select(-model_id)


# usethis::use_data(pedcan_expression, overwrite = TRUE)
