## code to prepare `cheese_abundance` dataset goes here
library(tidyverse)
library(phyloseq)
library(glue)

phylo_cheese <- read_rds("data-raw/data/phyloseq_cheese.rds")

cheese_taxonomy <-
  phylo_cheese %>%
  tax_table() %>%
  as.data.frame() %>%
  as_tibble(rownames = "sequence") %>%
  mutate(lineage = glue("k__{Kingdom}|p__{Phylum}|c__{Class}|o__{Order}|f__{Family}|g__{Genus}|s__{Species}"),
         lineage = str_replace_all(lineage, " ", "_")) %>%
  arrange(lineage) %>%
  rowid_to_column(var = "asv") %>%
  mutate(asv = if_else(asv >= 10,
                       paste0("asv_", asv),
                       paste0("asv_0", asv))) %>%
  select(asv, lineage, everything())

cheese_abundance <-
  cheese_taxonomy %>%
  select(asv, sequence) %>%
  left_join(otu_table(phylo_cheese)@.Data %>%
              as_tibble(rownames = "sequence"), by = "sequence") %>%
  select(-sequence) %>%
  pivot_longer(-asv, names_to = "sample", values_to = "count") %>%
  pivot_wider(names_from = asv, values_from = count) %>%
  mutate(sample = str_remove_all(sample, "_ITS2_CURATED"),
         sample = str_replace_all(sample, "echantillon", "sample")) %>%
  mutate(cheese = str_remove_all(sample, "sample"),
         cheese = str_remove_all(cheese, "-[1-3]"),
         cheese = case_when(cheese == 1 ~ "Saint-Nectaire",
                            cheese == 2 ~ "Livarot",
                            cheese == 3 ~ "Epoisses"),
         rind_type = if_else(cheese == "Saint-Nectaire", "Natural", "Washed"),
         .after = sample)

cheese_taxonomy <- select(cheese_taxonomy, -sequence)

# usethis::use_data(cheese_abundance, overwrite = TRUE)
# usethis::use_data(cheese_taxonomy, overwrite = TRUE)
