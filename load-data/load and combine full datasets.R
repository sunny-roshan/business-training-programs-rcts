# We have the full datasets publicly available for 6 studies, of which Berge (Tanzania) reported two separate sets of results - so in effect we have 7 datasets.
# These have all individually been loaded, cleaned, and saved. Now, load them all, combine them into one df, and save.

# Load libraries
library(dplyr)
library(here)
rm(list = ls())

# Load cleaned datasets
Kenya_clean <- read.csv(here("cleaned-datasets", "Kenya.csv"))
Nigeria1_clean <- read.csv(here("cleaned-datasets", "Nigeria1.csv"))
Nigeria2_clean <- read.csv(here("cleaned-datasets", "Nigeria2.csv"))
Pakistan_clean <- read.csv(here("cleaned-datasets", "Pakistan.csv"))
RSA_Fin_clean <- read.csv(here("cleaned-datasets", "RSA_Finance.csv"))
RSA_Mkt_clean <- read.csv(here("cleaned-datasets", "RSA_Market.csv"))
Tanzania_clean <- read.csv(here("cleaned-datasets", "Tanzania.csv"))

# Combine into one df
Master <- bind_rows(
  Kenya_clean,
  Nigeria1_clean,
  Nigeria2_clean,
  Pakistan_clean,
  RSA_Fin_clean,
  RSA_Mkt_clean,
  Tanzania_clean
)

# Check Profits
sum(is.na(Master$Profit_zscore)) # 690
sum(is.na(Master$female)) # 2
sum(is.na(Master$urban)) #0
sum(is.na(Master$Revenue_zscore)) # 1164
Master_noprofna <- Master %>% filter(!is.na(Profit_zscore), !is.na(female))
sum(is.na(Master_noprofna$Profit_zscore))
sum(is.na(Master_noprofna$female))
Master_noprofna <- as.data.frame(Master_noprofna)
View(Master_noprofna)
Master_noprofna <- Master_noprofna %>% select(treatment_indicator, Profit_zscore, urban, female, group)

# Save final combined profits df
write.table(Master_noprofna,
            file = here("cleaned-datasets", "Profits master dataset.csv"),
            sep = ",",
            row.names = FALSE)

# Check Sales
Master_nosalesna <- Master %>% filter(!is.na(Revenue_zscore), !is.na(female), !is.na(urban))
Master_nosalesna <- as.data.frame(Master_nosalesna)
Master_nosalesna <- Master_nosalesna %>% select(treatment_indicator, Revenue_zscore, urban, female, group)
View(Master_nosalesna)

# Save final combined sales df
write.table(Master_nosalesna,
            file = here("cleaned-datasets", "Sales master dataset.csv"),
            sep = ",",
            row.names = FALSE)
