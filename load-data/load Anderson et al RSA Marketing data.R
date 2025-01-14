# Load libraries
library(dplyr)
library(tidyr)
library(haven)
library(labelled)
library(here)
rm(list=ls())

# Set path
getwd()

# Load dataset
rsa <- read_dta(here("raw-datasets", "P2P_dataset.dta"))

# Check treatments and treatment status
table(rsa$Treatment_FIN, rsa$Treatment_MKT)  # no overlapping treated individuals (clean)

# Keep only marketing treatment and control; separate df for finance treatment for comparison
rsa_mkt <- filter(rsa, Treatment_FIN == 0)

# Check missing data for sales and profits
sum(is.na(rsa_mkt$Profits3_composite_w1))  # 282
sum(is.na(rsa_mkt$Sales5_composite_w1))  # 282
sum(is.na(rsa_fin$Profits3_composite_w1))  # 141
sum(is.na(rsa_fin$Sales5_composite_w1))  # 141

# Standardise data
standardisation_vars_rsa_mkt <- 
  group_by(rsa_mkt, T_survey_round, Treatment_MKT) %>%
  summarize(
    avgprofits = mean(Profits3_composite_w1, na.rm = TRUE),
    sdprofits = sd(Profits3_composite_w1, na.rm = TRUE),
    avgrevenue = mean(Sales5_composite_w1, na.rm = TRUE),
    sdrevenue = sd(Sales5_composite_w1, na.rm = TRUE)
  )
View(standardisation_vars_rsa_mkt)

# We only need the means and sd of the control group, and so can drop the values for the training group
# and then drop the column of treatment status:
standardisation_vars_rsa_mkt <- filter(standardisation_vars_rsa_mkt, Treatment_MKT == 0)
standardisation_vars_rsa_mkt <- standardisation_vars_rsa_mkt[, c(1, 3, 4, 5, 6)]
View(standardisation_vars_rsa_mkt)

# Now we can merge the 'Standardisation_vars_rsa_mkt' dataset to the full rsa_mkt dataset using left_join in dplyr
rsa_mkt_clean <- left_join(rsa_mkt, standardisation_vars_rsa_mkt, by = "T_survey_round")
View(rsa_mkt_clean)

# Now the key transformation! z-scores, standardising the profits in each survey wave by the
# control mean and control sd for the corresponding survey wave:
rsa_mkt_clean <- rsa_mkt_clean %>% mutate(
  Profit_zscore = (Profits3_composite_w1 - avgprofits) / sdprofits,
  Revenue_zscore = (Sales5_composite_w1 - avgrevenue) /
    sdrevenue
)

# Cleaner data:
# 1. collapse both follow-up survey rounds into a single follow-up indicator, for higher power;
# 2. add the baseline values as a column for each individual rather than a row entry;
rsa_mkt_clean <- rsa_mkt_clean %>% mutate(fup_indicator = as.numeric(T_survey_round != 1))
rsa_mkt_baseline <- filter(rsa_mkt_clean, fup_indicator == 0)
rsa_mkt_clean <- filter(rsa_mkt_clean, fup_indicator == 1)

rsa_mkt_baseline <- rsa_mkt_baseline %>%
  rename(Baseline_Profit_z = Profit_zscore, Baseline_Revenue_z = Revenue_zscore) %>%
  select(N_firm_id, Baseline_Profit_z, Baseline_Revenue_z)

rsa_mkt_clean <- rsa_mkt_clean %>%
  inner_join(rsa_mkt_baseline, by = "N_firm_id") %>%
  rename(treatment_indicator = Treatment_MKT, female = Gender) %>%
  mutate(urban = 1, group = "RSA Marketing") %>%
  relocate(female, .after = urban) %>%
  select(
    treatment_indicator,
    Profit_zscore,
    Revenue_zscore,
    Baseline_Profit_z,
    Baseline_Revenue_z,
    female
  )

table(rsa_mkt_clean$treatment_indicator)
View(rsa_mkt_clean)

# export df
write.table(rsa_mkt_clean,
            file = here("cleaned-datasets", "RSA_Market.csv"),
            sep = ",",
            row.names = FALSE)
