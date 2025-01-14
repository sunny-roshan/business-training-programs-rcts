# Load libraries
library(dplyr)
library(tidyr)
library(haven)
library(labelled)

# Set loc
getwd()

# Load dataset
rsa <- read_dta(here("raw-datasets", "P2P_dataset.dta"))

# Check treatments and treatment statuses
table(rsa$Treatment_FIN)

# Keep only financial treatment and control
rsa_fin <- filter(rsa, Treatment_MKT == 0)

# Check missing data for profits and sales
sum(is.na(rsa_fin$Profits3_composite_w1))  # 279
sum(is.na(rsa_fin$Sales5_composite_w1))  # 279

# Standardise control group profits and sales
standardisation_vars_rsa_fin <- 
  group_by(rsa_fin, T_survey_round, Treatment_FIN) %>%
  summarize(
    avgprofits = mean(Profits3_composite_w1, na.rm = TRUE),
    sdprofits = sd(Profits3_composite_w1, na.rm = TRUE),
    avgrevenue = mean(Sales5_composite_w1, na.rm = TRUE),
    sdrevenue = sd(Sales5_composite_w1, na.rm = TRUE)
  )
View(standardisation_vars_rsa_fin)

# We only need the means and sd of the control group (treatment==0), and so can drop the values for the training group
# and then drop the column of treatment status:
standardisation_vars_rsa_fin <- filter(standardisation_vars_rsa_fin, Treatment_FIN == 0)
standardisation_vars_rsa_fin <- standardisation_vars_rsa_fin %>% select(-Treatment_FIN)

# Now we can merge the 'standardisation_vars_rsa_fin' dataset to the full rsa_fin dataset using left_join in dplyr
rsa_fin_clean <- left_join(rsa_fin, standardisation_vars_rsa_fin, by = "T_survey_round")
View(rsa_fin_clean)

# Now the key transformation! z-scores, standardising the profits in each survey wave by the
# control mean and control sd for the corresponding survey wave:
rsa_fin_clean <- rsa_fin_clean %>% 
  mutate(
  Profit_zscore = (Profits3_composite_w1 - avgprofits) / sdprofits,
  Revenue_zscore = (Sales5_composite_w1 - avgrevenue) /
    sdrevenue
)

# Cleaner variables:
# 1. collapse both follow-up survey rounds into a single follow-up indicator, for higher power;
# 2. add the baseline values as a column for each individual rather than a row entry;
# 3. drop most controls
rsa_fin_clean <- rsa_fin_clean %>% mutate(fup_indicator = as.numeric(T_survey_round != 1))
rsa_fin_baseline <- filter(rsa_fin_clean, fup_indicator == 0)
rsa_fin_clean <- filter(rsa_fin_clean, fup_indicator == 1)

rsa_fin_baseline <- rsa_fin_baseline %>%
  rename(Baseline_Profit_z = Profit_zscore, Baseline_Revenue_z = Revenue_zscore) %>%
  select(N_firm_id, Baseline_Profit_z, Baseline_Revenue_z)

# clean and drop most controls
rsa_fin_clean <- rsa_fin_clean %>%
  inner_join(rsa_fin_baseline, by = "N_firm_id") %>%
  rename(treatment_indicator = Treatment_FIN, female = Gender) %>%
  mutate(urban = 1, group = "RSA Finance") %>%
  relocate(female, .after = urban) %>%
  select(
    treatment_indicator,
    Profit_zscore,
    Revenue_zscore,
    Baseline_Profit_z,
    Baseline_Revenue_z,
    female
  )

table(rsa_fin_clean$treatment_indicator)
View(rsa_fin_clean)

# export df
write.table(rsa_fin_clean,
            file = here("cleaned-datasets", "RSA_finance_2.csv"),
            sep = ",",
            row.names = FALSE)
