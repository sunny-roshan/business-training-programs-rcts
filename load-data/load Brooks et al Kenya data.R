# load packages
library(dplyr)
library(tidyr)
library(haven)
library(labelled)
library(here)

# set loc
getwd()

# Load Brooks, Donovan and Johnson (2018)'s kenya dataset
# The dataset has profits for each individual, in the control and the two treatments (training, which we need, and a mentorship policy which we can drop), measured at baseline and in SEVEN subsequent survey waves
kenya <- read_dta(here("raw-datasets", "BDJ_Dandora_Data.dta"))

# Check missing data for profits and revenues
sum(is.na(kenya$tprofits))  # 20 profit NAs in 2628 observations
sum(is.na(kenya$trevenue))  # 1228 missing entries for revenue - nearly half the dataset - don't drop, just have reduced sample size

# Drop missing profits only
kenya <- kenya %>% filter(!is.na(tprofits))

# What we want to do is:
# 1. drop treat2 == 4, i.e., the observations in the mentorship group;
# 2. split the dataset by survey wave and treatment status;
# 3. calculate the mean and sd for (tprofits) and (trevenue) only for treat2 == 2 (the control group) separately for each survey wave;.
# 4. we only need control group means and sd, so can drop the values for the training group and then drop treatment status column.
kenya <- filter(kenya, treat2 %in% c(2, 3))
standardisation_vars <- kenya %>%
  group_by(wave, treat2) %>%
  summarize(
    avgprofits = mean(tprofits, na.rm = TRUE),
    sdprofits = sd(tprofits, na.rm = TRUE),
    avgrevenue = mean(as.numeric(trevenue), na.rm = TRUE),
    sdrevenue = sd(as.numeric(trevenue), na.rm = TRUE)
  ) %>%
  filter(treat2 == 2) %>%
  select(wave, avgprofits, sdprofits, avgrevenue, sdrevenue)
View(standardisation_vars)
# It appears that revenues were only collected for survey waves 3, 4, 5, 6, 7; not at baseline.
# Hence, getting NaN/NA for avgrevenue and sdrevenue respectively.

# Now merge the 'standardisation_vars' dataset to the full kenya dataset using left_join
kenya_clean <- left_join(kenya, standardisation_vars, by = "wave")
View(kenya_clean)

# key transformation: standardise profits in each survey wave using the control mean and sd for corresponding survey wave, calculate z-scores, and order z-score cols next to the tprofits column for convenience
kenya_clean <- kenya_clean %>%
  mutate(
    Profit_zscore = (tprofits - avgprofits) / sdprofits,
    Revenue_zscore = (trevenue - avgrevenue) / sdrevenue
  ) %>%
  relocate(
    c(
      avgprofits,
      sdprofits,
      Profit_zscore,
      trevenue,
      avgrevenue,
      sdrevenue,
      Revenue_zscore
    ),
    .after = tprofits
  )

# cleaner variables:
# 1. make the treatment a (0,1) dummy instead of (2,3);
# 2. collapse all 7 follow-up survey waves into a single follow-up indicator, for higher power;
table(kenya_clean$treat2)
kenya_clean <- kenya_clean %>%
  mutate(treatment_indicator = as.numeric(treat2 == 3),
         fup_indicator = as.numeric(wave != 0))

# create baseline subset in order to add baseline values as a column for each individual rather than a row entry
kenya_baseline <- kenya_clean %>%
  filter(fup_indicator == 0) %>%
  rename(Baseline_Profit_z = Profit_zscore, Baseline_Revenue_z = Revenue_zscore) %>%
  select(id, fup_indicator, Baseline_Profit_z, Baseline_Revenue_z)

# add baseline values to follow-up observations, drop most columns (as controls are not the same across studies), create dummy variables
kenya_clean <- kenya_clean %>%
  filter(fup_indicator == 1) %>%
  inner_join(kenya_baseline, by = "id") %>%
  select(
    treatment_indicator,
    Profit_zscore,
    Revenue_zscore,
    Baseline_Profit_z,
    Baseline_Revenue_z
  ) %>%
  mutate(urban = 1,
         female = 1,
         group = "Kenya")

table(kenya_clean$treatment_indicator)

# export df
write.table(kenya_clean,
            file = here("cleaned-datasets", "Kenya.csv"),
            sep = ",",
            row.names = FALSE)
