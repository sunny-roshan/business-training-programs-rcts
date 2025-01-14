# load data analysis and Stata-reading packages
library(dplyr)
library(tidyr)
library(haven)
library(labelled)
library(here)

# set loc
getwd()

# load stata dta
tanzania <- read_dta(here("raw-datasets", "DataBBT2014.dta"))
View(tanzania)

# Inspect treatment status, verify figures vs. published paper
table(tanzania$dtreat) # 319 were offered training, incl. training only and training + grant
table(tanzania$bg) # 252 were offered grant, incl. grant only and training + grant
table(tanzania$trainonly) # 193 were offered training only
table(tanzania$bgonly) # 126 were offered grant only
table(tanzania$dtreat_bg) # 126 were offered both training and grant
table(tanzania$trainonly, tanzania$male)
## Treatment status: numbers match what the paper says

# Check missing data for profits
sum(is.na(tanzania$lnisoprofit0911f))  # 42 profit NAs for the average taken over the two follow-up surveys, in 644 observations
sum(is.na(tanzania$lnisoprofit09f))  # 114
sum(is.na(tanzania$lnisoprofit11f))  # 81

# Check missing data for sales?
sum(is.na(tanzania$lnisales09f))  # Same as for profits - 42 NAs in the 'average over 2009 and 2011' variable
sum(is.na(tanzania$lnisales11f))
sum(is.na(tanzania$lnisales0911f))

# Drop missing sales & profits observations
tanzania <- tanzania %>% filter(!is.na(lnisales0911f))

# Convert sales and profits variables from natural log to real currency
# (ogs are ln base e not base 10, else the earnings would be >trillions)
tanzania <- tanzania %>%
  mutate(sales0911 = (exp(lnisales0911f)) / 1000,
         profits0911 = (exp(lnisoprofit0911f)) / 1000)

# Drop the observations in the grant group, incl grant only and training + grant.
tanzania <- filter(tanzania, bg == 0)

# Check that we now have a clean control - training treatment distinction. We do.
table(tanzania$dtreat_bg)
table(tanzania$dtreat)
table(tanzania$trainonly) # After dropping NAs there are 182 in training group and 179 in control.

# The authors reported results separately for males and females; I'm not doing the same
# Check gender dummies and gender balance
table(tanzania$male)  # The 2 gender dummies are just the inverse of each other (as expected)
table(tanzania$female)  # There are 122 men and 239 women

# What we want to do is: calculate the mean and sd of (profits0911) and (sales0911), as well as the baseline values (sales08a) and (soprofit08a), only for the control group.
# Split the dataset by treatment status, generate average and sd of profits and sales, store them in a new df
standardisation_vars_tanzania <- group_by(tanzania, dtreat) %>%
  summarize(
    avgprofits = mean(profits0911),
    sdprofits = sd(profits0911),
    avgrevenue = mean(sales0911),
    sdrevenue = sd(sales0911),
    avgbaselineprofits = mean(soprofit08a),
    sdbaselineprofits = sd(soprofit08a),
    avgbaselinerevenue = mean(sales08a),
    sdbaselinerevenue = sd(sales08a)
  )
View(standardisation_vars_tanzania)

# We only need the means and sd of the control group, and so can drop the values for the training group and then drop the column of treatment status:
standardisation_vars_tanzania <- filter(standardisation_vars_tanzania, dtreat == 0)
standardisation_vars_tanzania <- standardisation_vars_tanzania %>% select(-dtreat)

# merge 'standardisation_vars_tanzania' dataset to the full tanzania dataset using crossing from tidyr  since there are no common variables to join by
tanzania_clean <- crossing(tanzania, standardisation_vars_tanzania)
View(tanzania_clean)

# Now, key transformation: calculate z-scores, standardising the profits by the control mean and control sd:
tanzania_clean <- tanzania_clean %>% mutate(
  Profit_zscore = (profits0911 - avgprofits) / sdprofits,
  Revenue_zscore = (sales0911 - avgrevenue) /
    sdrevenue,
  Baseline_Profit_z = (soprofit08a - avgbaselineprofits) /
    sdbaselineprofits,
  Baseline_Revenue_z = (sales08a - avgbaselinerevenue) /
    sdbaselinerevenue
)

tanzania_clean <- tanzania_clean %>% set_variable_labels(
  profits0911 = NULL,
  sales0911 = NULL,
  Profit_zscore = NULL,
  Revenue_zscore = NULL,
  Baseline_Profit_z = NULL,
  Baseline_Revenue_z = NULL
)

# Drop unnecessary variables - they may be used as controls to replicate the regressions in the Bandiera et al. method; but if we want to combine the datasets' individual observations in the Meager (2019) method, the controls won't be the same across datasets, so probably droppable.
tanzania_clean <- tanzania_clean %>%
  select(
    female,
    dtreat,
    Profit_zscore,
    Revenue_zscore,
    Baseline_Profit_z,
    Baseline_Revenue_z
  ) %>%
  rename(treatment_indicator = dtreat) %>%
  mutate(urban = 1, group = "Tanzania") %>%
  relocate(female, .after = urban)

# export
write.table(tanzania_clean,
            file = here("cleaned-datasets", "Tanzania.csv"),
            sep = ",",
            row.names = FALSE)
