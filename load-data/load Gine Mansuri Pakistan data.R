# Load libraries
library(dplyr)
library(tidyr)
library(haven)
library(labelled)
library(here)
rm(list=ls())

# Set loc
getwd()

# Load data
pakistan <- read_dta(here("raw-datasets", "business_visit.dta"))
View(pakistan)

# *** NB: There are some discrepancies between the numbers in the paper, the numbers in the business visit dataset (the one which has the sales and profit figures), and the follow-up dataset ***

# Check treatment status
table(pakistan$EDT_treat) # 2381 offered biz training, 1854 were not
table(pakistan$A_EDT) # 1660 were offered training w/o loan lottery - the treatment group we are interested in
table(pakistan$B_LL) # 549 were offered loan lottery only
table(pakistan$C_EDT_LL) # 721 were offered both training and lottery
table(pakistan$L_ttreat) # 1270 got the lottery, 2965 did not
table(pakistan$d_lleligible) # 2580 eligible

# Check gender balance
table(pakistan$female) # There are 2359 men and 1876 women

# Check missing data for profits
sum(is.na(pakistan$log_average_month))  # No missing sales data

# Convert sales and profits from natural log to real currency (I know the logs are ln base e not base 10 otherwise the earnings would be > trillions)
pakistan <- pakistan %>% 
  mutate(
  sales_fup = exp(log_average_month_fu),
  sales_baseline = exp(log_average_month_bl),
  profits_fup = exp(logprofit_revised)
)

# Drop the observations in the lottery group, incl lottery only and training + lottery
pakistan <- filter(pakistan, L_ttreat == 0)

# Checking that we now have a clean control - training treatment distinction
table(pakistan$A_EDT)
table(pakistan$B_LL)
table(pakistan$C_EDT_LL)
table(pakistan$L_ttreat)
# We do have a clean control

# What we want to do is: calculate the mean and sd of sales and profits only for the control group.
# We're splitting the dataset pakistan by treatment status, and then generating
# the average and sd of profits and sales, and storing them in a new df:
standardisation_vars_pakistan <- 
  group_by(pakistan, A_EDT) %>%
  summarize(
    avgprofits = mean(profits_fup, na.rm = TRUE),
    sdprofits = sd(profits_fup, na.rm = TRUE),
    avgrevenue = mean(sales_fup, na.rm = TRUE),
    sdrevenue = sd(sales_fup, na.rm = TRUE),
    avgbaselinerevenue = mean(sales_baseline, na.rm = TRUE),
    sdbaselinerevenue = sd(sales_baseline, na.rm = TRUE)
  )
View(standardisation_vars_pakistan)

# We only need the means and sd of the control group, and so can drop the values for the training group
# and then drop the column of treatment status:
standardisation_vars_pakistan <- filter(standardisation_vars_pakistan, A_EDT == 0)
standardisation_vars_pakistan <- standardisation_vars_pakistan %>% select(-A_EDT)
View(standardisation_vars_pakistan)

# Now we can merge the 'standardisation_vars_pakistan' dataset to the full pakistan dataset using crossing from tidyr
# since there are no common variables to join by
pakistan_clean <- crossing(pakistan, standardisation_vars_pakistan)
View(pakistan_clean)

# Now the key transformation! z-scores, standardising the profits by the control mean and control sd:
pakistan_clean <- pakistan_clean %>% mutate(
  Profit_zscore = (profits_fup - avgprofits) / sdprofits,
  Revenue_zscore = (sales_fup - avgrevenue) /
    sdrevenue,
  Baseline_Revenue_z = (sales_baseline - avgbaselinerevenue) /
    sdbaselinerevenue
)

pakistan_clean <- pakistan_clean %>% set_variable_labels(
  Profit_zscore = NULL,
  Revenue_zscore = NULL,
  Baseline_Revenue_z = NULL
)

# Only retain post-treatment data, but add baseline revenues as a column
pakistan_baseline <- pakistan_clean %>%
  filter(post == 0) %>%
  select(pid, Baseline_Revenue_z)

pakistan_clean <- pakistan_clean %>%
  filter(post == 1) %>%
  inner_join(pakistan_baseline, by = "pid")

# rename variables
pakistan_clean <- pakistan_clean %>% 
  rename(treatment_indicator = A_EDT, 
         Baseline_Revenue_z = Baseline_Revenue_z.y)

# clean dataset, add dummies, drop most variables
pakistan_clean <- pakistan_clean %>%
  mutate(urban = 0,
         group = "Pakistan",
         Baseline_Profit_z = NA) %>%
  relocate(female, .after = urban) %>%
  relocate(Baseline_Profit_z, .after = Revenue_zscore) %>%
  relocate(treatment_indicator, .before = Profit_zscore) %>%
  select(
    Profit_zscore,
    Revenue_zscore,
    Baseline_Revenue_z,
    female,
    treatment_indicator,
    urban,
    group,
    Baseline_Profit_z
  )

table(pakistan_clean$treatment_indicator)

# export df
write.table(pakistan_clean,
            file = here("cleaned-datasets", "pakistan.csv"),
            sep = ",",
            row.names = FALSE)
