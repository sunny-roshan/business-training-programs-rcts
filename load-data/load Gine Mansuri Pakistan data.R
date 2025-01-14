# Load libraries
library(dplyr)
library(tidyr)
library(haven)
install.packages("labelled")
library(labelled)

# Set loc
## Set path

# Load data
Pakistan <- read_dta(
  "/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training Data available/Giné Mansuri Pakistan/dataverse_files/business_visit.dta"
)
View(Pakistan)

# *** NB: There are some discrepancies between the numbers in the paper, the numbers in the business visit dataset (the one which has the sales and profit figures), and the follow-up dataset ***

# Check treatment status
table(Pakistan$EDT_treat) # 2381 offered biz training, 1854 were not
table(Pakistan$A_EDT) # 1660 were offered training w/o loan lottery - the treatment group we are interested in
table(Pakistan$A_EDT, Pakistan$bl_bus_owner)
table(Pakistan$A_EDT, Pakistan$female)
table(Pakistan$B_LL) # 549 were offered loan lottery only
table(Pakistan$C_EDT_LL) # 721 were offered both training and lottery
table(Pakistan$L_ttreat) # 1270 got the lottery, 2965 did not
table(Pakistan$d_lleligible)

table(Pak_baseline$bl_bus_owner)
table(Pak_baseline$female)
table(Pak_baseline$A_EDT)

# Check gender balance
table(Pakistan$female) # There are 2359 men and 1876 women

# Check missing data for profits
sum(is.na(Pakistan$log_average_month))  #
# No missing sales data

# Convert sales and profits from natural log to real currency (I know the logs are ln base e not base 10 otherwise the earnings would be > trillions)
Pakistan <- Pakistan %>% 
  mutate(
  sales_fup = exp(log_average_month_fu),
  sales_baseline = exp(log_average_month_bl),
  profits_fup = exp(logprofit_revised)
)

# Drop the observations in the lottery group, incl lottery only and training + lottery
Pakistan <- filter(Pakistan, L_ttreat == 0)

# Checking that we now have a clean control - training treatment distinction
table(Pakistan$A_EDT)
table(Pakistan$B_LL)
table(Pakistan$C_EDT_LL)
table(Pakistan$L_ttreat)
# We do have a clean control

# What we want to do is: calculate the mean and sd of sales and profits only for the control group.
# We're splitting the dataset Pakistan by treatment status, and then generating
# the average and sd of profits and sales, and storing them in a new df:
Standardisation_vars_Pakistan <- 
  group_by(Pakistan, A_EDT) %>%
  summarize(
    avgprofits = mean(profits_fup, na.rm = TRUE),
    sdprofits = sd(profits_fup, na.rm = TRUE),
    avgrevenue = mean(sales_fup, na.rm = TRUE),
    sdrevenue = sd(sales_fup, na.rm = TRUE),
    avgbaselinerevenue = mean(sales_baseline, na.rm = TRUE),
    sdbaselinerevenue = sd(sales_baseline, na.rm = TRUE)
  )
View(Standardisation_vars_Pakistan)

# We only need the means and sd of the control group, and so can drop the values for the training group
# and then drop the column of treatment status:
Standardisation_vars_Pakistan <- filter(Standardisation_vars_Pakistan, A_EDT == 0)
Standardisation_vars_Pakistan <- Standardisation_vars_Pakistan[-1]
View(Standardisation_vars_Pakistan)

# Now we can merge the 'Standardisation_vars_Pakistan' dataset to the full Pakistan dataset using crossing from tidyr
# since there are no common variables to join by
Pakistan_clean <- crossing(Pakistan, Standardisation_vars_Pakistan)
View(Pakistan_clean)

# Now the key transformation! z-scores, standardising the profits by the control mean and control sd:
Pakistan_clean <- Pakistan_clean %>% mutate(
  Profit_zscore = (profits_fup - avgprofits) / sdprofits,
  Revenue_zscore = (sales_fup - avgrevenue) /
    sdrevenue,
  Baseline_Revenue_z = (sales_baseline - avgbaselinerevenue) /
    sdbaselinerevenue
)

Pakistan_clean <- Pakistan_clean %>% set_variable_labels(
  Profit_zscore = NULL,
  Revenue_zscore = NULL,
  Baseline_Revenue_z = NULL
)

# Only retain post-treatment data, but add baseline revenues as a column
Pakistan_baseline <- Pakistan_clean %>%
  filter(post == 0) %>%
  select(pid, Baseline_Revenue_z)

Pakistan_clean <- filter(Pakistan_clean, post == 1) %>%
  inner_join(Pakistan_baseline, by = "pid")

# rename variables
Pakistan_clean <- Pakistan_clean %>% rename(treatment_indicator = A_EDT) %>% rename(Baseline_Revenue_z = Baseline_Revenue_z.y)

# clean dataset, add dummies, drop most variables
Pakistan_clean <- Pakistan_clean %>%
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

table(Pakistan_clean$treatment_indicator)
View(Pakistan_clean)

# export df
write.table(Pakistan_clean,
            file = "/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Pakistan.csv",
            sep = ",",
            row.names = FALSE)
