library(dplyr)
library(tidyr)
library(haven)
install.packages("labelled")
library(labelled)

Pakistan <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training Data available/Giné Mansuri Pakistan/dataverse_files/business_visit.dta")
View(Pakistan)

#### There is some discrepancy between the numbers in the paper, the numbers in the business visit dataset
#### (the one which has the sales and profit figures), and the follow-up dataset.

# Let's have a look at treatment status
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
# What's the gender balance? 
table(Pakistan$female) # There are 2359 men and 1876 women

#
# How many missing entries for profits exist?
sum(is.na(Pakistan$log_average_month))
# No sales NAs.

# Now let's convert the sales and profits variables from natural log to real currency, in new vars.
# I know the logs are ln base e not base 10 otherwise the earnings would be in the gazillions
Pakistan <- Pakistan %>% mutate(sales_fup = exp(log_average_month_fu),
                                sales_baseline = exp(log_average_month_bl),
                                profits_fup = exp(logprofit_revised))

# Drop the observations in the lottery group, incl lottery only and training + lottery
Pakistan <- filter(Pakistan, L_ttreat == 0)
# Just checking that we now have a clean control - training treatment distinction. We do.
table(Pakistan$A_EDT)
table(Pakistan$B_LL) 
table(Pakistan$C_EDT_LL) 
table(Pakistan$L_ttreat)

# What we want to do is: calculate the mean and sd of sales and profits only for the control group.

# We're splitting the dataset Pakistan by treatment status, and then generating
# the average and sd of profits and sales, and storing them in a new df:
Standardisation_vars_Pakistan <- group_by(Pakistan, A_EDT) %>% 
  summarize(avgprofits = mean(profits_fup, na.rm = TRUE), sdprofits = sd(profits_fup, na.rm = TRUE),
            avgrevenue = mean(sales_fup, na.rm = TRUE), sdrevenue = sd(sales_fup, na.rm = TRUE),
            avgbaselinerevenue = mean(sales_baseline, na.rm = TRUE), sdbaselinerevenue = sd(sales_baseline, na.rm = TRUE))
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
Pakistan_clean <- Pakistan_clean %>% mutate(Profit_zscore = (profits_fup - avgprofits)/sdprofits,
                                            Revenue_zscore = (sales_fup - avgrevenue)/sdrevenue,
                                            Baseline_Revenue_z = (sales_baseline - avgbaselinerevenue)/sdbaselinerevenue) 

Pakistan_clean <- Pakistan_clean %>% set_variable_labels(Profit_zscore = NULL, Revenue_zscore = NULL,
                                                         Baseline_Revenue_z = NULL)
###
Pakistan_baseline <- filter(Pakistan_clean, post == 0)
Pakistan_clean <- filter(Pakistan_clean, post == 1)
View(Pakistan_baseline)

Pakistan_baseline <- Pakistan_baseline %>% select(pid, Baseline_Revenue_z)

# Now we can drop most columns - they may be used as controls to replicate the regressions, if you want to use
# the Bandiera et al. method; but if we want to combine the datasets' individual observations in the 
# Meager (2019) method, the controls won't be the same across datasets so probably droppable.
Pakistan_clean <- Pakistan_clean %>% inner_join(Pakistan_baseline, by="pid")
Pakistan_clean <- rename(Pakistan_clean, treatment_indicator = A_EDT)
Pakistan_clean <- rename(Pakistan_clean, Baseline_Revenue_z = Baseline_Revenue_z.y)
Pakistan_clean <- Pakistan_clean %>% select(Profit_zscore, Revenue_zscore,
                                      Baseline_Revenue_z, female, treatment_indicator)
Pakistan_clean <- Pakistan_clean %>% mutate(urban = 0, group = "Pakistan")
Pakistan_clean <- Pakistan_clean %>% mutate(Baseline_Profit_z = NA)
Pakistan_clean <- Pakistan_clean %>% relocate(female, .after = urban)
Pakistan_clean <- Pakistan_clean %>% relocate(Baseline_Profit_z, .after = Revenue_zscore)
Pakistan_clean <- Pakistan_clean %>% relocate(treatment_indicator, .before = Profit_zscore)
table(Pakistan_clean$treatment_indicator)
View(Pakistan_clean)

#export df
write.table(Pakistan_clean, file = "/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Pakistan.csv", sep=",", row.names=FALSE)
