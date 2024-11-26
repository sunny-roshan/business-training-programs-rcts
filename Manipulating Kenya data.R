library(dplyr)
library(tidyr)
library(haven)
install.packages("labelled")
library(labelled)

#####
# 1. Kenya: Load Brooks et al. Kenya dataset
Kenya <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training Data available/Brooks Kenya/Data/App2017-0042_data/datasets/BDJ_Dandora_Data.dta")
View(Kenya)
# The dataset has profits for each individual, in the control and the two treatments 
# (training, which we need, and a mentorship policy which we can drop), measured at baseline 
# and in SEVEN subsequent survey waves.

# How many missing entries for profits exist?
sum(is.na(Kenya$tprofits))
# There are 20 profit NAs in 2628 observations. Dropping them:
Kenya <- Kenya %>% filter(!is.na(tprofits))

# What about missing entries for revenue?
sum(is.na(Kenya$trevenue))
# There are 1228 missing entries for revenue! That's nearly half the dataset, so let's not drop
# them. Instead we'll just have a significantly reduced sample size.

# What we want to do is: calculate the mean and sd for (tprofits) and (trevenue) only for treat2 == 2
# (the control group), separately for each survey wave, and store these.

# Drop treat2 == 4, i.e., the observations in the mentorship group
Kenya <- filter(Kenya, treat2 %in% c(2,3))

# This might be a slightly long-winded way of doing things, but it seems to work.
# We're splitting the dataset Kenya by survey wave and treatment status, and then generating
# the average and sd of tprofits and trevenue, and storing them in a new df:
Standardisation_vars <- group_by(Kenya, wave, treat2) %>% 
  summarize(avgprofits = mean(tprofits, na.rm = TRUE), sdprofits = sd(tprofits, na.rm = TRUE),
            avgrevenue = mean(as.numeric(trevenue), na.rm = TRUE), sdrevenue = sd(as.numeric(trevenue), na.rm = TRUE))
View(Standardisation_vars)
# So it appears that revenues were only collected for survey waves 3, 4, 5, 6, 7; not at baseline.
# Hence, getting NaN/NA for avgrevenue and sdrevenue respectively. 

# We only need the means and sd of the control group, and so can drop the values for the training group
# and then drop the column of treatment status:
Standardisation_vars <- filter(Standardisation_vars, treat2 == 2)
Standardisation_vars <- Standardisation_vars[,c(1,3,4,5,6)]
View(Standardisation_vars)

# Now we can merge the 'Standardisation_vars' dataset to the full Kenya dataset using left_join in dplyr
Kenya_clean <- left_join(Kenya, Standardisation_vars, by = "wave")
View(Kenya_clean)

# Now the key transformation! z-scores, standardising the profits in each survey wave by the 
# control mean and control sd for the corresponding survey wave:
Kenya_clean <- Kenya_clean %>% mutate(Profit_zscore = (tprofits - avgprofits)/sdprofits,
                                      Revenue_zscore = (trevenue - avgrevenue)/sdrevenue) 

# For convenience, move the vars we have created next to the tprofits column:
Kenya_clean <- Kenya_clean %>% 
  relocate(c(avgprofits,sdprofits,Profit_zscore,trevenue,avgrevenue,sdrevenue,Revenue_zscore), 
           .after = tprofits)

# Now make a couple of cleaner variables.
# Make the treatment a (0,1) dummy instead of (2,3);
# Collapse all 7 follow-up survey waves into a single follow-up indicator, for higher power;
# and add the baseline values as a column for each individual rather than a row entry.
table(Kenya_clean$treat2)
Kenya_clean <- Kenya_clean %>% mutate(treatment_indicator = as.numeric(treat2 == 3))
Kenya_clean <- Kenya_clean %>% mutate(fup_indicator = as.numeric(wave != 0))

Kenya_baseline <- filter(Kenya_clean, fup_indicator == 0)
Kenya_clean <- filter(Kenya_clean, fup_indicator == 1)

View(Kenya_baseline)
Kenya_baseline <- rename(Kenya_baseline, Baseline_Profit_z = Profit_zscore, Baseline_Revenue_z = Revenue_zscore)
Kenya_baseline <- Kenya_baseline %>% select(id, fup_indicator, Baseline_Profit_z, Baseline_Revenue_z)

# Now we can drop most columns - they may be used as controls to replicate the regressions, if you want to use
# the Bandiera et al. method; but if we want to combine the datasets' individual observations in the 
# Meager (2019) method, the controls won't be the same across datasets so probably droppable.
Kenya_clean <- Kenya_clean %>% inner_join(Kenya_baseline, by="id")
Kenya_clean <- Kenya_clean %>% select(treatment_indicator,
                                      Profit_zscore, Revenue_zscore,
                                      Baseline_Profit_z, Baseline_Revenue_z)
Kenya_clean <- Kenya_clean %>% mutate(urban = 1, female = 1, group = "Kenya")
table(Kenya_clean$treatment_indicator)
View(Kenya_clean)

#export df
write.table(Kenya_clean, file = "/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Kenya.csv", sep=",", row.names=FALSE)