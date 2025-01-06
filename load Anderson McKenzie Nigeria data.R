library(dplyr)
library(tidyr)
library(haven)
install.packages("labelled")
library(labelled)

Nigeria1 <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training Data available/Anderson McKenzie Nigeria/20201154data/ConstructedData/CleanedFU1.dta")
View(Nigeria1)
Nigeria2 <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training Data available/Anderson McKenzie Nigeria/20201154data/ConstructedData/CleanedFU2.dta")
table(Nigeria1$ftreat) # ftreat = *4* is control not 0, =2 is training

Nigeria1 <- Nigeria1 %>% select(entrep_id, surveyround, ftreat,
                                          sales, profits, b_averagemonthsales, b_female)
Nigeria2 <- Nigeria2 %>% select(entrep_id, surveyround, ftreat,
                                sales, profits, b_averagemonthsales, b_female)

# How many missing entries for profits exist?
sum(is.na(Nigeria1$sales))

# What we want to do is: calculate the mean and sd for (profits) and (sales) only for ftreat == 4
# (the control group), separately for each survey wave, and store these.

# Drop ftreat2 == 0,1,3, i.e., the observations in the other training arms
Nigeria1 <- filter(Nigeria1, ftreat %in% c(2,4))
Nigeria2 <- filter(Nigeria2, ftreat %in% c(2,4))

# This might be a slightly long-winded way of doing things, but it seems to work.
# We're splitting the dataset Nigeria by survey wave and treatment status, and then generating
# the average and sd of tprofits and trevenue, and storing them in a new df:
Standardisation_vars_N1 <- group_by(Nigeria1, ftreat) %>% 
  summarize(avgprofits = mean(profits, na.rm = TRUE), sdprofits = sd(profits, na.rm = TRUE),
            avgrevenue = mean(sales, na.rm = TRUE), sdrevenue = sd(sales, na.rm = TRUE),
            avgbaselinerevenue = mean(b_averagemonthsales, na.rm = TRUE), sdbaselinerevenue = sd(b_averagemonthsales, na.rm = TRUE))
View(Standardisation_vars_N1)

Standardisation_vars_N2 <- group_by(Nigeria2, ftreat) %>% 
  summarize(avgprofits = mean(profits, na.rm = TRUE), sdprofits = sd(profits, na.rm = TRUE),
            avgrevenue = mean(sales, na.rm = TRUE), sdrevenue = sd(sales, na.rm = TRUE),
            avgbaselinerevenue = mean(b_averagemonthsales, na.rm = TRUE), sdbaselinerevenue = sd(b_averagemonthsales, na.rm = TRUE))
View(Standardisation_vars_N2)

# We only need the means and sd of the control group, and so can drop the values for the training group
# and then drop the column of treatment status:
Standardisation_vars_N1 <- filter(Standardisation_vars_N1, ftreat == 4)
Standardisation_vars_N2 <- filter(Standardisation_vars_N2, ftreat == 4)
Standardisation_vars_N1 <- Standardisation_vars_N1[-1]
Standardisation_vars_N2 <- Standardisation_vars_N2[-1]
View(Standardisation_vars_N1)
View(Standardisation_vars_N2)

# Now we can merge the 'Standardisation_vars_N' datasets to the full Nigeria dataset using crossing from tidyr 
# since there are no common variables to join by
Nigeria1_clean <- crossing(Nigeria1, Standardisation_vars_N1)
View(Nigeria1_clean)
Nigeria2_clean <- crossing(Nigeria2, Standardisation_vars_N2)
View(Nigeria2_clean)

# Now the key transformation! z-scores, standardising the profits in each survey wave by the 
# control mean and control sd for the corresponding survey wave:
Nigeria1_clean <- Nigeria1_clean %>% mutate(Profit_zscore = (profits - avgprofits)/sdprofits,
                                      Revenue_zscore = (sales - avgrevenue)/sdrevenue,
                                      Baseline_Revenue_z = (b_averagemonthsales - avgbaselinerevenue)/sdbaselinerevenue)

Nigeria2_clean <- Nigeria2_clean %>% mutate(Profit_zscore = (profits - avgprofits)/sdprofits,
                                            Revenue_zscore = (sales - avgrevenue)/sdrevenue,
                                            Baseline_Revenue_z = (b_averagemonthsales - avgbaselinerevenue)/sdbaselinerevenue)

# Now make a couple of cleaner variables.
# Make the treatment a (0,1) dummy instead of (2,3);
# Collapse all 7 follow-up survey waves into a single follow-up indicator, for higher power;
# and add the baseline values as a column for each individual rather than a row entry.
Nigeria1_clean <- Nigeria1_clean %>% mutate(treatment_indicator = as.numeric(ftreat == 2))
Nigeria2_clean <- Nigeria2_clean %>% mutate(treatment_indicator = as.numeric(ftreat == 2))


# Now we can drop most columns - they may be used as controls to replicate the regressions, if you want to use
# the Bandiera et al. method; but if we want to combine the datasets' individual observations in the 
# Meager (2019) method, the controls won't be the same across datasets so probably droppable.
Nigeria1_clean <- Nigeria1_clean %>% select(treatment_indicator,
                                      Profit_zscore, Revenue_zscore,
                                      Baseline_Revenue_z, b_female)
Nigeria2_clean <- Nigeria2_clean %>% select(treatment_indicator,
                                            Profit_zscore, Revenue_zscore,
                                            Baseline_Revenue_z, b_female)
Nigeria1_clean <- Nigeria1_clean %>% mutate(urban = 1)
Nigeria2_clean <- Nigeria2_clean %>% mutate(urban = 1)
Nigeria1_clean <- Nigeria1_clean %>% relocate(b_female, .after = urban)
Nigeria2_clean <- Nigeria2_clean %>% relocate(b_female, .after = urban)
Nigeria1_clean  <- rename(Nigeria1_clean, female = b_female)
Nigeria2_clean  <- rename(Nigeria2_clean, female = b_female)
Nigeria1_clean <- Nigeria1_clean %>% mutate(group = "Nigeria", Baseline_Profit_z = NA)
Nigeria2_clean <- Nigeria2_clean %>% mutate(group = "Nigeria", Baseline_Profit_z = NA)

Nigeria1_clean <- Nigeria1_clean %>% relocate(Baseline_Profit_z, .after = Revenue_zscore)
Nigeria2_clean <- Nigeria2_clean %>% relocate(Baseline_Profit_z, .after = Revenue_zscore)
table(Nigeria2_clean$treatment_indicator)
View(Nigeria1_clean)
View(Nigeria2_clean)

#export df
write.table(Nigeria1_clean, file = "/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Nigeria1.csv", sep=",", row.names=FALSE)
write.table(Nigeria2_clean, file = "/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Nigeria2.csv", sep=",", row.names=FALSE)
