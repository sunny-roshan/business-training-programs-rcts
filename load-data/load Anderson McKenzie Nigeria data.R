# load packages
library(dplyr)
library(tidyr)
library(haven)
install.packages("labelled")
library(labelled)

# set path
## set loc

# load dataset for both follow-ups
Nigeria1 <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training Data available/Anderson McKenzie Nigeria/20201154data/ConstructedData/CleanedFU1.dta")
View(Nigeria1)
Nigeria2 <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training Data available/Anderson McKenzie Nigeria/20201154data/ConstructedData/CleanedFU2.dta")
table(Nigeria1$ftreat) # ftreat = *4* is control not 0, =2 is training

# keep relevant columns
selected_columns <- c("entrep_id", "surveyround", "ftreat", "sales", "profits", "b_averagemonthsales", "b_female")
Nigeria1 <- Nigeria1 %>% select(all_of(selected_columns))
Nigeria2 <- Nigeria2 %>% select(all_of(selected_columns))

# Check missing data for profits
sum(is.na(Nigeria1$sales))  #

# What we want to do is: 
# drop ftreat2 == 0,1,3, i.e., the observations in the other training arms
# calculate the mean and sd for (profits) and (sales) for the control group (ftreat == 4), separately for each survey wave
# We only need the means and sd of the control group, and so can drop the values for the training group and then drop the column of treatment status:
Nigeria1 <- filter(Nigeria1, ftreat %in% c(2,4))
Nigeria2 <- filter(Nigeria2, ftreat %in% c(2,4))

Standardisation_vars_N1 <- Nigeria1 %>%
  group_by(ftreat) %>% 
  summarize(avgprofits = mean(profits, na.rm = TRUE), sdprofits = sd(profits, na.rm = TRUE),
            avgrevenue = mean(sales, na.rm = TRUE), sdrevenue = sd(sales, na.rm = TRUE),
            avgbaselinerevenue = mean(b_averagemonthsales, na.rm = TRUE), sdbaselinerevenue = sd(b_averagemonthsales, na.rm = TRUE)) %>%
  filter(ftreat == 4) %>%
  select(-ftreat)

Standardisation_vars_N2 <- Nigeria2 %>% 
  group_by(ftreat) %>% 
  summarize(avgprofits = mean(profits, na.rm = TRUE), sdprofits = sd(profits, na.rm = TRUE),
            avgrevenue = mean(sales, na.rm = TRUE), sdrevenue = sd(sales, na.rm = TRUE),
            avgbaselinerevenue = mean(b_averagemonthsales, na.rm = TRUE), sdbaselinerevenue = sd(b_averagemonthsales, na.rm = TRUE)) %>%
  filter(ftreat == 4) %>%
  select(-ftreat)

# merge the 'Standardisation_vars_N' datasets to the full Nigeria dataset using crossing from tidyr since there are no common variables to join by
Nigeria1_clean <- crossing(Nigeria1, Standardisation_vars_N1)
Nigeria2_clean <- crossing(Nigeria2, Standardisation_vars_N2)

# key transformation: z-scores, standardising the profits in each survey wave by the control mean and control sd for the corresponding survey wave:
Nigeria1_clean <- Nigeria1_clean %>% mutate(Profit_zscore = (profits - avgprofits)/sdprofits,
                                      Revenue_zscore = (sales - avgrevenue)/sdrevenue,
                                      Baseline_Revenue_z = (b_averagemonthsales - avgbaselinerevenue)/sdbaselinerevenue)

Nigeria2_clean <- Nigeria2_clean %>% mutate(Profit_zscore = (profits - avgprofits)/sdprofits,
                                            Revenue_zscore = (sales - avgrevenue)/sdrevenue,
                                            Baseline_Revenue_z = (b_averagemonthsales - avgbaselinerevenue)/sdbaselinerevenue)

# define function to clean data and add dummies: make the treatment a (0,1) dummy instead of (2,3); keep only relevant columns (controls won't be the same across datasets so can be dropped).
clean_data <- function(data) {
  data %>%
    mutate(treatment_indicator = as.numeric(ftreat == 2),
           urban = 1, 
           group = "Nigeria", 
           Baseline_Profit_z = NA) %>%
    relocate(b_female, .after = urban) %>%
    rename(female = b_female) %>%
    relocate(Baseline_Profit_z, .after = Revenue_zscore) %>%
    select(treatment_indicator, Profit_zscore, Revenue_zscore, Baseline_Revenue_z, female, urban, group, Baseline_Profit_z)
}
Nigeria1_clean <- clean_data(Nigeria1_clean)
Nigeria2_clean <- clean_data(Nigeria2_clean)

table(Nigeria2_clean$treatment_indicator)
View(Nigeria1_clean)
View(Nigeria2_clean)

# export df
write.table(Nigeria1_clean, file = "/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Nigeria1.csv", sep=",", row.names=FALSE)
write.table(Nigeria2_clean, file = "/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Nigeria2.csv", sep=",", row.names=FALSE)
