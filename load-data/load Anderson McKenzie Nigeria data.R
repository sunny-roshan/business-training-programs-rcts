# load packages
library(dplyr)
library(tidyr)
library(haven)
library(labelled)
library(here)

# set path
getwd()

# load dataset for both follow-ups
nigeria1 <- read_dta(here("raw-datasets", "CleanedFU1.dta"))
nigeria2 <- read_dta(here("raw-datasets", "CleanedFU2.dta"))

table(nigeria1$ftreat) # ftreat = *4* is control not 0, =2 is training
View(nigeria1)

# keep relevant columns
selected_columns <- c(
  "entrep_id",
  "surveyround",
  "ftreat",
  "sales",
  "profits",
  "b_averagemonthsales",
  "b_female"
)
nigeria1 <- nigeria1 %>% select(all_of(selected_columns))
nigeria2 <- nigeria2 %>% select(all_of(selected_columns))

# Check missing data for sales and profits
sum(is.na(nigeria1$sales))  # 83
sum(is.na(nigeria1$profits))  # 83

# What we want to do is:
# drop ftreat2 == 0,1,3, i.e., the observations in the other training arms
# calculate the mean and sd for (profits) and (sales) for the control group (ftreat == 4), separately for each survey wave
# We only need the means and sd of the control group, and so can drop the values for the training group and then drop the column of treatment status:
nigeria1 <- filter(nigeria1, ftreat %in% c(2, 4))
nigeria2 <- filter(nigeria2, ftreat %in% c(2, 4))

standardisation_vars_n1 <- nigeria1 %>%
  group_by(ftreat) %>%
  summarize(
    avgprofits = mean(profits, na.rm = TRUE),
    sdprofits = sd(profits, na.rm = TRUE),
    avgrevenue = mean(sales, na.rm = TRUE),
    sdrevenue = sd(sales, na.rm = TRUE),
    avgbaselinerevenue = mean(b_averagemonthsales, na.rm = TRUE),
    sdbaselinerevenue = sd(b_averagemonthsales, na.rm = TRUE)
  ) %>%
  filter(ftreat == 4) %>%
  select(-ftreat)

standardisation_vars_n2 <- nigeria2 %>%
  group_by(ftreat) %>%
  summarize(
    avgprofits = mean(profits, na.rm = TRUE),
    sdprofits = sd(profits, na.rm = TRUE),
    avgrevenue = mean(sales, na.rm = TRUE),
    sdrevenue = sd(sales, na.rm = TRUE),
    avgbaselinerevenue = mean(b_averagemonthsales, na.rm = TRUE),
    sdbaselinerevenue = sd(b_averagemonthsales, na.rm = TRUE)
  ) %>%
  filter(ftreat == 4) %>%
  select(-ftreat)

# merge the 'Standardisation_vars_N' datasets to the full Nigeria dataset using crossing from tidyr since there are no common variables to join by
nigeria1_clean <- crossing(nigeria1, standardisation_vars_n1)
nigeria2_clean <- crossing(nigeria2, standardisation_vars_n2)

# key transformation: z-scores, standardising the profits in each survey wave by the control mean and control sd for the corresponding survey wave:
nigeria1_clean <- nigeria1_clean %>% mutate(
  Profit_zscore = (profits - avgprofits) / sdprofits,
  Revenue_zscore = (sales - avgrevenue) /
    sdrevenue,
  Baseline_Revenue_z = (b_averagemonthsales - avgbaselinerevenue) /
    sdbaselinerevenue
)

nigeria2_clean <- nigeria2_clean %>% mutate(
  Profit_zscore = (profits - avgprofits) / sdprofits,
  Revenue_zscore = (sales - avgrevenue) /
    sdrevenue,
  Baseline_Revenue_z = (b_averagemonthsales - avgbaselinerevenue) /
    sdbaselinerevenue
)

# define function to clean data and add dummies: make the treatment a (0,1) dummy instead of (2,3); keep only relevant columns (controls won't be the same across datasets so can be dropped).
clean_data <- function(data) {
  data %>%
    mutate(
      treatment_indicator = as.numeric(ftreat == 2),
      urban = 1,
      group = "Nigeria",
      Baseline_Profit_z = NA
    ) %>%
    relocate(b_female, .after = urban) %>%
    rename(female = b_female) %>%
    relocate(Baseline_Profit_z, .after = Revenue_zscore) %>%
    select(
      treatment_indicator,
      Profit_zscore,
      Revenue_zscore,
      Baseline_Revenue_z,
      female,
      urban,
      group,
      Baseline_Profit_z
    )
}
nigeria1_clean <- clean_data(nigeria1_clean)
nigeria2_clean <- clean_data(nigeria2_clean)

table(nigeria1_clean$treatment_indicator)
table(nigeria2_clean$treatment_indicator)


# export df
write.table(nigeria1_clean,
            file = here("cleaned-datasets", "Nigeria1.csv"),
            sep = ",",
            row.names = FALSE)
write.table(nigeria2_clean,
            file = here("cleaned-datasets", "Nigeria2.csv"),
            sep = ",",
            row.names = FALSE)
