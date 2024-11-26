library(dplyr)
library(tidyr)
library(haven)
install.packages("labelled")
library(labelled)

RSA <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training Data available/Anderson et al South Africa/MS Data Folder -- Pathways to Profits/P2P_dataset.dta")
View(RSA)

table(RSA$Treatment_FIN)
table(RSA$Treatment_FIN)

RSA_Fin <- filter(RSA, Treatment_FIN == 0)

sum(is.na(RSA_Fin$Profits3_composite_w1))
sum(is.na(RSA_Fin$Sales5_composite_w1))
sum(is.na(RSA_Fin$Profits3_composite_w1))
sum(is.na(RSA_Fin$Sales5_composite_w1))

##### 
##### Finance treatment now
Standardisation_vars_RSA_Fin <- group_by(RSA_Fin, T_survey_round, Treatment_FIN) %>% 
  summarize(avgprofits = mean(Profits3_composite_w1, na.rm = TRUE), sdprofits = sd(Profits3_composite_w1, na.rm = TRUE),
            avgrevenue = mean(Sales5_composite_w1, na.rm = TRUE), sdrevenue = sd(Sales5_composite_w1, na.rm = TRUE))
View(Standardisation_vars_RSA_Fin)

# We only need the means and sd of the control group, and so can drop the values for the training group
# and then drop the column of treatment status:
Standardisation_vars_RSA_Fin <- filter(Standardisation_vars_RSA_Fin, Treatment_FIN == 0)
Standardisation_vars_RSA_Fin <- Standardisation_vars_RSA_Fin[,c(1,3,4,5,6)]
View(Standardisation_vars_RSA_Fin)

# Now we can merge the 'Standardisation_vars_RSA_Fin' dataset to the full RSA_Fin dataset using left_join in dplyr
RSA_Fin_clean <- left_join(RSA_Fin, Standardisation_vars_RSA_Fin, by = "T_survey_round")
View(RSA_Fin_clean)

# Now the key transformation! z-scores, standardising the profits in each survey wave by the 
# control mean and control sd for the corresponding survey wave:
RSA_Fin_clean <- RSA_Fin_clean %>% mutate(Profit_zscore = (Profits3_composite_w1 - avgprofits)/sdprofits,
                                          Revenue_zscore = (Sales5_composite_w1 - avgrevenue)/sdrevenue)

##
# Now make a cleaner variable:
# Collapse both follow-up survey rounds into a single follow-up indicator, for higher power;
# and add the baseline values as a column for each individual rather than a row entry.
RSA_Fin_clean <- RSA_Fin_clean %>% mutate(fup_indicator = as.numeric(T_survey_round != 1))
RSA_Fin_baseline <- filter(RSA_Fin_clean, fup_indicator == 0)
RSA_Fin_clean <- filter(RSA_Fin_clean, fup_indicator == 1)

View(RSA_Fin_baseline)
RSA_Fin_baseline <- rename(RSA_Fin_baseline, Baseline_Profit_z = Profit_zscore, Baseline_Revenue_z = Revenue_zscore)
RSA_Fin_baseline <- RSA_Fin_baseline %>% select(N_firm_id, Baseline_Profit_z, Baseline_Revenue_z)

# Now we can drop most columns - they may be used as controls to replicate the regressions, if you want to use
# the Bandiera et al. method; but if we want to combine the datasets' individual observations in the 
# Meager (2019) method, the controls won't be the same across datasets so probably droppable.
RSA_Fin_clean <- RSA_Fin_clean %>% inner_join(RSA_Fin_baseline, by="N_firm_id")
RSA_Fin_clean <- rename(RSA_Fin_clean, treatment_indicator = Treatment_FIN, female = Gender)
RSA_Fin_clean <- RSA_Fin_clean %>% select(treatment_indicator,
                                          Profit_zscore, Revenue_zscore,
                                          Baseline_Profit_z, Baseline_Revenue_z,
                                          female)
RSA_Fin_clean <- RSA_Fin_clean %>% mutate(urban = 1, group = "RSA Finance")
RSA_Fin_clean <- RSA_Fin_clean %>% relocate(female, .after = urban)
table(RSA_Fin_clean$treatment_indicator)
View(RSA_Fin_clean)

#export df
write.table(RSA_Fin_clean, file = "/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/RSA_Finance.csv", sep=",", row.names=FALSE)
