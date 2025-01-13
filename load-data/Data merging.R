rm(list = ls())
library(haven)
library(tidyr)
Bandiera_BHM <- read.dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Bandiera BHM/R-BuildingEvidence/dataReadOnly/giMetaForR.dta")
View(Bandiera_BHM)

#####
## 1. Brooks, Kenya
Brooks_Kenya <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training/Brooks Kenya/Data/App2017-0042_data/datasets/BDJ_Dandora_Data.dta")
View(Brooks_Kenya)
table(Brooks_Kenya$treat)
table(Brooks_Kenya$treat2)
table(Brooks_Kenya$class)
Brooks_Kenya2<-as.data.frame(Brooks_Kenya)
View(Brooks_Kenya2)
Brooks_Kenya_baseline <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training/Brooks Kenya/Data/App2017-0042_data/datasets/BDJ_Baseline_Data.dta")
View(Brooks_Kenya_baseline)
summary(Brooks_Kenya$tprofits)
str(Brooks_Kenya$tprofits)
df %>% drop_na(tprofits)
df<-tibble(Brooks_Kenya)
df %>% summarise_at(vars(tprofits), list(name = mean))
#### It worked here:
Brooks_Kenya2<-Brooks_Kenya2[!is.na(Brooks_Kenya2$tprofits), ]
aggregate(Brooks_Kenya2$tprofits, by = list(Brooks_Kenya2$wave, Brooks_Kenya2$treat2), FUN = mean)

####
if(Brooks_Kenya2$tprofits != NA){
  aggregate(Brooks_Kenya2$tprofits,by = list(Brooks_Kenya2$wave),  FUN = sd)
}
aggregate(Brooks_Kenya$tprofits, by = list(Brooks_Kenya$wave),  FUN = mean)
Brooks_Kenya %>%
  group_by(wave) %>%
  summarise_at(vars(tprofits), list(name = mean))
##### 
#having a look at Meager's merged and simplified microcredit datasets
test <- data.frame(microcredit)
test2 <- data.frame(microcredit_simplified)
test3 <- as_tibble(microcredit)
test4 <- as_tibble(microcredit_simplified)

library(haven)
SA <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training/Anderson et al South Africa/MS Data Folder -- Pathways to Profits/P2P_dataset.dta")
Tanzania <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training/Berge et al Tanzania/mnsc/DataBBT2014.dta")
Kenya <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training/Brooks Kenya/Data/App2017-0042_data/datasets/BDJ_Dandora_Data.dta")
DomRep <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training/Drexler Dominican Rep no profit/data/Keep-it-Simple-Replication-Files/kisDataFinal.dta")
Pak <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training/Giné Mansuri Pakistan/dataverse_files/merge_bl_fu.dta")
Peru1_Ind <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training/Karlan Valdivia Peru no profit/dataverse_files/BDS_database_individual_REStat.dta")
Nigeria <- read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Data for thesis/Business Training/Anderson McKenzie Nigeria/20201154data/ConstructedData/CleanedFU2.dta")

View(Tanzania)
table(Tanzania$dtreat)

testing<-read_dta("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/Bandiera BHM/R-BuildingEvidence/dataReadOnly/giMetaForR.dta")
View(testing)
