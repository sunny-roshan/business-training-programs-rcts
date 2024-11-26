library(baggr)
vignette('baggr')
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
library(ggplot2)
library(rstan)
library(stargazer)
library(dplyr)
library(scales)
install.packages('extraDistr')
library(extraDistr)

Kenya_clean <- read.csv("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Kenya.csv")
Nigeria1_clean <- read.csv("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Nigeria1.csv")
Nigeria2_clean <- read.csv("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Nigeria2.csv")
Pakistan_clean <- read.csv("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Pakistan.csv")
RSA_Fin_clean <- read.csv("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/RSA_Finance.csv")
RSA_Mkt_clean <- read.csv("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/RSA_Market.csv")
Tanzania_clean <- read.csv("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Tanzania.csv")

## We have 6 datasets/studies. Since Berge reported separate results, we could 
## say it is 7 studies.
Master <- bind_rows(Kenya_clean, Nigeria1_clean, Nigeria2_clean, Pakistan_clean,
                    RSA_Fin_clean, RSA_Mkt_clean, Tanzania_clean)

View(Master)

## Profits 
sum(is.na(Master$Profit_zscore)) # 690
sum(is.na(Master$female)) # 2
sum(is.na(Master$urban)) #0
sum(is.na(Master$Revenue_zscore)) # 1164
Master_noprofna <- Master %>% filter(!is.na(Profit_zscore), !is.na(female))
sum(is.na(Master_noprofna$Profit_zscore))
sum(is.na(Master_noprofna$female))
Master_noprofna <- as.data.frame(Master_noprofna)
View(Master_noprofna)
Master_noprofna <- Master_noprofna %>% select(treatment_indicator, Profit_zscore, urban, female, group)

## Sales
Master_nosalesna <- Master %>% filter(!is.na(Revenue_zscore), !is.na(female), !is.na(urban))
Master_nosalesna <- as.data.frame(Master_nosalesna)
Master_nosalesna <- Master_nosalesna %>% select(treatment_indicator, Revenue_zscore, urban, female, group)
View(Master_nosalesna)

Full <- baggr(data = Master_noprofna, model = "rubin_full",
      iter = 15000, chains = 8, control = list(adapt_delta = 0.95),
      pooling = "partial", prior_hypermean = normal(0,100),
      prior_hypersd = cauchy(0,25), ppd = FALSE, pooling_control = "partial",
      covariates = c("urban", "female"),
      outcome = "Profit_zscore", group = "group",
      treatment = "treatment_indicator")

# this model took over 20 minutes to run!!
## Sales - 
Full_sales <- baggr(data = Master_nosalesna, model = "rubin_full",
              iter = 15000, chains = 8, control = list(adapt_delta = 0.95),
              pooling = "partial", prior_hypermean = normal(0,100),
              prior_hypersd = cauchy(0,25), 
              prior_control = normal(0.000, 0.071^2),
              prior_sigma = uniform(0, 76),
              prior_beta = normal(0.0, 4.9^2),
              covariates = c("urban", "female"),
              outcome = "Revenue_zscore", group = "group",
              treatment = "treatment_indicator")
# Not a reliable model!
# pairs(Full$fit, pars = c("mu", "theta_k"))

Full_sales$fit
#* control [independent prior on control means] ~ normal(0.000, 0.071^2)
#* sigma [error term in linear regresion] ~ uniform(0, 76)
#Setting prior for covariates in regression to normal, with SD equal to 10*(highest SD among covariates):
# * beta ~ normal(0.0, 4.9^2)---purely experimental, we recommend you set this manually
