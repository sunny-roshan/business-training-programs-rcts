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

# Set loc
##

#Load profits df with no NAs
#Load sales df with no NAs

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
