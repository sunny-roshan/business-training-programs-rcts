# here we run Bayesian hierarchical models on the combined datasets for the 6 studies for which we have access to all observations

# Load packages
library(baggr)
library(xtable)
library(ggplot2)
library(rstan)
library(stargazer)
library(dplyr)
library(scales)
library(extraDistr)

# Set options
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# Set loc
getwd()

# Load profits df with no NAs
master_noprofna <- read.csv(here("cleaned-datasets", "Profits master dataset.csv"))

# Load sales df with no NAs
master_nosalesna <- read.csv(here("cleaned-datasets", "Sales master dataset.csv"))

# Profits full BHM (this model took over 20 minutes to run)
full_profits <- baggr(
  data = master_noprofna,
  model = "rubin_full",
  iter = 15000,
  chains = 8,
  control = list(adapt_delta = 0.95),
  pooling = "partial",
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  ppd = FALSE,
  pooling_control = "partial",
  covariates = c("urban", "female"),
  outcome = "Profit_zscore",
  group = "group",
  treatment = "treatment_indicator"
)

# Check
pairs(full_profits$fit, pars = c("mu", "theta_k"))

# Profits full BHM (this model took over 20 minutes to run)
full_sales <- baggr(
  data = master_nosalesna,
  model = "rubin_full",
  iter = 15000,
  chains = 8,
  control = list(adapt_delta = 0.95),
  pooling = "partial",
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  prior_control = normal(0.000, 0.071 ^ 2),  # Setting prior for covariates in regression to normal, with SD equal to 10*(highest SD among covariates)
  prior_sigma = uniform(0, 76),
  prior_beta = normal(0.0, 4.9 ^ 2),  # experimental
  covariates = c("urban", "female"),
  outcome = "Revenue_zscore",
  group = "group",
  treatment = "treatment_indicator"
)

# Check
full_sales$fit
