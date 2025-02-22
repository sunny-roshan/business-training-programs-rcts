# Load libraries
library(baggr)
vignette('baggr')
library(xtable)
library(ggplot2)
library(rstan)
library(stargazer)
library(dplyr)
library(scales)
library(extraDistr)

# Set options
rm(list = ls())
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# Load Sales summary stats dataset which we manually created
getwd()
Sales <- read.csv(here("cleaned-datasets", "Sales summary stats.csv"))

# First, fit models using summary statistics (effect size and CI/se) only. We have these for 18 studies
# Later, fit models with covariates for the studies with public datasets

# Three BHM models using summary stats (without covariates) with different priors for the mean and standard deviation of the effect size:
# 1: normal N(0,100) mean and wide uniform sd
# 2: normal N(0,100) mean and Cauchy(0,25) sd
# 3: narrower normal N(0,75) mean and narrower Cauchy(0,15) sd

# Model 1 - uniform prior sd and default mean
defaultvarpiorlimit <- 10 * sd(tau)
defaultvarpiorlimit
Sales_BHM_1 <- baggr(
  data = Sales,
  model = "rubin",
  pooling = "partial",
  prior_hypermean = normal(0, 100),
  prior_hypersd = uniform(0, 228),
  iter = 20000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
print(Sales_BHM_1)

# Model 2 - Cauchy (0,25) prior sd and default mean
Sales_BHM_2 <- baggr(
  data = Sales,
  model = "rubin",
  pooling = "partial",
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  iter = 20000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
print(Sales_BHM_2)

# Model 3 - Cauchy (0,15) prior sd and N(0,75) mean
Sales_BHM_3 <- baggr(
  data = Sales,
  model = "rubin",
  pooling = "partial",
  prior_hypermean = normal(0, 75),
  prior_hypersd = cauchy(0, 15),
  iter = 20000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
print(Sales_BHM_3)

# extracting the BHM to Latex: the stan fit object contains the hypermean, hypervar, and shrunk theta parameters for each iteration, as well as the summary figures, which are what we want
sales_fit_1_summary <- summary(Sales_BHM_1$fit)
print(names(sales_fit_1_summary))
print(sales_fit_1_summary$summary)

# the summary contains the parameter 'eta'; it also contains some percentiles and stats for the mean, var, and thetas; we can subset to get only the rows and columns that we want
sales_output_matrix_unif <- summary(Sales_BHM_1$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1, 3, 4, 5, 7, 8, 10)]
print(sales_output_matrix_unif)

sales_output_matrix_cauchy1 <- summary(Sales_BHM_2$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1, 3, 4, 5, 7, 8, 10)]
print(sales_output_matrix_cauchy1)

sales_output_matrix_cauchynarrow <- summary(Sales_BHM_3$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1, 3, 4, 5, 7, 8, 10)]
print(sales_output_matrix_cauchynarrow)

# convert the stan output matrices to dataframes
sales_output_matrix_unif <- as.data.frame(sales_output_matrix_unif)
sales_output_matrix_cauchy1 <- as.data.frame(sales_output_matrix_cauchy1)
sales_output_matrix_cauchynarrow <- as.data.frame(sales_output_matrix_cauchynarrow)

# change the rownames from mu, tau, theta[1], etc. to the study names
tab_lab <- c("Hypermean (tau)", "Hyper-sd (sigma_tau)", Sales$group_s)
rownames(sales_output_matrix_unif) <- tab_lab
rownames(sales_output_matrix_cauchy1) <- tab_lab
rownames(sales_output_matrix_cauchynarrow) <- tab_lab
sales_output_matrix_cauchy1

# extract sales_output_matrix_(1,2,3) to Latex tables
setwd(here("bayesian-models", "outputs"))

sink("sales_all18_BHM_unif.txt")
print.xtable(xtable(sales_output_matrix_unif))
sink()

sink("sales_all18_BHM_Cauchy1.txt")
print.xtable(xtable(sales_output_matrix_cauchy1))
sink()

sink("sales_all18_BHM_Cauchy_narrow.txt")
print.xtable(xtable(sales_output_matrix_cauchynarrow))
sink()


# Pooling and hetero stats
# The pooling factor equals (1 - lambda) from Rubin (1981), where lambda is the between-group heterogeneity (in tau^2, the hyper-var) relative to total variation incl. sampling se. So the pooling factor here is the sampling variation relative to total variation incl. true heterogeneity.

pooling(Sales_BHM_2) # gives the pooling factors for each group separately, based on own s.e.'s
heterogeneity(Sales_BHM_2) # avg pooling factor measured as explained above

# I^2 is the 'lambda' pooling factor we're interested in: the between-group hetero (in tau^2, the hyper-var)
# relative to total variation incl. sampling se.
# higher the I^2 stat (closer to 1), greater the true hetero.
pf_unif <- pooling(Sales_BHM_1, type = "total", metric = "isq")
pf_cauchy1 <- pooling(Sales_BHM_2, type = "total", metric = "isq")
pf_cauchy2 <- pooling(Sales_BHM_3, type = "total", metric = "isq")

# make a table of the pooling factors' percentiles, and extract Latex output
avg_pool_factors <- rbind(pf_unif, pf_cauchy1, pf_cauchy2)
colnames(avg_pool_factors) <- c("2.5%", "Mean", "97.5%")
rownames(avg_pool_factors) <- c("Uniform Prior", "Cauchy(0,25)", "Cauchy(0,15)")
avg_pool_factors

sink("sales_heterogenity_poolfactor.txt")
print.xtable(xtable(avg_pool_factors))
sink()

## Graphical output ---
# (forest) plots of posterior effects, ordered by effect size, with hypermean added
## Uniform prior
pdf(file = "Posterior intervals unif ordered.pdf",
    width = 8,
    height = 4)
plot(Sales_BHM_1, order = TRUE, hyper = TRUE) +
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

## Cauchy(0,25) prior
pdf(file = "Posterior intervals cauchy1 ordered.pdf",
    width = 8,
    height = 4)
plot(Sales_BHM_2, order = TRUE, hyper = TRUE) +
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

## Cauchy(0,15) and N(0,75) prior
pdf(file = "Posterior intervals cauchy2 ordered.pdf",
    width = 8,
    height = 4)
plot(Sales_BHM_3, order = TRUE, hyper = TRUE) +
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

# With the actual posterior distributions:
## Uniform prior
pdf(file = "Posterior distributions unif ordered.pdf",
    width = 8,
    height = 6.5)
plot(Sales_BHM_1,
     style = "areas",
     hyper = TRUE,
     order = TRUE) +
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

## Cauchy(0,25) prior
pdf(file = "Posterior distributions cauchy1 ordered.pdf",
    width = 8,
    height = 6.5)
plot(Sales_BHM_2,
     style = "areas",
     hyper = TRUE,
     order = TRUE) +
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

## Cauchy(0,15) and N(0,75) prior
pdf(file = "Posterior distributions cauchy2 ordered.pdf",
    width = 8,
    height = 6.5)
plot(Sales_BHM_3,
     style = "areas",
     hyper = TRUE,
     order = TRUE) +
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

# forest plot with "both" shows the individual estimates and BHM shrunk estimates
## Uniform
pdf(file = "Original and posteriors unif forest.pdf",
    width = 8,
    height = 5)
forest_plot(Sales_BHM_1, show = "both")
dev.off()

## Cauchy(0,25)
pdf(file = "Original and posteriors cauchy1 forest.pdf",
    width = 8,
    height = 5)
forest_plot(Sales_BHM_2, show = "both")
dev.off()

## Cauchy(0,15) and N(0,75) prior
pdf(file = "Original and posteriors cauchy2 forest.pdf",
    width = 8,
    height = 5)
forest_plot(Sales_BHM_3, show = "both")
dev.off()

# plot of posterior mu distribution: this is the predictive distribution for extrapolation
## Uniform
pdf(file = "Posterior hypermean treatment distribution unif.pdf",
    width = 6,
    height = 6)
effect_plot(Sales_BHM_1) +
  coord_cartesian(xlim = c(-10, 30)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

## Cauchy(0,25)
pdf(file = "Posterior hypermean treatment distribution cauchy1.pdf",
    width = 6,
    height = 6)
effect_plot(Sales_BHM_2) +
  coord_cartesian(xlim = c(-10, 30)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

## Cauchy(0,25) and N(0,75)
pdf(file = "Posterior hypermean treatment distribution cauchy2.pdf",
    width = 6,
    height = 6)
effect_plot(Sales_BHM_3) +
  coord_cartesian(xlim = c(-10, 30)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

# random draws from posterior tau distribution
effect_draw(Sales_BHM_2, n = 20)

# First comparison: comparing Bayesian FE, no-pooling, BHM with Cauchy prior (= Sales_BHM_2) ----
# For Bayesian full pooling and no-pooling there is no prior variance specification.
fullpoolcomparo <- baggr(
  Sales,
  model = "rubin",
  pooling = "full",
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  iter = 20000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)

fullpoolcomparo
nopoolcomparo <- baggr(
  Sales,
  model = "rubin",
  pooling = "none",
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  iter = 20000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
nopoolcomparo

BHM_compare <- baggr_compare(
  "No pooling model" = nopoolcomparo,
  "Full pooling model" = fullpoolcomparo,
  "Partial pooling BHM" = Sales_BHM_2,
  what = "pooling"
)

BHM_compare
summary(BHM_compare)

BHM_compare_df <- rbind(BHM_compare$mean_trt,
                        BHM_compare$sd_trt,
                        BHM_compare$posteriorpd_trt)

# drop the no-pooling since that means there are no aggregate estimates:
BHM_compare_df <- BHM_compare_df[c(2, 3, 5, 6, 8, 9), ]
BHM_compare_df

rownames(BHM_compare_df) <- c(
  "Partial Pooling: Mean Effect",
  "Full Pooling FE: Mean Effect",
  "Partial Pooling: Hyper-SD (tau)",
  "Full Pooling FE: Hyper-SD (tau)",
  "Partial Pooling: Posterior Predictive Effect",
  "Full Pooling FE: Posterior Predictive Effect"
)
BHM_compare_df

pdf(file = "Treatment effect distribution - Compare pooling assmptions.pdf",
    width = 8,
    height = 6)
plot(BHM_compare, order = TRUE) + ggtitle("Sales: Comparison of pooling assumptions") +
  theme(plot.title = element_text(hjust = 0.35)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

## Second comparison - compare priors, not pooling assumptions ----
# Compare BHM with U(0,10*sd(theta)) prior sd to Cauchy (0,25) and Cauchy (0,15) prior sd + N(0,75) prior mean
BHM_compare_2 <- baggr_compare(
  "U[0,228]" = Sales_BHM_1,
  "Cauchy (0,25)" = Sales_BHM_2,
  "Cauchy (0,15) + N(0,75)" = Sales_BHM_3
)
BHM_compare_2
summary(BHM_compare_2)

BHM_compare_2_df <- rbind(BHM_compare_2$mean_trt,
                          BHM_compare_2$sd_trt,
                          BHM_compare_2$posteriorpd_trt)

rownames(BHM_compare_2_df) <- c(
  "U[0,228]: Mean",
  "Cauchy (0,25): Mean",
  "Cauchy (0,15), N(0,75): Mean",
  "U[0,228]: Hyper-SD (tau)",
  "Cauchy (0,25): Hyper-SD (tau)",
  "Cauchy (0,15), N(0,75): Hyper-SD (tau)",
  "U[0,228]: Posterior Predictive Effect",
  "Cauchy (0,25): Posterior Predictive Effect",
  "Cauchy (0,15), N(0,75): Posterior Predictive Effect"
)
BHM_compare_2_df

sink("Comparison of 3 priors Sales BHM.txt")
print.xtable(xtable(BHM_compare_2_df))
sink()

# Compare credible intervals:
pdf(file = "Treatment effect distribution - Compare prior assumptions.pdf",
    width = 8,
    height = 7)
plot(BHM_compare_2, order = TRUE) + ggtitle("Sales: model priors comparison") +
  theme(plot.title = element_text(hjust = 0.35)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

# compare the posterior hyper-distribution of the effect size
# note that these plots will look different if re-run!
pdf(file = "Posterior hyper-effect distribution - Compare prior assumptions.pdf",
    width = 8,
    height = 6)
effect_plot(
  "U[0,228]" = Sales_BHM_1,
  "Cauchy (0,25)" = Sales_BHM_2,
  "Cauchy (0,15) and N(0,75)" = Sales_BHM_3
) +
  coord_cartesian(xlim = c(-10, 30), ) + theme(legend.position = "top") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

# Cross-validation - how leaving out each group affects the output; how the model predicts the left-out groups
BHM_crossval <- loocv(
  Sales,
  model = "rubin",
  pooling = "partial",
  return_models = TRUE,
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  iter = 20000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
BHM_crossval
summary(BHM_crossval)

# We can extract the hyper-mean and heterogeneity BHM estimates, with the Cauchy(0,25) prior,
# from the leave-one-out cross-validation. There is variation in the estimated treatment
# effect and hetero, but it appears reasonable:
# Change LPD to the L-O-O info criterion = -2 * mean log predictive density, which should be close to 0:
looic <- -2 * BHM_crossval$df$lpd
mean(looic) # The mean is 8.89.
Sales_crossval_output <- cbind(group, BHM_crossval$df[, c(1, 2)], looic)
colnames(Sales_crossval_output) <- c("Left-out study",
                                     "Hypermean (mu)",
                                     "Hyper-SD (tau)",
                                     "LOO Info Criterion")
Sales_crossval_output

# extract the cross-validation table to Latex
sink("Sales_CauchyBHM_LOOCV.txt")
print.xtable(xtable(Sales_crossval_output))
sink()

# comparing LOOIC/ELPD for full pooling vs BHM partial pooling - not extracted:
full_pool_crossval <- loocv(
  data = Sales,
  model = "rubin",
  pooling = "full",
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25)
)
full_pool_looic <- -2 * full_pool_crossval$df$lpd
mean(full_pool_looic) # 8.77.
# compare this mean to the partial pooling BHM
mean(full_pool_looic) - mean(looic) # Lower than BHM: -0.1107446.

# With covariates
Sales_expanded <- read.csv(here("cleaned-datasets", "Sales stats with covariates.csv"))

Sales_femalescontrol <- baggr(
  Sales_expanded,
  outcome = "outcome",
  group = "group",
  pooling = "partial",
  model = "rubin",
  covariates = c("Female_proportion"),
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  prior_beta = normal(0, 100),
  iter = 15000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
print(Sales_femalescontrol)

Sales_gdpcontrol <- baggr(
  Sales_expanded,
  outcome = "outcome",
  group = "group",
  pooling = "partial",
  model = "rubin",
  covariates = c("GDP_pc_growth"),
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  prior_beta = normal(0, 100),
  iter = 15000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
print(Sales_gdpcontrol)

# To use Urban proportion as a fixed effect we need to drop Bakhtiar et al. and Chong and Velez.

Sales_expanded_minus1 <- na.omit(Sales_expanded)
View(Sales_expanded_minus1)

Sales_urbancontrol <- baggr(
  Sales_expanded_minus1,
  outcome = "outcome",
  group = "group",
  pooling = "partial",
  model = "rubin",
  covariates = c("Urban_proportion"),
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  prior_beta = normal(0, 100),
  iter = 15000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
print(Sales_urbancontrol)

Sales_3controls <- baggr(
  Sales_expanded_minus1,
  outcome = "outcome",
  group = "group",
  pooling = "partial",
  model = "rubin",
  covariates = c("Female_proportion", "GDP_pc_growth", "Urban_proportion"),
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  prior_beta = normal(0, 100),
  iter = 15000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
print(Sales_3controls)

## Now to extract output from the covariate-augmented models
## Starting with the Latex tables of posteriors

Sales_output_matrix_female <- summary(Sales_femalescontrol$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1, 3, 4, 5, 7, 8, 10)]
print(Sales_output_matrix_female)

Sales_output_matrix_gdp <- summary(Sales_gdpcontrol$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1, 3, 4, 5, 7, 8, 10)]
print(Sales_output_matrix_gdp)

Sales_output_matrix_urban <- summary(Sales_urbancontrol$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1, 3, 4, 5, 7, 8, 10)]
print(Sales_output_matrix_urban)

Sales_output_matrix_3controls <- summary(Sales_3controls$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1, 3, 4, 5, 7, 8, 10)]
print(Sales_output_matrix_3controls)

# convert this stan output matrix to a dataframe
Sales_output_matrix_female <- as.data.frame(Sales_output_matrix_female)
Sales_output_matrix_female

Sales_output_matrix_urban <- as.data.frame(Sales_output_matrix_urban)
Sales_output_matrix_urban

Sales_output_matrix_gdp <- as.data.frame(Sales_output_matrix_gdp)
Sales_output_matrix_gdp

Sales_output_matrix_3controls <- as.data.frame(Sales_output_matrix_3controls)
Sales_output_matrix_3controls

# change the rownames from mu, tau, tau[1], etc. to the study names
group_3control <- group[-(6:7)]
tab_lab_urban <- c("Hypermean (mu)", "Hyper-sd (tau)", group_3control)
rownames(Sales_output_matrix_female) <- tab_lab
rownames(Sales_output_matrix_gdp) <- tab_lab
rownames(Sales_output_matrix_urban) <- tab_lab_urban
rownames(Sales_output_matrix_3controls) <- tab_lab_urban


# extract Sales_output_matrix_female, Sales_output_matrix_gdp and Sales_output_matrix_urban to Latex tables
sink("Sales_female_BHM.txt")
print.xtable(xtable(Sales_output_matrix_female))
sink()

sink("Sales_gdp_BHM.txt")
print.xtable(xtable(Sales_output_matrix_gdp))
sink()

sink("Sales_urban_BHM.txt")
print.xtable(xtable(Sales_output_matrix_urban))
sink()

sink("Sales_3controls_BHM.txt")
print.xtable(xtable(Sales_output_matrix_3controls))
sink()

## Pooling stat - I^2 (=lambda from your methodology)
pf_female <- pooling(Sales_femalescontrol, type = "total", metric = "isq")
pf_gdp <- pooling(Sales_gdpcontrol, type = "total", metric = "isq")
pf_urban <- pooling(Sales_urbancontrol, type = "total", metric = "isq")

# make a table of the pooling factors' percentiles, and extract Latex output
avg_pool_factors_covariates <- rbind(pf_female, pf_gdp, pf_urban)
colnames(avg_pool_factors_covariates) <- c("2.5%", "Mean", "97.5%")
rownames(avg_pool_factors_covariates) <- c("Gender indicator", "GDP pc growth %", "% Urban")
avg_pool_factors_covariates

sink("Sales_heterogeneity_poolfactor_covariates.txt")
print.xtable(xtable(avg_pool_factors_covariates))
sink()

## Graphical output ----
forest_plot(Sales_femalescontrol, show = "both")

plot(Sales_3controls,
     style = "areas",
     hyper = TRUE,
     order = TRUE) +
  ggtitle("Posterior treatment distributions") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(family = "Arial", size = 14))

pdf(file = "Posterior hyper-effect distribution - controls or no.pdf",
    width = 8,
    height = 6)
effect_plot("Cauchy (0,25)" = Sales_BHM_2, "3 controls" = Sales_3controls) +
  coord_cartesian(xlim = c(-30, 100), ) + theme(legend.position = "top") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(family = "serif", size = 14))
dev.off()

# Cross val
BHM_3controls_crossval <- loocv(
  Sales_expanded_minus1,
  outcome = "outcome",
  group = "group",
  pooling = "partial",
  model = "rubin",
  covariates = c("Female_proportion", "GDP_pc_growth", "Urban_proportion"),
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  prior_beta = normal(0, 100),
  iter = 15000,
  chains = 8,
  control = list(adapt_delta = 0.95),
  return_models = TRUE
)
BHM_3controls_crossval

# We can extract the hyper-mean and heterogeneity BHM estimates
# from the leave-one-out cross-validation.
# Change LPD to the L-O-O info criterion = -2 * mean log predictive density, which should be close to 0:
looic_3controls <- -2 * BHM_3controls_crossval$df$lpd
mean(looic_3controls) # The mean is 8.25. Ref. Gelman TB to see how this looks.
Sales_3controls_crossval_output <- cbind(group_3control, BHM_3controls_crossval$df[, c(1, 2)], looic_3controls)
colnames(Sales_3controls_crossval_output) <- c("Left-out study",
                                               "Hypermean (mu)",
                                               "Hyper-SD (tau)",
                                               "LOO Info Criterion")
Sales_3controls_crossval_output

# extract the cross-validation table to Latex
sink("Sales_3controls_LOOCV.txt")
print.xtable(xtable(Sales_3controls_crossval_output))
sink()

## Now with only the datasets we have full access to ----
Sales_Data <- filter(
  Sales,
  group == "Gine and Mansuri 2020" |
    group == "Berge et al. 2015 females" |
    group == "Berge et al. 2015 males" |
    group == "Anderson et al. 2018 finance" |
    group == "Anderson et al. 2018 marketing" |
    group == "Brooks et al. 2018" |
    group == "Campos et al. 2017 traditional" |
    group == "Anderson and McKenzie 2020"
)
Sales_Data

# model 1 - default priors, only summary data, no covariates; but adding debugging  bc otherwise MCMC problematic
defaultvarpiorlimit_sales_Data8 <- 10 * sd(Sales_Data$tau)
defaultvarpiorlimit_sales_Data8

Sales_BHM_1_Data8 <- baggr(
  Sales_Data,
  model = "rubin",
  pooling = "partial",
  prior_hypermean = normal(0, 100),
  prior_hypersd = uniform(0, defaultvarpiorlimit_sales_Data8),
  iter = 20000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
print(Sales_BHM_1_Data8)

# model 2 - Cauchy prior for variance
Sales_BHM_2_Data8 <- baggr(
  Sales_Data,
  model = "rubin",
  pooling = "partial",
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  iter = 20000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
print(Sales_BHM_2_Data8)

# extracting the BHM model to Latex
sales_fit_1_Data8 <- Sales_BHM_1_Data8$fit
sales_fit_2_Data8 <- Sales_BHM_2_Data8$fit

# the stan fit object contains the hypermean, hypervar, and shrunk theta parameters for each iteration, as well as the summary figures, which are what we wan
sales_fit_1_Data8_summary <- summary(sales_fit_1_Data8)
sales_fit_2_Data8_summary <- summary(sales_fit_2_Data8)

print(sales_fit_1_Data8_summary$summary)
print(sales_fit_2_Data8_summary$summary)

# the summary contains the parameter 'eta', which I don't understand and have not been able to find info about online;
# it also contains some percentiles and stats for the mean, var, and thetas;
# we can subset to get only the rows and columns that we want
sales_output_matrix_1_Data8 <- summary(sales_fit_1_Data8, pars = c("mu", "tau", "theta_k"))$summary[, c(1, 3, 4, 5, 7, 8)]
sales_output_matrix_2_Data8 <- summary(sales_fit_2_Data8, pars = c("mu", "tau", "theta_k"))$summary[, c(1, 3, 4, 5, 7, 8)]
print(sales_output_matrix_2_Data8)

# convert this stan output matrix to a dataframe
sales_df_1_Data8 <- as.data.frame(sales_output_matrix_1_Data8)
sales_df_2_Data8 <- as.data.frame(sales_output_matrix_2_Data8)
sales_df_1_Data8
sales_df_2_Data8

# change the rownames from mu, tau, theta[1], etc. to the study names
tab_lab_Data8 <- c("Hypermean (tau)", "Hyper-sd (sigma_tau)", Sales_Data$group)
tab_lab_Data8
rownames(sales_df_1_Data8) <- tab_lab_Data8
rownames(sales_df_2_Data8) <- tab_lab_Data8
sales_df_1_Data8
sales_df_2_Data8

# extract sales_df_1_Data8 and sales_df_2_Data8 to Latex tables
sink("Sales_Data8_BHM_Default.txt")
print.xtable(xtable(sales_df_1_Data8))
sink()

sink("Sales_Data8_BHM_Cauchy.txt")
print.xtable(xtable(sales_df_2_Data8))
sink()

# Pooling and hetero stats ----
# note that the pooling factor equals (1 - lambda) from Rubin (1981) - between-group hetero (in sigma_tau) relative to total variation incl. sampling se

# aside - pooling stats in the stanfit object
pooling_Sales_1_Data8 <- Sales_BHM_1_Data8$pooling_metric
pooling_Sales_2_Data8 <- Sales_BHM_2_Data8$pooling_metric
poolfactors_sales_1_Data8 <- as.data.frame(pooling_Sales_1_Data8)
poolfactors_sales_2_Data8 <- as.data.frame(pooling_Sales_2_Data8)

pooling(Sales_BHM_2_Data8) # gives the pooling factors for each group separately, based on own s.e.'s
avg_lambda_Sales_Data8 <- heterogeneity(Sales_BHM_2_Data8) # avg pooling factor
avg_Isq_Sales_Data8 <- pooling(Sales_BHM_2_Data8, type = "total", metric = "isq") # I^2, measure of hetero, = (1 - pooling factor)

# make a table of the heterogeneity and pooling factors' percentiles, and extract Latex output
hetero_table_Sales_Data8 <- rbind(avg_lambda_Sales_Data8, avg_Isq_Sales_Data8)
colnames(hetero_table_Sales_Data8) <- c("2.5%", "Mean", "97.5%")
rownames(hetero_table_Sales_Data8) <- c("Average Pooling", "Average I^2")
hetero_table_Sales_Data8

sink("sales_hetero_Data8_CauchyBHM.txt")
print.xtable(xtable(hetero_table_Sales_Data8))
sink()

## Graphical output ----
# (forest) plots of posterior effects
pdf(file = "Posterior intervals Data 8.pdf",
    width = 8,
    height = 4)
plot(Sales_BHM_2_Data8, order = FALSE) +
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
dev.off()

# ordered by effect size, with hypermean added
pdf(file = "Posterior intervals ordered Data 8.pdf",
    width = 8,
    height = 4)
plot(Sales_BHM_2_Data8, order = TRUE, hyper = TRUE) +
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
dev.off()

# with the actual posterior distributions:
pdf(file = "Posterior distributions ordered Data 8.pdf",
    width = 8,
    height = 6)
plot(Sales_BHM_2_Data8,
     style = "areas",
     hyper = TRUE,
     order = TRUE) +
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
dev.off()

# "both" shows the individual estimates and BHM shrunk estimates
# doesn't seem possible to order a forest plot by effect size
pdf(file = "Original and posteriors Data 8.pdf",
    width = 8,
    height = 4)
forest_plot(Sales_BHM_2_Data8, order = TRUE, show = "both")
dev.off()

# plot of posterior tau distribution: this is the predictive distribution for extrapolation
pdf(file = "Posterior treatment distribution Data 8.pdf",
    width = 6,
    height = 6)
effect_plot(Sales_BHM_2_Data8) +
  coord_cartesian(xlim = c(-10, 30)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

# random draws from posterior tau distribution
effect_draw(Sales_BHM_2_Data8, n = 20)

## First comparison: comparing Bayesian FE, no-pooling, BHM with Cauchy prior (= Sales_BHM_2_Data8) ----
# For Bayesian full pooling and no-pooling there is no prior variance specification.
fullpoolcomparo_Data8 <- baggr(
  Sales_Data,
  model = "rubin",
  pooling = "full",
  prior_hypermean = normal(0, 100),
  iter = 20000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
fullpoolcomparo_Data8

nopoolcomparo_Data8 <- baggr(
  Sales_Data,
  model = "rubin",
  pooling = "none",
  prior_hypermean = normal(0, 100),
  iter = 20000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
nopoolcomparo_Data8

BHM_compare_sales_Data8 <- baggr_compare(
  "No pooling model" = nopoolcomparo_Data8,
  "Full pooling model" = fullpoolcomparo_Data8,
  "Partial pooling BHM" = Sales_BHM_2_Data8,
  what = "pooling"
)
BHM_compare_sales_Data8
summary(BHM_compare_sales_Data8)

BHM_compare_Sales_Data8_df <- rbind(
  BHM_compare_sales_Data8$mean_trt,
  BHM_compare_sales_Data8$sd_trt,
  BHM_compare_sales_Data8$posteriorpd_trt
)

# drop the no-pooling since that means there are no aggregate estimates:
BHM_compare_Sales_Data8_df <- BHM_compare_Sales_Data8_df[c(2, 3, 5, 6, 8, 9), ]
BHM_compare_Sales_Data8_df
rownames(BHM_compare_Sales_Data8_df) <- c(
  "Partial Pooling: Mean",
  "Full Pooling FE: Mean",
  "Partial Pooling: SD",
  "Full Pooling FE: SD",
  "Partial Pooling: Posterior Predictive Effect",
  "Full Pooling FE: Posterior Predictive Effect"
)
BHM_compare_Sales_Data8_df

# export this comparison table to Latex; the more interesting comparison is in graphical form
sink("Comparison of BHM and Full Pooling Sales BHM Data 8.txt")
print.xtable(xtable(BHM_compare_Sales_Data8_df))
sink()

# plot of comparing pooling assumptions - unable to change the x-axis increments
pdf(file = "Treatment effect distribution - Compare pooling assmptions - Data 8.pdf",
    width = 8,
    height = 6)
plot(BHM_compare_sales_Data8) + ggtitle("8 sites: Comparison of pooling assumptions") +
  theme(plot.title = element_text(hjust = 0.35))
dev.off()

# Second comparison - compare priors, not pooling assumptions ----
# Compare BHM with default priors to Cauchy variance prior and longer chains/iterations
BHM_compare_2_sales_Data8 <- baggr_compare("Default priors" = Sales_BHM_1_Data8,
                                           "Cauchy variance prior" = Sales_BHM_2_Data8)
BHM_compare_2_sales_Data8
summary(BHM_compare_2_sales_Data8)

BHM_compare_2_sales_Data8_df <- rbind(
  BHM_compare_2_sales_Data8$mean_trt,
  BHM_compare_2_sales_Data8$sd_trt,
  BHM_compare_2_sales_Data8$posteriorpd_trt
)

rownames(BHM_compare_2_sales_Data8_df) <- c(
  "Default Model: Mean",
  "Cauchy Prior: Mean",
  "Default Model: SD",
  "Cauchy Prior: SD",
  "Default Model: Posterior Predictive Effect",
  "Cauchy Prior: Posterior Predictive Effect"
)
BHM_compare_2_sales_Data8_df

sink("Comparison of Default and Cauchy Sales BHM Data 8.txt")
print.xtable(xtable(BHM_compare_2_sales_Data8_df))
sink()

# Plot of the prior comparison - Cauchy prior has slightly narrower credible intervals:
pdf(file = "Treatment effect distribution - Compare prior assumptions - Data 8.pdf",
    width = 8,
    height = 6)
plot(BHM_compare_2_sales_Data8, order = TRUE) + ggtitle("8 sites: model comparison") +
  theme(plot.title = element_text(hjust = 0.35))
dev.off()

# compare the posterior hyper-distribution of the effect size
# note that these plots will look different if re-run!
pdf(file = "Posterior hyper-effect distribution - Compare prior assumptions - Data 8.pdf",
    width = 8,
    height = 6)
effect_plot("Default priors" = Sales_BHM_1_Data8, "Cauchy prior" = Sales_BHM_2_Data8) +
  coord_cartesian(xlim = c(-10, 30), ) + theme(legend.position = "top") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme(plot.title = element_text(hjust = 0.35))
dev.off()

# Cross-validation - how each group affects the output; how the model predicts the left-out groups
BHM_crossval_Data8 <- loocv(
  Sales_Data,
  model = "rubin",
  pooling = "partial",
  return_models = TRUE,
  prior_hypermean = normal(0, 100),
  prior_hypersd = cauchy(0, 25),
  iter = 20000,
  chains = 8,
  control = list(adapt_delta = 0.95)
)
BHM_crossval_Data8
summary(BHM_crossval_Data8)


# We can extract the hyper-mean and heterogeneity BHM estimates, with the Cauchy(0,25) prior, from the leave-one-out cross-validation. There is variation in the estimated treatment effect and hetero, but it appears reasonable:
# Change LPD to the L-O-O info criterion = -2 * mean log predictive density, which should be close to 0:
looic_Data8 <- -2 * BHM_crossval_Data8$df$lpd
mean(looic_Data8)

Sales_crossval_output_Data8 <- cbind(Sales_Data$group, BHM_crossval_Data8$df[, c(1, 2)], looic_Data8)
colnames(Sales_crossval_output_Data8) <- c("Left-out study",
                                           "Hypermean (tau)",
                                           "Hyper-SD",
                                           "LOO Info Criterion")
Sales_crossval_output_Data8

# extract the cross-validation table to Latex
sink("sales_Data8_CauchyBHM_LOOCV.txt")
print.xtable(xtable(Sales_crossval_output_Data8))
sink()

# comparing LOOIC/ELPD for full pooling vs BHM partial pooling - not extracted:
full_pool_crossval_sales_Data8 <- loocv(
  data = Sales_Data,
  model = "rubin",
  pooling = "full",
  prior_hypermean = normal(0, 100)
)

full_pool_looic_Data8 <- -2 * full_pool_crossval_sales_Data8$df$lpd
mean(full_pool_looic_Data8)
loo_compare(BHM_crossval_Data8, full_pool_crossval_sales_Data8)
