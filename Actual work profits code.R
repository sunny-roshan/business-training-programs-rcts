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

########
# Only summary data, all 16 studies:
group <- c("Avdeenko et al. (2019)", "Anderson and McKenzie (2020)", "Buvinic et al. (2020)",
           "Alibhai et al. (2019)", "Brooks et al. (2018)", "Campos et al. (2017)",
           "Bakhtiar et al. (2021)", "Anderson et al. (2018) (Finance)",
           "Anderson et al. (2018) (Marketing)", "Bruhn and Zia (2013)", "Calderon et al. (2020)",
           "de Mel et al. (2014) (Existing)", "de Mel et al. (2014) (Potential)",
           "Berge et al. (2015) (Males)", "Berge et al. (2015) (Females)",
           "Giné and Mansuri (2020)")

# function for backing out s.e.s from 95% C.I.'s some of which are asymmetric
compute_se <- function(mean, CI_upperbound, CI_lowerbound) {
  s_e <- (((CI_upperbound - mean)/1.96) + (mean - CI_lowerbound)/1.96)/2
  return(s_e)
}

tau <- c(-23.89, 21.82, 15.23, 7.21, 6.90, 11.18, 80.48, 40.96, 61.06, -15.02, 23.73, -4.27, 41.68, 13.66, 3.60, -8.06)
CIupper <- c(12.20, 69.93, 27.12, 16.30, 22.65, 25.06, 125.71, 77.72, 105.22, 32.0, 53.51, 26.31, 82.01, 51.61, 28.88, 9.47) 
CIlower <- c(-48.37, -26.29, 3.35, -1.88, -8.84, -2.69, 35.23, 4.20, 16.90, -62.05, -0.26, -34.84, 1.34, -14.8, -21.68, -22.77)     
se <- compute_se(tau,CIupper,CIlower)

Profits <- data.frame(group,tau,se)
Profits

# model 1 - uniform prior variance, only summary data, no covariates
defaultvarpiorlimit <- 10*sd(tau)
defaultvarpiorlimit

# need to research what adapt_delta does in MCMC
# First prior is N(0,100) for mu and U(0, 275.981) for hypersd (not hypervar)
Profits_BHM <- baggr(data = Profits, model = "rubin", pooling = "partial", 
                     prior_hypermean = normal(0,100), prior_hypersd = uniform(0,defaultvarpiorlimit), 
                     iter = 15000, chains = 8, control = list(adapt_delta = 0.95))
print(Profits_BHM)

curve(dhcauchy(x, 25), 0, 100, col = "blue", add = TRUE)
curve(dhcauchy(x, 50), 0, 100, col = "red", add = TRUE)
curve(dhcauchy(x, 15), 0, 100, col = "black", add = TRUE)
curve(dhcauchy(x, 40), 0, 100, col = "orange", add = TRUE)

# changing prior on on hypersd to half-Cauchy(0,25) - better than uniform (less weight on extreme hetero)
Profits_BHM_2 <- baggr(data = Profits, model = "rubin", pooling = "partial",
                       prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25),
                       iter = 15000, chains = 8, control = list(adapt_delta = 0.95))
print(Profits_BHM_2)

# what if I use an even flatter half Cauchy(0,40)?
Profits_BHM_3 <- baggr(data = Profits, model = "rubin", pooling = "partial",
                       prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,40),
                       iter = 15000, chains = 8, control = list(adapt_delta = 0.95))
print(Profits_BHM_3)

#####
# understanding the baggr object so we can extract the model table ##
summary(Profits_BHM_2)
# the stan model is the object 'fit' - extract it
Profits_BHM$fit
class(Profits_BHM$fit)

# the stan fit object contains the hypermean, hypervar, and 
# shrunk tau parameters for each iteration; as well as the summary figures, which are what we want
profits_fit_unif_summary <- summary(Profits_BHM$fit)
print(names(profits_fit_unif_summary))
print(profits_fit_unif_summary$summary)

# the summary contains the parameter 'eta', the site-specific error term;
# it also contains some percentiles and stats for the mean, var, and taus;
# we can subset to get only the rows and columns that we want
profits_output_matrix_unif <- summary(Profits_BHM$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1,3,4,5,7,8,10)]
print(profits_output_matrix_unif)
profits_output_matrix_cauchy1 <- summary(Profits_BHM_2$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1,3,4,5,7,8,10)]
print(profits_output_matrix_cauchy1)
profits_output_matrix_cauchy2 <- summary(Profits_BHM_3$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1,3,4,5,7,8,10)]
print(profits_output_matrix_cauchy2)

# convert this stan output matrix to a dataframe
prof_df_unif <- as.data.frame(profits_output_matrix_unif)
prof_df_unif
prof_df_cauchy1 <- as.data.frame(profits_output_matrix_cauchy1)
prof_df_cauchy1
prof_df_cauchy2 <- as.data.frame(profits_output_matrix_cauchy2)
prof_df_cauchy2

# change the rownames from mu, tau, tau[1], etc. to the study names
tab_lab <- c("Hypermean (mu)", "Hyper-sd (tau)", group)
rownames(prof_df_unif) <- tab_lab
rownames(prof_df_cauchy1) <- tab_lab
rownames(prof_df_cauchy2) <- tab_lab
prof_df_unif
prof_df_cauchy1
prof_df_cauchy2

# extract prof_df_unif and prof_df_cauchy1 and prof_df_cauchy2 to Latex tables
setwd("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Profits output")

sink("profits_all16_Unif_BHM.txt")
print.xtable(xtable(prof_df_unif))
sink()

sink("profits_all16_Cauchy1_BHM.txt")
print.xtable(xtable(prof_df_cauchy1))
sink()

sink("profits_all16_Cauchy2_BHM.txt")
print.xtable(xtable(prof_df_cauchy2))
sink()

#####
# Pooling and hetero stats
# note that the pooling factor equals (1 - lambda) from Rubin (1981) - 
# lambda is the between-group hetero (in tau^2, the hyper-var) relative to total variation incl. sampling se
# so the pooling factor here is the sampling variation relative to total variation incl. true hetero

# aside - pooling stats in the stanfit object
pooling <- Profits_BHM_2$pooling_metric
pool2<-as.data.frame(pooling)
pool2
#
pooling(Profits_BHM_2) # gives the pooling factors for each group separately, based on own s.e.'s
heterogeneity(Profits_BHM_2) # avg 'pooling factor' measured as explained above
# I^2 is the 'lambda' pooling factor we're interested in: the between-group hetero (in tau^2, the hyper-var) 
# relative to total variation incl. sampling se
# higher the I^2 stat (closer to 1), greater the true hetero.
pf_unif <- pooling(Profits_BHM, type = "total", metric = "isq") 
pf_cauchy1 <- pooling(Profits_BHM_2, type = "total", metric = "isq") 
pf_cauchy2 <- pooling(Profits_BHM_3, type = "total", metric = "isq")

# make a table of the pooling factors' percentiles, and extract Latex output
avg_pool_factors <- rbind(pf_unif, pf_cauchy1, pf_cauchy2)
colnames(avg_pool_factors) <- c("2.5%", "Mean", "97.5%")
rownames(avg_pool_factors) <- c("Uniform Prior", "Cauchy(0,25)", "Cauchy(0,40)")
avg_pool_factors

sink("profits_heterogeneity_poolfactor.txt")
print.xtable(xtable(avg_pool_factors))
sink()

#####
# Graphical output
# If there is a ggplot error, add + theme(text=element_text(family="Arial", size=14))
# If you want to centre the plot title, add + theme(plot.title = element_text(hjust = 0.5))

# (forest) plots of posterior effects, ordered by effect size, with hypermean added
## Uniform prior
pdf(file="Posterior intervals 1 ordered.pdf", width = 8, height = 4)
plot(Profits_BHM, order = TRUE, hyper = TRUE) + 
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

## Cauchy(0,25) prior
pdf(file="Posterior intervals 2 ordered.pdf", width = 8, height = 4)
plot(Profits_BHM_2, order = TRUE, hyper = TRUE) + 
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

## Cauchy(0,40) prior
pdf(file="Posterior intervals 3 ordered.pdf", width = 8, height = 4)
plot(Profits_BHM_3, order = TRUE, hyper = TRUE) + 
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

# With the actual posterior distributions:
## Uniform prior
pdf(file="Posterior distributions 1 ordered.pdf", width = 8, height = 6.5)
plot(Profits_BHM, style = "areas", hyper = TRUE, order = TRUE) +  
  ggtitle("Posterior treatment distributions") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

## Cauchy(0,25) prior
pdf(file="Posterior distributions 2 ordered.pdf", width = 8, height = 6.5)
plot(Profits_BHM_2, style = "areas", hyper = TRUE, order = TRUE) +  
  ggtitle("Posterior treatment distributions") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

## Cauchy(0,40) prior
pdf(file="Posterior distributions 3 ordered.pdf", width = 8, height = 6.5)
plot(Profits_BHM_3, style = "areas", hyper = TRUE, order = TRUE) +  
  ggtitle("Posterior treatment distributions") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

# forest plot with "both" shows the individual estimates and BHM shrunk estimates
## Uniform
pdf(file="Original and posteriors 1 forest.pdf", width = 8, height = 5)
forest_plot(Profits_BHM, show = "both")
dev.off()

##Cauchy(0,25)
pdf(file="Original and posteriors 2 forest.pdf", width = 8, height = 5)
forest_plot(Profits_BHM_2, show = "both")
dev.off()

##Cauchy(0,40)
pdf(file="Original and posteriors 3 forest.pdf", width = 8, height = 5)
forest_plot(Profits_BHM_3, show = "both")
dev.off()

# Plot of posterior mu distribution: this is the predictive distribution for extrapolation
## uniform
pdf(file="Posterior hypermean treatment distribution 1.pdf", width = 6, height = 6)
effect_plot(Profits_BHM) + 
  coord_cartesian(xlim = c(-15, 40)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

## Cauchy(0,25)
pdf(file="Posterior hypermean treatment distribution 2.pdf", width = 6, height = 6)
effect_plot(Profits_BHM_2) + 
  coord_cartesian(xlim = c(-15, 40)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

## Cauchy(0,40)
pdf(file="Posterior hypermean treatment distribution 3.pdf", width = 6, height = 6)
effect_plot(Profits_BHM_3) + 
  coord_cartesian(xlim = c(-15, 40)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

# random draws from posterior tau distribution
x<-effect_draw(Profits_BHM, n = 1e4)
hist(x, breaks = 900, xlim = c(-10, 30))

# First comparison: comparing Bayesian FE, no-pooling, BHM with cauchy 1 prior (= Profits_BHM_2):
# I DON'T KNOW WHAT POSTERIOR PREDICTIVE EFFECTS ARE - WHY ARE THEY SO WIDE??
# For Bayesian full pooling and no-pooling there is no prior variance specification.

fullpoolcomparo <- baggr(Profits, model = "rubin", pooling = "full",
                         prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25),
                         iter = 20000, chains = 8, control = list(adapt_delta = 0.95))
fullpoolcomparo

nopoolcomparo <- baggr(Profits, model = "rubin", pooling = "none",
                       prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25),
                       iter = 20000, chains = 8, control = list(adapt_delta = 0.95))
nopoolcomparo

BHM_compare <- baggr_compare("No pooling model" = nopoolcomparo, "Full pooling model" = fullpoolcomparo,
                             "Partial pooling BHM" = Profits_BHM_2, what = "pooling")
BHM_compare
summary(BHM_compare)
BHM_compare_df <- rbind(BHM_compare$mean_trt, BHM_compare$sd_trt, BHM_compare$posteriorpd_trt)
# drop the no-pooling since that means there are no aggregate estimates:
BHM_compare_df <- BHM_compare_df[c(2,3,5,6,8,9),]
BHM_compare_df
rownames(BHM_compare_df) <- c("Partial Pooling: Mean Effect", "Full Pooling FE: Mean Effect",
                                "Partial Pooling: Hyper-SD (tau)", "Full Pooling FE: Hyper-SD (tau)",
                                "Partial Pooling: Posterior Predictive Effect", "Full Pooling FE: Posterior Predictive Effect")
BHM_compare_df

## Haven't exported it because doesn't seem important to have it in a table;
## the more interesting comparison is in graphical form
#export this comparison table to Latex
#sink("Comparison of Cauchy BHM and Full Pooling.txt")
#print.xtable(xtable(BHM_compare_df))
#sink()

# unable to change the x-axis increments
pdf(file="Treatment effect distribution - Compare pooling assmptions.pdf", width = 8, height = 6)
plot(BHM_compare, order = TRUE) + ggtitle("Profits: Comparison of Pooling Assumptions") +
  theme(plot.title = element_text(hjust = 0.35)) +
  theme(text=element_text(family="serif", size=14))
dev.off()

## Second comparison: compare priors, not pooling assumptions
# Compare BHM with U(0,10*sd(theta)) prior sd to Cauchy (0,25) and Cauchy (0,40) prior sd
BHM_compare_2 <- baggr_compare("U[0,275]" = Profits_BHM,
                               "Cauchy (0,25)" = Profits_BHM_2,
                               "Cauchy (0,40)" = Profits_BHM_3)
summary(BHM_compare_2)
BHM_compare_2_df <- rbind(BHM_compare_2$mean_trt, BHM_compare_2$sd_trt, BHM_compare_2$posteriorpd_trt)
rownames(BHM_compare_2_df) <- c("U[0,275]: Mean", "Cauchy (0,25): Mean", "Cauchy (0,40): Mean",
                                "U[0,275]: Hyper-SD (tau)", "Cauchy (0,25): Hyper-SD (tau)", "Cauchy (0,40): Hyper-SD (tau)",
                                "U[0,275]: Posterior Predictive Effect", "Cauchy (0,25): Posterior Predictive Effect",
                                "Cauchy (0,40): Posterior Predictive Effect")
BHM_compare_2_df

sink("Comparison of 3 priors Profit BHM.txt")
print.xtable(xtable(BHM_compare_2_df))
sink()

# Compare credible intervals:
pdf(file="Treatment effect distribution - Compare prior assumptions.pdf", width = 8, height = 7)
plot(BHM_compare_2, order = TRUE) + ggtitle("Profits: model priors comparison") +
  theme(text=element_text(family="serif", size=14))
dev.off()

# compare the posterior hyper-distribution of the effect size
# note that these plots will look different if re-run!
pdf(file="Posterior hyper-effect distribution - Compare prior assumptions 2.pdf", width = 8, height = 6)
effect_plot("U[0,275]" = Profits_BHM, "Cauchy (0,25)" = Profits_BHM_2,
            "Cauchy (0,40)" = Profits_BHM_3) + 
  coord_cartesian(xlim = c(-30, 100),) + theme(legend.position = "top") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(family="serif", size=14))
dev.off()

# Cross-validation - how leaving out each group affects the output; how the model predicts the left-out groups
BHM_crossval <- loocv(Profits, model = "rubin", pooling = "partial", return_models = TRUE, 
                      prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25), 
                      iter = 15000, chains = 8, control = list(adapt_delta = 0.95))
BHM_crossval
summary(BHM_crossval)

# We can extract the hyper-mean and heterogeneity BHM estimates, with the Cauchy(0,25) prior,
# from the leave-one-out cross-validation. There is variation in the estimated treatment
# effect and hetero, but it appears reasonable:
# Change LPD to the L-O-O info criterion = -2 * mean log predictive density, which should be close to 0:
looic <- -2*BHM_crossval$df$lpd
mean(looic) # The mean is 9.54. Ref. Gelman TB to see how this looks.
Profits_crossval_output <- cbind(group, BHM_crossval$df[,c(1,2)],looic)
colnames(Profits_crossval_output) <- c("Left-out study", "Hypermean (mu)", "Hyper-SD (tau)", "LOO Info Criterion")
Profits_crossval_output

# extract the cross-validation table to Latex
sink("Profits_CauchyBHM_LOOCV.txt")
print.xtable(xtable(Profits_crossval_output))
sink()

# comparing LOOIC/ELPD for full pooling vs BHM partial pooling - not extracted:
full_pool_crossval <- loocv(data = Profits, model = "rubin", pooling = "full", 
                            prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25))
full_pool_crossval$df
full_pool_looic <- -2*full_pool_crossval$df$lpd
mean(full_pool_looic) # = 9.39. Lower than BHM. Ref. Gelman again.
mean(full_pool_looic) - mean(looic) # = -0.1519265 

##### 
##### With covariates
x<-prepare_ma(microcredit_simplified, outcome = "consumption")
x

Female_proportion <- c(.683, .49, 1, 1, 1, .53, 1, .44, .44, .35, 1, 1, 1, 0, 1, .49)
Urban_proportion <- c(.155, 1, 0, 1, 1, 1, N/A, 1, 1, .29, 0, 1, 1, 1, 1, 0)
GDP_pc_growth <- c(4, -1.8, 3.8, 6.5, 2.4, 3.2, 7.2, 0.8, 0.8, -2.5, -6.7, 2.8, 2.8, 2.7, 2.7, 2.5)

Profits_expanded <- data.frame(group,tau,se,Female_proportion,Urban_proportion, GDP_pc_growth)
View(Profits_expanded)

Profits_femalescontrol <- baggr(Profits_expanded, 
      outcome = "outcome",
      group = "group",
      pooling = "partial",
      model = "rubin",
      covariates = c("Female_proportion"),
      prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25),
      prior_beta = normal(0,100),
      iter = 15000, chains = 8, control = list(adapt_delta = 0.95))
print(Profits_femalescontrol)

print(Profits_BHM_2)

Profits_gdpcontrol <- baggr(Profits_expanded, 
                              outcome = "outcome",
                              group = "group",
                              pooling = "partial",
                              model = "rubin",
                              covariates = c("GDP_pc_growth"),
                              prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25),
                              prior_beta = normal(0,100),
                              iter = 15000, chains = 8, control = list(adapt_delta = 0.95))
print(Profits_gdpcontrol)

Profits_2controls <- baggr(Profits_expanded, 
                            outcome = "outcome",
                            group = "group",
                            pooling = "partial",
                            model = "rubin",
                            covariates = c("Female_proportion", "GDP_pc_growth"),
                            prior_hypermean = normal(0,50), prior_hypersd = cauchy(0,20),
                            prior_beta = normal(0,50),
                            iter = 15000, chains = 8, control = list(adapt_delta = 0.95))
print(Profits_2controls)

# To use Urban proportion as a fixed effect we need to drop Bakhtiar et al.

Profits_expanded_minus1 <- na.omit(Profits_expanded)
View(Profits_expanded_minus1)

Profits_urbancontrol <- baggr(Profits_expanded_minus1, 
                              outcome = "outcome",
                              group = "group",
                              pooling = "partial",
                              model = "rubin",
                              covariates = c("Urban_proportion"),
                              prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25),
                              prior_beta = normal(0,100),
                              iter = 15000, chains = 8, control = list(adapt_delta = 0.95))
print(Profits_urbancontrol)

Profits_3controls <- baggr(Profits_expanded_minus1, 
                           outcome = "outcome",
                           group = "group",
                           pooling = "partial",
                           model = "rubin",
                           covariates = c("Female_proportion", "GDP_pc_growth", "Urban_proportion"),
                           prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25),
                           prior_beta = normal(0,100),
                           iter = 15000, chains = 8, control = list(adapt_delta = 0.95))
print(Profits_3controls)

## Now to extract output from the covariate-augmented models
## Starting with the Latex tables of posteriors

profits_output_matrix_female <- summary(Profits_femalescontrol$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1,3,4,5,7,8,10)]
print(profits_output_matrix_female)
profits_output_matrix_gdp <- summary(Profits_gdpcontrol$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1,3,4,5,7,8,10)]
print(profits_output_matrix_gdp)
profits_output_matrix_urban <- summary(Profits_urbancontrol$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1,3,4,5,7,8,10)]
print(profits_output_matrix_urban)
profits_output_matrix_3controls <- summary(Profits_3controls$fit, pars = c("mu", "tau", "theta_k"))$summary[, c(1,3,4,5,7,8,10)]
print(profits_output_matrix_3controls)

# convert this stan output matrix to a dataframe
profits_output_matrix_female <- as.data.frame(profits_output_matrix_female)
profits_output_matrix_female
profits_output_matrix_urban <- as.data.frame(profits_output_matrix_urban)
profits_output_matrix_urban
profits_output_matrix_gdp <- as.data.frame(profits_output_matrix_gdp)
profits_output_matrix_gdp
profits_output_matrix_3controls <- as.data.frame(profits_output_matrix_3controls)
profits_output_matrix_3controls

# change the rownames from mu, tau, tau[1], etc. to the study names
group_3control <- group[-7]
tab_lab_urban <- c("Hypermean (mu)", "Hyper-sd (tau)",group_3control)
rownames(profits_output_matrix_female) <- tab_lab
rownames(profits_output_matrix_gdp) <- tab_lab
rownames(profits_output_matrix_urban) <- tab_lab_urban
rownames(profits_output_matrix_3controls) <- tab_lab_urban
profits_output_matrix_female
profits_output_matrix_gdp
profits_output_matrix_urban
profits_output_matrix_3controls

# extract profits_output_matrix_female, profits_output_matrix_gdp,
# profits_output_matrix_urban, and profits_output_matrix_3controls to Latex tables
setwd("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Profits output")

sink("profits_female_BHM.txt")
print.xtable(xtable(profits_output_matrix_female))
sink()

sink("profits_gdp_BHM.txt")
print.xtable(xtable(profits_output_matrix_gdp))
sink()

sink("profits_urban_BHM.txt")
print.xtable(xtable(profits_output_matrix_urban))
sink()

sink("profits_3controls_BHM.txt")
print.xtable(xtable(profits_output_matrix_3controls))
sink()

##
## Pooling stat - I^2 (=lambda from your methodology)
pf_female <- pooling(Profits_femalescontrol, type = "total", metric = "isq") 
pf_gdp <- pooling(Profits_gdpcontrol, type = "total", metric = "isq") 
pf_urban <- pooling(Profits_urbancontrol, type = "total", metric = "isq")

# make a table of the pooling factors' percentiles, and extract Latex output
avg_pool_factors_covariates <- rbind(pf_female, pf_gdp, pf_urban)
colnames(avg_pool_factors_covariates) <- c("2.5%", "Mean", "97.5%")
rownames(avg_pool_factors_covariates) <- c("Gender indicator", "GDP pc growth %", "% Urban")
avg_pool_factors_covariates

sink("profits_heterogeneity_poolfactor_covariates.txt")
print.xtable(xtable(avg_pool_factors_covariates))
sink()

##
## Graphical output
# If there is a ggplot error, add + theme(text=element_text(family="Arial", size=14))
# If you want to centre the plot title, add + theme(plot.title = element_text(hjust = 0.5))
forest_plot(Profits_femalescontrol, show = "both")

plot(Profits_3controls, style = "areas", hyper = TRUE, order = TRUE) +  
  ggtitle("Posterior treatment distributions") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text=element_text(family="Arial", size=14))

pdf(file="Posterior hyper-effect distribution - controls or no.pdf", width = 8, height = 6)
effect_plot("Cauchy (0,25)" = Profits_BHM_2,
            "3 controls" = Profits_3controls) +
  coord_cartesian(xlim = c(-30, 100),) + theme(legend.position = "top") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(family="serif", size=14))
dev.off()

# Cross val
BHM_3controls_crossval <- loocv(Profits_expanded_minus1, 
                                outcome = "outcome",
                                group = "group",
                                pooling = "partial",
                                model = "rubin",
                                covariates = c("Female_proportion", "GDP_pc_growth", "Urban_proportion"),
                                prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25),
                                prior_beta = normal(0,100),
                                iter = 15000, chains = 8, control = list(adapt_delta = 0.95),
                                return_models = TRUE)
BHM_3controls_crossval

# We can extract the hyper-mean and heterogeneity BHM estimates
# from the leave-one-out cross-validation. 
# Change LPD to the L-O-O info criterion = -2 * mean log predictive density, which should be close to 0:
looic_3controls <- -2*BHM_3controls_crossval$df$lpd
mean(looic_3controls) # The mean is 10.18. Ref. Gelman TB to see how this looks.
Profits_3controls_crossval_output <- cbind(group_3control, BHM_3controls_crossval$df[,c(1,2)],looic_3controls)
colnames(Profits_3controls_crossval_output) <- c("Left-out study", "Hypermean (mu)", "Hyper-SD (tau)", "LOO Info Criterion")
Profits_3controls_crossval_output

# extract the cross-validation table to Latex
sink("Profits_3controls_LOOCV.txt")
print.xtable(xtable(Profits_3controls_crossval_output))
sink()


##########################################################
### NOW WITH ONLY THE DATASETS THAT YOU HAVE ACCESS TO ###
# subsetting the full df using dplyr

# Save all output to this folder, not the overall R folder, for cleanliness
getwd()
setwd("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Graphical output/Profits")
Profits
Profits_Data <- filter(Profits, group == "Gine and Mansuri 2020" | group == "Berge et al. 2015 females" |
                                group == "Berge et al. 2015 males" | group == "Anderson et al. 2018 finance" |
                                group == "Anderson et al. 2018 marketing" | group == "Brooks et al. 2018" |
                                group == "Campos et al. 2017 traditional" | group == "Anderson and McKenzie 2020")
Profits_Data

# model 1 - default priors, only summary data, no covariates; but adding debugging  bc otherwise MCMC problematic
defaultvarpiorlimit_Data8 <- 10*sd(Profits_Data$tau)
defaultvarpiorlimit_Data8

Profits_BHM_Data8 <- baggr(Profits_Data, model = "rubin", pooling = "partial", prior_hypermean = normal(0,100), prior_hypersd = uniform(0,defaultvarpiorlimit_Data8), iter = 20000, chains = 8, control = list(adapt_delta = 0.95))
print(Profits_BHM_Data8)

# model 2 - Cauchy variance prior
Profits_BHM_2_Data8 <- baggr(Profits_Data, model = "rubin", pooling = "partial", prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25), iter = 20000, chains = 8, control = list(adapt_delta = 0.95))
print(Profits_BHM_2_Data8)

# extracting the BHM model to Latex
profits_fit_1_Data8 <- Profits_BHM_Data8$fit
profits_fit_2_Data8 <- Profits_BHM_2_Data8$fit
class(profits_fit_1_Data8)
# the stan fit object contains the hypermean, hypervar, and 
# shrunk tau parameters for each iteration, as well as the summary figures, which are what we want
profits_fit_1_Data8_summary <- summary(profits_fit_1_Data8)
print(names(profits_fit_1_Data8_summary))
print(profits_fit_1_Data8_summary$summary)

profits_fit_2_Data8_summary <- summary(profits_fit_2_Data8)
print(profits_fit_2_Data8_summary$summary)

# the summary contains the parameter 'eta', which I don't understand and 
# have not been able to find info about online;
# it also contains some percentiles and stats for the mean, var, and taus;
# we can subset to get only the rows and columns that we want
profits_output_matrix_1_Data8 <- summary(profits_fit_1_Data8, pars = c("mu", "tau", "tau_k"))$summary[, c(1,3,4,5,7,8)]
print(profits_output_matrix_1_Data8)
profits_output_matrix_2_Data8 <- summary(profits_fit_2_Data8, pars = c("mu", "tau", "tau_k"))$summary[, c(1,3,4,5,7,8)]
print(profits_output_matrix_2_Data8)

# convert this stan output matrix to a dataframe
prof_df_1_Data8 <- as.data.frame(profits_output_matrix_1_Data8)
prof_df_1_Data8
prof_df_2_Data8 <- as.data.frame(profits_output_matrix_2_Data8)
prof_df_2_Data8

# change the rownames from mu, tau, tau[1], etc. to the study names
tab_lab_Data8 <- c("Hypermean (tau)", "Hyper-sd (sigma_tau)", Profits_Data$group)
tab_lab_Data8
rownames(prof_df_1_Data8) <- tab_lab_Data8
rownames(prof_df_2_Data8) <- tab_lab_Data8
prof_df_1_Data8
prof_df_2_Data8

# extract prof_df_1_Data8 and prof_df_2_Data8 to Latex tables
sink("profits_Data8_BHM_default.txt")
print.xtable(xtable(prof_df_1_Data8))
sink()

sink("profits_Data8_BHM_Cauchy.txt")
print.xtable(xtable(prof_df_2_Data8))
sink()


# Pooling and hetero stats
# note that the pooling factor equals (1 - lambda) from Rubin (1981) - 
# between-group hetero (in sigma_tau) relative to total variation incl. sampling se
# aside - pooling stats in the stanfit object
pooling_1_Data8 <- Profits_BHM_Data8$pooling_metric
pooling_2_Data8 <- Profits_BHM_2_Data8$pooling_metric
poolfactors_1_Data8 <- as.data.frame(pooling_1_Data8)
poolfactors_2_Data8 <- as.data.frame(pooling_2_Data8)

#
pooling(Profits_BHM_2_Data8) # gives the pooling factors for each group separately, based on own s.e.'s
avg_lambda_Data8 <- heterogeneity(Profits_BHM_2_Data8) # avg pooling factor
avg_Isq_Data8 <- pooling(Profits_BHM_2_Data8, type = "total", metric = "isq") # I^2, measure of hetero, = (1 - pooling factor)

# make a table of the heterogeneity and pooling factors' percentiles, and extract Latex output
hetero_table_Data8 <- rbind(avg_lambda_Data8, avg_Isq_Data8)
colnames(hetero_table_Data8) <- c("2.5%", "Mean", "97.5%")
rownames(hetero_table_Data8) <- c("Average Pooling", "Average I^2")
hetero_table_Data8

sink("profits_hetero_Data8_CauchyBHM.txt")
print.xtable(xtable(hetero_table_Data8))
sink()


# Graphical output
# Change wd to sub-folder for these plots with only 8 datasets:
setwd("/Users/sunny/Library/CloudStorage/OneDrive-Personal/Documents/MPhil/Thesis/Thesis/R/Graphical output/Profits Data 8")

# If there is a ggplot error, add + theme(text=element_text(family="Arial", size=14))
# If you want to centre the plot title, add + theme(plot.title = element_text(hjust = 0.5))

# (forest) plots of posterior effects
pdf(file="Posterior intervals.pdf", width = 8, height = 4)
plot(Profits_BHM_2_Data8, order = FALSE) +  
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
dev.off()

# ordered by effect size, with hypermean added
pdf(file="Posterior intervals ordered.pdf", width = 8, height = 4)
plot(Profits_BHM_2_Data8, order = TRUE, hyper = TRUE) + 
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
dev.off()

# with the actual posterior distributions:
pdf(file="Posterior distributions ordered.pdf", width = 8, height = 6)
plot(Profits_BHM_2_Data8, style = "areas", hyper = TRUE, order = TRUE) +  
  ggtitle("Posterior means and credible intervals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
dev.off()

# "both" shows the individual estimates and BHM shrunk estimates
pdf(file="Original and posteriors.pdf", width = 8, height = 4)
forest_plot(Profits_BHM_2_Data8, show = "both") 
dev.off()

# plot of posterior tau distribution: this is the predictive distribution for extrapolation
pdf(file="Posterior treatment distribution.pdf", width = 6, height = 6)
effect_plot(Profits_BHM_2_Data8) + 
  coord_cartesian(xlim = c(-10, 30)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

# random draws from posterior tau distribution
effect_draw(Profits_BHM_2_Data8, n = 20)

##
# First comparison: comparing Bayesian FE, no-pooling, BHM with default priors (= Profits_BHM):
# I DON'T KNOW WHAT POSTERIOR PREDICTIVE EFFECTS ARE - WHY ARE THEY SO WIDE??
# For Bayesian full pooling and no-pooling there is no prior variance specification.

fullpoolcomparo_Data8 <- baggr(Profits_Data, model = "rubin", pooling = "full", prior_hypermean = normal(0,100), 
                                      iter = 20000, chains = 8, control = list(adapt_delta = 0.95))
fullpoolcomparo_Data8
nopoolcomparo_Data8 <- baggr(Profits_Data, model = "rubin", pooling = "none", prior_hypermean = normal(0,100), 
                                    iter = 20000, chains = 8, control = list(adapt_delta = 0.95))
nopoolcomparo_Data8

BHM_compare_Data8 <- baggr_compare("No pooling model" = nopoolcomparo_Data8, "Full pooling model" = fullpoolcomparo_Data8,
                                   "Partial pooling BHM" = Profits_BHM_2_Data8, what = "pooling")
BHM_compare_Data8
summary(BHM_compare_Data8)
BHM_compare_Data8_df <- rbind(BHM_compare_Data8$mean_trt, BHM_compare_Data8$sd_trt, BHM_compare_Data8$posteriorpd_trt)
# drop the no-pooling since that means there are no aggregate estimates:
BHM_compare_Data8_df <- BHM_compare_Data8_df[c(2,3,5,6,8,9),]
BHM_compare_Data8_df
rownames(BHM_compare_Data8_df) <- c("Partial Pooling: Mean", "Full Pooling FE: Mean",
                              "Partial Pooling: SD", "Full Pooling FE: SD",
                              "Partial Pooling: Posterior Predictive Effect", "Full Pooling FE: Posterior Predictive Effect")
BHM_compare_Data8_df

# export this comparison table to Latex; the more interesting comparison is in graphical form
sink("Comparison of BHM and Full Pooling Profit BHM 8 Datasets.txt")
print.xtable(xtable(BHM_compare_Data8_df))
sink()

# graphical comparison of full, partial, and no pooling modelling assumptions;
# unable to change the x-axis increments:
pdf(file="Treatment effect distribution - Compare pooling assmptions.pdf", width = 8, height = 6)
plot(BHM_compare_Data8) + ggtitle("8 sites: Model Comparison") +
  theme(plot.title = element_text(hjust = 0.35))
dev.off()
#

# Second comparison: compare BHM with default priors to Cauchy variance prior and longer chains/iterations
BHM_compare_Data8_2 <- baggr_compare("Default priors" = Profits_BHM_Data8, "Cauchy prior and debugging" = Profits_BHM_2_Data8)
summary(BHM_compare_Data8_2)

BHM_compare_Data8_2_df <- rbind(BHM_compare_Data8_2$mean_trt, BHM_compare_Data8_2$sd_trt, BHM_compare_Data8_2$posteriorpd_trt)
rownames(BHM_compare_Data8_2_df) <- c("Default Model: Mean", "Cauchy Prior: Mean",
                                "Default Model: SD", "Cauchy Prior: SD",
                                "Default Model: Posterior Predictive Effect", "Cauchy Prior: Posterior Predictive Effect")
BHM_compare_Data8_2_df
sink("Comparison of Default and Cauchy Profit BHM.txt")
print.xtable(xtable(BHM_compare_Data8_2_df))
sink()

# Cauchy prior has slightly narrower credible intervals:
pdf(file="Treatment effect distribution - Compare prior assumptions.pdf", width = 8, height = 6)
plot(baggr_compare("Default priors" = Profits_BHM_Data8, "Cauchy prior" = Profits_BHM_2_Data8)) +
  ggtitle("8 sites: model comparison") 
dev.off()

# compare the posterior hyper-distribution of the effect size
# note that these plots will look different if re-run!
pdf(file="Posterior hyper-effect distribution - Compare prior assumptions.pdf", width = 8, height = 6)
effect_plot("Default priors" = Profits_BHM_Data8, "Cauchy prior" = Profits_BHM_2_Data8) + 
  coord_cartesian(xlim = c(-10, 30),) + theme(legend.position = "top") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#

# Cross-validation - how each group affects the output; how the model predicts the left-out groups
BHM_crossval_Data8 <- loocv(Profits_Data, model = "rubin", pooling = "partial", return_models = TRUE, 
                            prior_hypermean = normal(0,100), prior_hypersd = cauchy(0,25), 
                            iter = 25000, chains = 8, control = list(adapt_delta = 0.95))
BHM_crossval_Data8
summary(BHM_crossval_Data8)

# We can extract the hyper-mean and heterogeneity BHM estimates, with the Cauchy(0,25) prior,
# from the leave-one-out cross-validation. There is variation in the estimated treatment
# effect and hetero, but it appears reasonable:
# Change LPD to the L-O-O info criterion = -2 * mean log predictive density, which should be close to 0:
looic_Data8 <- -2*BHM_crossval_Data8$df$lpd
mean(looic_Data8)
Profits_crossval_Data8_output <- cbind(Profits_Data$group, BHM_crossval_Data8$df[,c(1,2)],looic_Data8)
colnames(Profits_crossval_Data8_output) <- c("Left-out study", "Hypermean (tau)", "Hyper-SD", "LOO Info Criterion")
Profits_crossval_Data8_output

# extract the cross-validation table to Latex
sink("Profits_Data8_CauchyBHM_LOOCV.txt")
print.xtable(xtable(Profits_crossval_Data8_output))
sink()

# comparing LOOIC/ELPD for full pooling vs BHM partial pooling - not extracted:
full_pool_crossval_Data8 <- loocv(data = Profits_Data, model = "rubin", pooling = "full", prior_hypermean = normal(0,100))
full_pool_looic_Data8 <- -2*full_pool_crossval_Data8$df$lpd
mean(full_pool_looic_Data8)
loo_compare(BHM_crossval_Data8, full_pool_crossval_Data8)
