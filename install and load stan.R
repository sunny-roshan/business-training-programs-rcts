# Install rstan package and its dependencies, 
# Needed for Bayesian inference and MCMC simulations
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

# Load rstan and set options (run in each script)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Install and load cmdstanr
# alternative to rstan
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)
check_cmdstan_toolchain()
cmdstanr::install_cmdstan()

# Install additional packages
install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")

# Example usage of stan_model from rstan
example(stan_model, package = "rstan", run.dontrun = TRUE)
