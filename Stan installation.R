install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
example(stan_model, package = "rstan", run.dontrun = TRUE)

## you must run these 3 commands every time
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)
check_cmdstan_toolchain()
cmdstanr::install_cmdstan()
install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")
