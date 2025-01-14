# We need to manually create datasets of the summary statistics and key covariates from the studies in our meta-analysis
library(dplyr)
library

# Define a function that backs out standard errors from frequentist 95% confidence intervals
compute_se <- function(mean, CI_upperbound, CI_lowerbound) {
  std_error <- (((CI_upperbound - mean)/1.96) + (mean - CI_lowerbound)/1.96)/2
  return(std_error)
}

# First create Sales df with effect sizes (tau) and std errors
group_s <- c(
  "Avdeenko et al. (2019)",
  "Anderson and McKenzie (2020)",
  "Alibhai et al. (2019)",
  "Brooks et al. (2018)",
  "Campos et al. (2017)",
  "Bakhtiar et al. (2021)",
  "Chong and Velez (2020)",
  "Anderson et al. (2018) (Finance)",
  "Anderson et al. (2018) (Marketing)",
  "Calderon et al. (2020)",
  "de Mel et al. (2014) (Existing)",
  "de Mel et al. (2014) (Potential)",
  "Valdivia (2015)",
  "Berge et al. (2015) (Males)",
  "Berge et al. (2015) (Females)",
  "Giné and Mansuri (2020)",
  "Drexler et al. (2014) (Accounting)",
  "Karlan and Valdivia (2011)"
)

tau_s <- c(-0.99, 20.73, -0.89, 3.32, 5.59, 62.20, 35.8, 25.32, 64.43, 28.78, -14.11, 38.9, 16.88, 12.98, -0.5, -2.86, -7.8, 1)
CIupper_s <- c(20.2, 70.25, 25.46, 18.18, 22.24, 109.86, 76.18, 56.53, 111.03, 61.35, 40.27, 86.96, 38.34, 54.89, 27.7, 14.08, 10.37, 11.4)
CIlower_s <- c(-18.45, -28.8, -27.24, -11.53, -11.05, 14.54, -4.58, -5.9, 17.84, 2.8, -68.49, -9.14, -1.25, -17.6, -22.47, -17.28, -26.03, -8.42)
Female_proportion_s <- c(.683, .49, 1, 1, .53, 1, 1, .44, .44, 1, 1, 1, 1, 0, 1, .49, .9, .96)
Urban_proportion_s <- c(.155, 1, 1, 1, 1, NA, NA, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, .712)
GDP_pc_growth_s <- c(4, -1.8, 6.5, 2.4, 3.2, 7.2, 8.7, 0.8, 0.8, -6.7, 2.8, 2.8, 0.3, 2.7, 2.7, 2.5, 6, 4.3)

# back out standard errors
se_s <- compute_se(tau_s, CIupper_s, CIlower_s)

# Combine into df with and without covariates
Sales <- data.frame(group_s, tau_s, se_s)
Sales_expanded <- data.frame(group_s,
                             tau_s,
                             se_s,
                             Female_proportion_s,
                             Urban_proportion_s,
                             GDP_pc_growth_s)

# Export
write.table(Sales,
            file = here("cleaned-datasets", "Sales summary stats.csv"),
            sep = ",",
            row.names = FALSE)
write.table(Sales_expanded,
            file = here("cleaned-datasets", "Sales stats with covariates.csv"),
            sep = ",",
            row.names = FALSE)

# Next create Profits df with effect sizes (tau) and std errors
group_p <- c(
  "Avdeenko et al. (2019)",
  "Anderson and McKenzie (2020)",
  "Buvinic et al. (2020)",
  "Alibhai et al. (2019)",
  "Brooks et al. (2018)",
  "Campos et al. (2017)",
  "Bakhtiar et al. (2021)",
  "Anderson et al. (2018) (Finance)",
  "Anderson et al. (2018) (Marketing)",
  "Bruhn and Zia (2013)",
  "Calderon et al. (2020)",
  "de Mel et al. (2014) (Existing)",
  "de Mel et al. (2014) (Potential)",
  "Berge et al. (2015) (Males)",
  "Berge et al. (2015) (Females)",
  "Giné and Mansuri (2020)"
)

tau_p <- c(-23.89, 21.82, 15.23, 7.21, 6.90, 11.18, 80.48, 40.96, 61.06, -15.02, 23.73, -4.27, 41.68, 13.66, 3.60, -8.06)
CIupper_p <- c(12.20, 69.93, 27.12, 16.30, 22.65, 25.06, 125.71, 77.72, 105.22, 32.0, 53.51, 26.31, 82.01, 51.61, 28.88, 9.47) 
CIlower_p <- c(-48.37, -26.29, 3.35, -1.88, -8.84, -2.69, 35.23, 4.20, 16.90, -62.05, -0.26, -34.84, 1.34, -14.8, -21.68, -22.77)     
Female_proportion_p <- c(.683, .49, 1, 1, 1, .53, 1, .44, .44, .35, 1, 1, 1, 0, 1, .49)
Urban_proportion_p <- c(.155, 1, 0, 1, 1, 1, NA, 1, 1, .29, 0, 1, 1, 1, 1, 0)
GDP_pc_growth_p <- c(4, -1.8, 3.8, 6.5, 2.4, 3.2, 7.2, 0.8, 0.8, -2.5, -6.7, 2.8, 2.8, 2.7, 2.7, 2.5)


# Back out std errors
se_p <- compute_se(tau_p, CIupper_p, CIlower_p)

# Combine into df with and without covariates
Profits <- data.frame(group_p, tau_p, se_p)
Profits_expanded <- data.frame(group_p,
                               tau_p,
                               se_p,
                               Female_proportion_p,
                               Urban_proportion_p,
                               GDP_pc_growth_p)

# Export
write.table(Profits,
            file = here("cleaned-datasets", "Profits summary stats.csv"),
            sep = ",",
            row.names = FALSE)

write.table(Profits_expanded,
            file = here("cleaned-datasets", "Profits stats with covariates.csv"),
            sep = ",",
            row.names = FALSE)
