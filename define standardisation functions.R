# Define modular functions to:
# 1. standardise profits and sales using control mean and sd
# 2. calculate z-scores
# in each case, allow for baseline profits/revenues to be a separate column

# 1. Define a function to calculate means and standard deviations for control group
standardize_data <- function(data, treatment_col, sales_col, profits_col, baseline_sales_col = NULL) {
  if (!is.null(baseline_sales_col)) {
    data %>%
      group_by(!!sym(treatment_col)) %>%
      summarize(
        avgprofits = mean(!!sym(profits_col), na.rm = TRUE),
        sdprofits = sd(!!sym(profits_col), na.rm = TRUE),
        avgrevenue = mean(!!sym(sales_col), na.rm = TRUE),
        sdrevenue = sd(!!sym(sales_col), na.rm = TRUE),
        avgbaselinerevenue = mean(!!sym(baseline_sales_col), na.rm = TRUE),
        sdbaselinerevenue = sd(!!sym(baseline_sales_col), na.rm = TRUE)
      ) %>%
      filter(!!sym(treatment_col) == 4) %>%
      select(-!!sym(treatment_col))
  } else {
    data %>%
      group_by(!!sym(treatment_col)) %>%
      summarize(
        avgprofits = mean(!!sym(profits_col), na.rm = TRUE),
        sdprofits = sd(!!sym(profits_col), na.rm = TRUE),
        avgrevenue = mean(!!sym(sales_col), na.rm = TRUE),
        sdrevenue = sd(!!sym(sales_col), na.rm = TRUE)
      ) %>%
      filter(!!sym(treatment_col) == 4) %>%
      select(-!!sym(treatment_col))
  }
}

# 2. Define a function to calculate z-scores
calculate_z_scores <- function(data, sales_col, profits_col, baseline_sales_col = NULL) {
  if (!is.null(baseline_sales_col)) {
    data %>%
      mutate(
        Profit_zscore = (!!sym(profits_col) - avgprofits) / sdprofits,
        Revenue_zscore = (!!sym(sales_col) - avgrevenue) / sdrevenue,
        Baseline_Revenue_z = (!!sym(baseline_sales_col) - avgbaselinerevenue) / sdbaselinerevenue
      )
  } else {
    data %>%
      mutate(
        Profit_zscore = (!!sym(profits_col) - avgprofits) / sdprofits,
        Revenue_zscore = (!!sym(sales_col) - avgrevenue) / sdrevenue
      )
  }
}
