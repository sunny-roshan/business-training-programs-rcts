# Define modular functions to standardise profits and sales using control mean and sd and calculate z-scores,
# allowing for baseline revenues/profits to be a separate column

# 1. Define a function to calculate means and standard deviations for control group
standardize_data <- function(data, treatment_col, sales_col, profits_col, baseline_sales_col = NULL, baseline_profits_col = NULL) {
  summarization <- data %>%
    group_by(!!sym(treatment_col)) %>%
    summarize(
      avgprofits = mean(!!sym(profits_col), na.rm = TRUE),
      sdprofits = sd(!!sym(profits_col), na.rm = TRUE),
      avgrevenue = mean(!!sym(sales_col), na.rm = TRUE),
      sdrevenue = sd(!!sym(sales_col), na.rm = TRUE)
    )
  
  if (!is.null(baseline_sales_col) && baseline_sales_col %in% colnames(data)) {
    summarization <- summarization %>%
      mutate(
        avgbaselinerevenue = mean(!!sym(baseline_sales_col), na.rm = TRUE),
        sdbaselinerevenue = sd(!!sym(baseline_sales_col), na.rm = TRUE)
      )
  }
  
  if (!is.null(baseline_profits_col) && baseline_profits_col %in% colnames(data)) {
    summarization <- summarization %>%
      mutate(
        avgbaselineprofits = mean(!!sym(baseline_profits_col), na.rm = TRUE),
        sdbaselineprofits = sd(!!sym(baseline_profits_col), na.rm = TRUE)
      )
  }
}

# 2. Define a function to calculate z-scores
calculate_z_scores <- function(data, sales_col, profits_col, baseline_sales_col = NULL, baseline_profits_col = NULL) {
  data <- data %>%
    mutate(
      Profit_zscore = (!!sym(profits_col) - avgprofits) / sdprofits,
      Revenue_zscore = (!!sym(sales_col) - avgrevenue) / sdrevenue
    )
  
  if (!is.null(baseline_sales_col) && baseline_sales_col %in% colnames(data)) {
    data <- data %>%
      mutate(
        Baseline_Revenue_z = (!!sym(baseline_sales_col) - avgbaselinerevenue) / sdbaselinerevenue
      )
  }
  
  if (!is.null(baseline_profits_col) && baseline_profits_col %in% colnames(data)) {
    data <- data %>%
      mutate(
        Baseline_Profit_z = (!!sym(baseline_profits_col) - avgbaselineprofits) / sdbaselineprofits
      )
  }
  
  data  # Explicitly return the modified data frame
}
