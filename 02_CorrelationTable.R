# Extract Correlation Parameters


library(dplyr)
library(broom)
library(tidyr)

# Function to perform correlation test and return tidy results
cor_test_tidy <- function(data, x, y, region = NULL) {
  cor_test <- cor.test(data[[x]], data[[y]], method = "spearman", exact = FALSE)
  tidy_result <- tidy(cor_test) %>%
    mutate(
      x_var = x,
      y_var = y,
      region = region
    ) %>%
    select(region, x_var, y_var, estimate, p.value)
  return(tidy_result)
}

# Function to extract correlation parameters
extract_correlations <- function(data, x_var, y_var, region_col = "Region", region_order = c("EI", "RR", "HM", "Overall")) {
  correlation_table <- bind_rows(
    data %>%
      group_by(!!sym(region_col)) %>%
      group_modify(~cor_test_tidy(.x, x_var, y_var, .y[[region_col]])),
    cor_test_tidy(data, x_var, y_var, "Overall")
  ) %>%
    arrange(factor(region, levels = region_order))
  
  return(correlation_table)
}

# Create a table for each correlations :
 correlation_table_1 <- extract_correlations(qc_subset, "DHg_Mean", "DOC_Mean")
 correlation_table_2 <- extract_correlations(qc_subset, "DHg_Mean", "C1")
 correlation_table_3 <- extract_correlations(qc_subset, "DHg_Mean", "C4")
 correlation_table_4 <- extract_correlations(qc_subset, "DMeHg_DHg_ratio", "DOC_Mean")
 correlation_table_5 <- extract_correlations(qc_subset, "DMeHg_DHg_ratio", "C1")
 correlation_table_6 <- extract_correlations(qc_subset, "DMeHg_DHg_ratio", "C4")
 
 correlation_table_7 <- extract_correlations(qc_subset, "DMeHg_DHg_DMeHG_ratio","Cgas_DOC_ratio")
 correlation_table_8 <- extract_correlations(qc_subset, "DMeHg_Mean", "CH4_plus_MOX")
 correlation_table_9 <- extract_correlations(qc_subset, "DMeHg_Mean", "CH4_mgCL")
 
 #Combine 
 correlation_table_all= rbind(correlation_table_1, correlation_table_2,correlation_table_3,
                              correlation_table_4,correlation_table_5,correlation_table_6,
                              correlation_table_7,correlation_table_8, correlation_table_9)
 
# Display the table
print(correlation_table_all)

# to save these tables to CSV files, uncomment the following lines:
4# write.csv(correlation_table_1, "correlation_results.csv", row.names = FALSE)
