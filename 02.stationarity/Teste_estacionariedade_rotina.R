##### Load necessary libraries ####
library(tseries)
library(tidyverse)
library(readr)
library(tseries)  # For Augmented Dickey-Fuller and Phillips-Perron tests
library(urca)     # For KPSS test
library(readr)    # For reading CSV files
library(dplyr)    # For data manipulation
library(knitr)
library(kableExtra)
library(rbcb)



##### Load the data ####
data0 <- read_delim("F:/Educacao/Pós Graduações e Graduações/Mestrado UNB/dissertacao/inflation_forecast_integration/00.data/dataset.csv", 
                    delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip(), 
                                                                         data = col_date(format = "%d/%m/%Y")), 
                    trim_ws = TRUE)
 
data <-  data0 %>%
  select(-c(1))


##### Function to apply stationarity tests and return results with combined decision #####
apply_tests <- function(series) {
  adf_test <- adf.test(series, alternative = "stationary", k = trunc((length(series)-1)^(1/3)))
  pp_test <- pp.test(series, alternative = "stationary")
  kpss_test <- ur.kpss(series)
  
  # Decision criteria (change the threshold as needed)
  threshold <- 0.05
  adf_decision <- adf_test$p.value < threshold
  pp_decision <- pp_test$p.value < threshold
  # For KPSS, the p-value is extracted differently
  kpss_decision <- kpss_test@cval[1,] < kpss_test@teststat
  
  # Combined decision
  combined_decision <- ifelse(adf_decision && pp_decision && !kpss_decision, "accepted", "rejected")
  
  return(c(adf_p = as.numeric(adf_test$p.value), 
           pp_p = as.numeric(pp_test$p.value), 
           kpss_stat = as.numeric(kpss_test@teststat), 
           Stationarity_Decision = combined_decision))
}

# Apply the tests to each column
results <- apply(data, 2, apply_tests)

# Create a results table
results_table <- as.data.frame(t(results)) 
names(results_table) <- c("ADF", "PP", "KPSS", "Stationarity Decision")
results_table$Series <- rownames(results_table)
# results_table %>% results_table %>%
#   relocate(Series)
rownames(results_table) <- NULL

# Count the number of accepted and rejected time series
decision_counts <- table(results_table$Stationarity_Decision)
# Print the counts
print(decision_counts)

# Export the results
write.csv(results_table, "stationarity_test_results.csv", row.names = FALSE)


#### Selecting stationary time series based on accepted ####
accepted_series <- results_table %>%
  filter(`Stationarity Decision` == "accepted")

accepted_series_vector <- accepted_series$Series

accepted <- data[,accepted_series_vector]
accepted$data <- data0$data
name_accepted <- names(accepted)

#### Dummy for structural Breask ####
accepted$dummy <- 0 
#accepted$dummy[which(accepted$data >= "2016-05-01")] <- 1
accepted$dummy[which(accepted$data >= as.Date("2016-05-01") & accepted$data <= as.Date("2020-07-01"))] <- 1

# Export the results
write.csv(accepted, "stationarity_accepted.csv", row.names = FALSE)
write.csv(name_accepted, "name_stationarity_accepted.csv", row.names = FALSE)

##### Convert p-value columns to numeric and then round them ####
results_table$ADF <- round(as.numeric(as.character(results_table$ADF)), 2)
results_table$PP <- round(as.numeric(as.character(results_table$PP)), 2)
results_table$KPSS <- round(as.numeric(as.character(results_table$KPSS)), 2)

# If you also have a KPSS_stat column and you want to round it
if("KPSS_stat" %in% colnames(results_table)) {
  results_table$KPSS_stat <- round(as.numeric(as.character(results_table$KPSS_stat)), 2)
}

##### Format the results_table ####
formatted_table <- kable(results_table, format = "html", 
                         caption = "Results of Stationarity Tests (P Value)",
                         align = 'c', booktabs = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  footnote(general = c("ADF = Augmented Dickey-Fuller, PP = Phillips-Perron, and KPSS = Kwiatkowski-Phillips-Schmidt-Shin",
                       "p-value < 0.01 suggests stationarity for the ADF and PP tests, while for the KPSS test indicates non-stationarity.",
                       "The clolumn Stationarity Decision means that the serie is stationary (accepted) or non-stationary (rejected) based on the result for the three tests."))

# Display the table
formatted_table




#####################
# Salve tabela criada no formato desejado na aba "Viewer" do RStudio.
# Consulta CHATGPT: https://chat.openai.com/g/g-HMNcP6w7d-data-analyst/c/d22c3de4-1095-4ef6-bc00-2d6a347b3480
#####################


library(knitr)
library(kableExtra)

# Assuming results_table is your data frame

# Determine the number of rows per table
rows_per_table <- 100  # Adjust this based on your needs

# Create a sequence for splitting the table
split_seq <- seq(1, nrow(results_table), by = rows_per_table)

# Iterate over the splits and create individual HTML tables
html_tables <- lapply(split_seq, function(start_row) {
  end_row <- min(start_row + rows_per_table - 1, nrow(results_table))
  
  # Subset the table for this chunk
  subset_table <- results_table[start_row:end_row, ]
  
  # Format the table chunk
  kable(subset_table, format = "html", 
        caption = paste("Results of Stationarity Tests (Rows", start_row, "to", end_row, ")"),
        align = 'c', booktabs = TRUE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    footnote(general = c("ADF = Augmented Dickey-Fuller, PP = Phillips-Perron, and KPSS = Kwiatkowski-Phillips-Schmidt-Shin",
                         "p-value < 0.01 or p-value = 0.01 suggests stationarity for the ADF and PP tests, while for the KPSS test indicates non-stationarity.",
                         "The clolumn Stationarity Decision means that the serie is stationary (accepted) or non-stationary (rejected) based on the result for the three tests."))
})

# View the first table as an example
html_tables[[5]]