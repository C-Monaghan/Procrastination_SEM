rm(list=ls()) # Clearing work space

library(dplyr)

path_data <- "./01__Data/02__Processed_data/"

# Reading in data --------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Defining depression cut off --------------------------------------------------
descriptives <- hrs_data %>%
  filter(complete.cases(Marital_status, Job_status)) %>%
  select(c(starts_with("Depression"), starts_with("Procras"))) %>%
  mutate(Total_depression = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
         Total_procrastination = rowSums(select(., starts_with("Procras")), na.rm = TRUE)) %>%
  mutate(across(c("Total_depression",
                  "Total_procrastination"), ~ ifelse(. %in% 0, NA, .))) %>%
  mutate(Depression_binary = ifelse(Total_depression < 11, 0, 1)) %>%
  rowwise() %>%
  mutate(Mean_procrastination = mean(c_across(paste0("Procras_", 1:12)), na.rm = TRUE))


depressed_binary <- descriptives %>%
  select(Mean_procrastination, Depression_binary) %>%
  mutate(Depression_binary = factor(Depression_binary))

# Performing T-Test ------------------------------------------------------------
results <- t.test(Mean_procrastination ~ Depression_binary, 
                  var.equal = TRUE, data = depressed_binary) # Significant effect is present (p < 0.001)

# Performing multiple linear regression ----------------------------------------
model <- lm(Total_procrastination ~ Depression_1 + Depression_2 + Depression_3 + Depression_4 + 
                                    Depression_5 + Depression_6 + Depression_7 + Depression_8,
            data = descriptives)

model_summary <- summary(model)

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/04__Exploratory/"

output_1 <- capture.output(results)
output_2 <- capture.output(model_summary)

writeLines(output_1, file.path(export_path, "results/04__t_test_results.txt"))
writeLines(output_2, file.path(export_path, "results/05__regression_results.txt"))


