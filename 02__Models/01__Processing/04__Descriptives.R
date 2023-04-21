# The below code generated descriptive statistics
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

library(dplyr)
library(gtsummary)

path_data <- "./01__Data/02__Processed_data/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Creating initial total columns ----------------------------------------------
hrs_data_descript <- hrs_data %>%
  mutate(Depression_total = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
         Loneliness_total = rowSums(select(., starts_with("Loneliness")), na.rm = TRUE),
         Procrastination_total = rowSums(select(., starts_with("Procras")), na.rm = TRUE)) %>%
  mutate(across(c("Depression_total", "Loneliness_total", "Procrastination_total"), 
                ~ if_else(. %in% 0, NA, .))) %>%
  select(Gender, Age_w2, Education, Marital_status, Job_status, 
         Depression_total, Loneliness_total, Procrastination_total)


# Creating a descriptive table -----------------------------------------------
names <- list(
  Age_w2 = "Age", Procrastination_total = "Procrastination", Depression_total = "Depression", 
  Loneliness_total = "Loneliness", Education = "Education", Job_status = "Retired (Yes/No)", 
  Marital_status = "Married (Yes/No)"
  )
stat <- list(
  all_continuous() ~ c("{mean}, ({sd})", "{median}", "{min}, {max}"),
  all_categorical() ~ "{n} / {N} ({p}%)"
  )


hrs_data_descript %>%
  tbl_summary(type = list(Age_w2 ~ "continuous2",
                          Procrastination_total ~ "continuous2",
                          Depression_total ~ "continuous2",
                          Loneliness_total ~ "continuous2"),
              label = names, 
              statistic = stat, 
              digits = all_continuous() ~ 2, 
              missing = "no") %>%
  bold_labels()





