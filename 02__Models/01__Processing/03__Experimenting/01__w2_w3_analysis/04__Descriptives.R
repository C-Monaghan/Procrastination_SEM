# The below code generated descriptive statistics
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

library(dplyr)
library(gtsummary)

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Creating initial total columns ----------------------------------------------
hrs_data <- hrs_data %>%
  mutate(Depression_total = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
         Procrastination_total = rowSums(select(., starts_with("Procras")), na.rm = TRUE))

# Any total columns equal to 0 are assigned as missing (NA)
hrs_data <- hrs_data %>%
  mutate(across(c("Depression_total",
                  "Procrastination_total"), ~ if_else(. %in% 0, NA, .)))

# Generating descriptive
# Mean, Median, Range
summary(select(hrs_data, Age_w2, Health_assessment, Procrastination_total, Depression_total))

# Standard Deviations
sd <- summarise(select(hrs_data, Age_w2, Health_assessment, Procrastination_total, Depression_total),
                across(everything(), sd, na.rm = TRUE))

# Standard error of mean
hrs_data %>%
  select(Age_w2, Health_assessment, Procrastination_total, Depression_total) %>%
  apply(2, function(x) sd(x, na.rm = TRUE)/sqrt(length(x)))

# Frequency table for categorical variables
hrs_data %>%
  select(Education, Marital_status, Job_status) %>%
  table()


d_table <- tbl_summary(
  hrs_data %>%
    select(Age_w2, Procrastination_total) %>%
    summarise(mean = mean(c(Age_w2, Procrastination_total))))

print()
