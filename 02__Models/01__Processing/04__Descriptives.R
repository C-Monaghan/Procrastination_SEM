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

# Creating a descriptive table -----------------------------------------------
names <- list(
  Age_w2 = "Age", Procrastination_total = "Procrastination", Depression_total = "Depression", 
  Health_assessment = "Health", Education = "Education", Job_status = "Retired (Yes/No)", 
  Marital_status = "Married (Yes/No)"
  )
stat <- list(
  all_continuous() ~ c("{mean}, ({sd})", "{median}", "{min}, {max}"),
  all_categorical() ~ "{n} / {N} ({p}%)"
  )


hrs_data %>%
  select(Age_w2, Procrastination_total, Depression_total, 
         Health_assessment, Education, Job_status, 
         Marital_status) %>%
  tbl_summary(type = all_continuous() ~ "continuous2",
              label = names, 
              statistic = stat, 
              digits = all_continuous() ~ 2, 
              missing = "no") %>%
  bold_labels()

hrs_data %>%
  select(Age_w2, Procrastination_total, Depression_total, 
         Health_assessment, Education, Job_status, 
         Marital_status) %>%
  tbl_summary(type = all_continuous() ~ "continuous2",
              label = names, 
              statistic = all_continuous() ~ "{mean}, ({sd})", 
              digits = all_continuous() ~ 2, 
              missing = "no") %>%
  add_overall(last = TRUE,
              statistics = all_continuous() ~ "{median}",
              digits = all_continuous() ~ 2)





