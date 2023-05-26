# The below code generated descriptive statistics
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

library(dplyr)
library(ggplot2)

path_data <- "./01__Data/02__Processed_data/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Data manipulation -----------------------------------------------------------
# Initially, we want to remove the participants with missing values in their covariates
# Then we create summation columns for our dependent variables, setting those columns with
# a value of 0 to NA, and then selecting only the necessary data columns
hrs_data_descript <- hrs_data %>%
  filter(complete.cases(Marital_status, Job_status)) %>%
  mutate(Depression_total = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
         Loneliness_total = rowSums(select(., starts_with("Loneliness")), na.rm = TRUE),
         Procrastination_total = rowSums(select(., starts_with("Procras")), na.rm = TRUE)) %>%
  mutate(across(c("Depression_total", "Loneliness_total", "Procrastination_total"), 
                ~ if_else(. %in% 0, NA, .))) %>%
  select(Gender, Age_w2, Education, Marital_status, Job_status, 
         Depression_total, Loneliness_total, Procrastination_total)

# Calculate mean scores for numerical variables
hrs_data_descript %>%
  summarise(mean_depression = mean(Depression_total, na.rm = TRUE),
            mean_loneliness = mean(Loneliness_total, na.rm = TRUE),
            mean_procrastination = mean(Procrastination_total, na.rm = TRUE))

hrs_data_descript %>%
  summarise(sd_gender = sd(Gender, na.rm = TRUE))

# Testing ---------------------------------------------------------------------
hrs_data %>%
  filter(complete.cases(Marital_status, Job_status)) %>%
  mutate(Procrastination_total = rowSums(select(., starts_with("Procras")), na.rm = TRUE)) %>%
  mutate(across("Procrastination_total", ~ if_else(. %in% 0, NA, .))) %>%
  select(Age_w2, Procrastination_total) %>%
  group_by(Age_w2) %>%
  summarise(mean_procrastination = mean(Procrastination_total, na.rm = TRUE)) %>%
  filter(complete.cases(mean_procrastination)) %>%
  ggplot(aes(x = Age_w2, y = mean_procrastination)) +
  geom_line() +
  scale_x_continuous(breaks = seq(25, 95, by = 10))

