# The below code generated descriptive statistics
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

library(dplyr)
library(ggplot2)

path_data <- "./01__Data/02__Processed_data/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Testing ---------------------------------------------------------------------
test_data <- hrs_data %>%
  filter(complete.cases(Marital_status, Job_status)) %>%
  mutate(Procrastination_total = rowSums(select(., starts_with("Procras")), na.rm = TRUE)) %>%
  mutate(across("Procrastination_total", ~ if_else(. %in% 0, NA, .))) %>%
  select(Age_w2, Education, Marital_status, Job_status, Procrastination_total) %>%
  mutate(Education = as.factor(Education), Marital_status = as.factor(Marital_status), 
         Job_status = as.factor(Job_status))

ggplot(data = test_data, aes(x = Education, y = Procrastination_total, fill = Marital_status)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  facet_wrap(~ Job_status) +
  labs(x = "College Education", y = "Mean Procrastination Score", fill = "Marital Status") +
  scale_fill_manual(values = c("darkblue", "lightblue")) +
  theme_minimal()

# Getting descriptives
hrs_data_descript <- hrs_data %>%
  filter(complete.cases(Marital_status, Job_status)) %>%
  mutate(Depression_total = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
         Loneliness_total = rowSums(select(., starts_with("Loneliness")), na.rm = TRUE),
         Procrastination_total = rowSums(select(., starts_with("Procras")), na.rm = TRUE)) %>%
  mutate(across(c("Depression_total", "Loneliness_total", "Procrastination_total"), 
                ~ if_else(. %in% 0, NA, .))) %>%
  select(Age_w2, Depression_total, Loneliness_total, Procrastination_total)

hrs_data_descript %>%
  summarise(mean_age = mean(Age_w2, na.rm = TRUE),
            mean_depression = mean(Depression_total, na.rm = TRUE),
            mean_loneliness = mean(Loneliness_total, na.rm = TRUE),
            mean_procrastination = mean(Procrastination_total, na.rm = TRUE))


t.test(hrs_data_descript$Loneliness_total)$conf.int


stderror <- function(x) sd(x)/sqrt(length(x))

stderror(hrs_data_descript$Procrastination_total)


c <- hrs_data_descript$Procrastination_total

c <- select(complete.cases(c) == TRUE)

plotrix::std.error(hrs_data_descript$Loneliness_total, na.rm = TRUE)



median(hrs_data_descript$Loneliness_total, na.rm = TRUE)


sd(hrs_data_descript$Loneliness_total, na.rm = TRUE)
