rm(list=ls()) # Clearing work space

library(dplyr)
library(ggplot2)

path_data <- "./01__Data/02__Processed_data/"

# Reading in data --------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Creating descriptive --------------------------------------------------------
descriptives <- hrs_data %>%
  filter(complete.cases(Marital_status, Job_status)) %>%
  mutate(age_group = cut(Age_w2, breaks = c(28, 40, 50, 60, 70, 80, 92),
                         labels = c("28-40", "41-50", "51-60", "61-70", "71-80", "80+"),
                         include.lowest = TRUE)) %>%
  mutate(Depression_total = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
         Loneliness_total = rowSums(select(., starts_with("Loneliness")), na.rm = TRUE),
         Procrastination_total = rowSums(select(., starts_with("Procras")), na.rm = TRUE)) %>%
  mutate(across(c("Depression_total", "Loneliness_total", "Procrastination_total"), 
                ~ if_else(. %in% 0, NA, .))) %>%
  select(Age_w2, age_group, Depression_total, Loneliness_total, Procrastination_total)

# Creating dataset with means --------------------------------------------------
means <- descriptives %>%
  group_by(age_group) %>%
  summarise(Procrastination = mean(Procrastination_total, na.rm = TRUE),
            Depression = mean(Depression_total, na.rm = TRUE),
            Loneliness = mean(Loneliness_total, na.rm = TRUE)) %>%
  tidyr::pivot_longer(cols = -age_group,
                      names_to = "variable",
                      values_to = "mean_score")

# Plotting graphs --------------------------------------------------------------
distribution <- ggplot(descriptives, aes(x = Procrastination_total)) +
  geom_density(alpha = 0.7, fill = "#8F0A0A") +
  facet_wrap(~ age_group) +
  labs(title = "Distribution of Procrastination Scores Across Age",
       x = "Total Procrastination",
       y = "Density") +
  theme_bw() +
  ggeasy::easy_center_title()

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/03__SEM/"

cowplot::save_plot(filename = file.path(export_path, "results/03__distribution.png"),
                   plot = distribution)
