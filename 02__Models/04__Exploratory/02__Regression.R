rm(list=ls()) # Clearing work space

library(dplyr)
library(ggplot2)

path_data <- "./01__Data/02__Processed_data/"

# Reading in data --------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Creating various summary datasets --------------------------------------------
# Depression cut off
descriptives <- hrs_data %>%
  filter(complete.cases(Marital_status, Job_status)) %>%
  select(c(starts_with("Depression"), starts_with("Procras"))) %>%
  mutate(Total_depression = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
         Total_procrastination = rowSums(select(., starts_with("Procras")), na.rm = TRUE)) %>%
  mutate(across(c("Total_depression",
                  "Total_procrastination"), ~ ifelse(. %in% 0, NA, .))) %>%
  mutate(Depression_binary = ifelse(Total_depression < 11, 0, 1),
         Depression_binary = factor(Depression_binary), .before = "Depression_1") %>%
  rowwise() %>%
  mutate(Mean_procrastination = round(mean(
    c_across(paste0("Procras_", 1:12)), na.rm = TRUE), 
    digits = 2))

# Descriptives per symptom
summary_data <- descriptives %>%
select(starts_with("Depression"), Total_procrastination, -Depression_binary) %>%
  tidyr::pivot_longer(cols = starts_with("Depression"), 
                      names_to = "Symptom", 
                      values_to = "Present") %>%
  mutate(Symptom = factor(case_when(
    Symptom == "Depression_1" ~ "Depression",
    Symptom == "Depression_2" ~ "Fatigue",
    Symptom == "Depression_3" ~ "Restlessness",
    Symptom == "Depression_4" ~ "Lack of Happiness",
    Symptom == "Depression_5" ~ "Loneliness",
    Symptom == "Depression_6" ~ "Lack of Enjoyment",
    Symptom == "Depression_7" ~ "Sadness",
    Symptom == "Depression_8" ~ "Unmotivated"
  ), levels = c("Depression", "Fatigue", "Restlessness", "Lack of Happiness",
                "Loneliness", "Lack of Enjoyment", "Sadness", "Unmotivated"))) %>%
  filter(complete.cases(Present)) %>%
  group_by(Symptom, Present) %>%
  summarize(mean_procrastination = round(mean(
    Total_procrastination, na.rm = TRUE), 
    digits = 2)) %>%
  mutate(Present = ifelse(Present == 1, "No", "Yes"),
         Present = factor(Present))

# Performing T-Test ------------------------------------------------------------
results <- t.test(Mean_procrastination ~ Depression_binary, 
                  var.equal = TRUE, data = descriptives) # Significant effect is present (p < 0.001)

# Performing multiple linear regression ----------------------------------------
model <- lm(Mean_procrastination ~ Depression_1 + Depression_2 + Depression_3 + Depression_4 + 
                                    Depression_5 + Depression_6 + Depression_7 + Depression_8,
            data = descriptives)

model_summary <- summary(model)

# Plotting ---------------------------------------------------------------------
# Mean Procrastination Scores Per Symptom
symptom_plot <- ggplot(summary_data, aes(x = Symptom, y = mean_procrastination, fill = Present)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Mean Procrastination Scores", 
       title = "Mean Procrastination Scores Per Symptom") +
  theme_bw() +
  ggeasy::easy_center_title() +
  ggeasy::easy_add_legend_title("Symptom Present ") +
  ggeasy::easy_move_legend(to = c("bottom"))

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/04__Exploratory/"

output_1 <- capture.output(results)
output_2 <- capture.output(model_summary)

writeLines(output_1, file.path(export_path, "results/02__Regression/01__t_test_results.txt"))
writeLines(output_2, file.path(export_path, "results/02__Regression/02__regression_results.txt"))

cowplot::save_plot(filename = file.path(export_path, "results/02__Regression/03__symptom_plot.png"),
                   plot = symptom_plot, base_height = 7)
  
