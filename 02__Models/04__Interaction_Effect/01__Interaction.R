# Come back to this -----------------------------------------------------------

rm(list=ls()) # Clearing work space

library(lavaan)
library(dplyr)
library(ggplot2)

path_data <- "./01__Data/02__Processed_data/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Interaction Model -----------------------------------------------------------
interaction_model <- '
  # LATENT VARIABLES
  procras_w3 =~ Procras_1 + Procras_2 + Procras_3 + Procras_4 + Procras_5 + Procras_6 + Procras_7 + Procras_8 + Procras_9 + Procras_10 + Procras_11 + Procras_12;

  # INTERACTION EFFECT
  procras_w3 ~ a*Age_w2 + b*Education + c*Age_w2*Education;

  '

# Model Fitting ----------------------------------------------------------------
fit <- sem(interaction_model, data = hrs_data)
summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)


# Plotting -------------------------------------------------------------------
# Creating initial total columns
hrs_data <- hrs_data %>%
  mutate(Depression_total = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
         Procrastination_total = rowSums(select(., starts_with("Procras")), na.rm = TRUE))

# Any total columns equal to 0 are assigned as missing (NA)
hrs_data <- hrs_data %>%
  mutate(across(c("Depression_total",
                  "Procrastination_total"), ~ if_else(. %in% 0, NA, .)))

ggplot(hrs_data, aes(x = Age_w2, y = Procrastination_total, colour = Education)) +
  geom_point() +
  labs(title = "Interaction Effect of Age and Education Level on Procrastination",
       x = "Age", y = "Procrastination",
       color = "Education Level")


model <- lm(Procrastination_total ~ Age_w2 + Education + Age_w2*Education, data = hrs_data)
summary(model, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)
