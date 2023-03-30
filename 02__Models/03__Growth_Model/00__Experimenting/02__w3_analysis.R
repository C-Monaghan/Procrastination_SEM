rm(list=ls()) # Clearing work space

library(lavaan)
library(lavaanPlot)
library(mice)

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal(2).xlsx"))

# Imputation ------------------------------------------------------------------
# hrs_imputed <- hrs_data |>
#   dplyr::select(!c("HHID", "ID", "Gender", "Birth_year", "Age_w2", "Age_w3")) |>
#   mice(m = 5, maxit = 50, method = "pmm", seed = 580)

# Three columns are imputed using Logistic Regressions while the rest of imputed using
# Predictive Means Matching
imputation_method <- c(rep("logreg", 3), rep("pmm", 39))

hrs_imputed <- hrs_data |>
  dplyr::select(!c("HHID", "ID", "Gender", "Birth_year", "Age_w2", "Age_w3")) |>
  dplyr::mutate(Education = as.factor(Education)) |>
  dplyr::mutate(Marital_status = as.factor(Marital_status)) |>
  dplyr::mutate(Job_status = as.factor(Job_status)) |>
  mice(m = 5, maxit = 50, method = imputation_method, seed = 600)

hrs_complete <- complete(hrs_imputed, 1)

# Merging back in the removed columns
hrs_complete <- cbind(
  hrs_data[, c("HHID", "ID", "Gender", "Birth_year", "Age_w2", "Age_w3")],
  hrs_complete
)

# # Reverting factor columns back into numeric columns
hrs_complete <- hrs_complete |>
  dplyr::mutate(Education = as.numeric(Education)) |>
  dplyr::mutate(Marital_status = as.numeric(Marital_status)) |>
  dplyr::mutate(Job_status = as.numeric(Job_status)) |>
  dplyr::mutate(Education = ifelse(Education == 2, 1, 0)) |>
  dplyr::mutate(Marital_status = ifelse(Marital_status ==  2, 1, 0)) |>
  dplyr::mutate(Job_status = ifelse(Job_status ==  2, 1, 0))

# Creating Model --------------------------------------------------------------
pp_model <- '
  # Latent Variables
  con =~ Reckless + Organised + Hardworking + Self_disiplined + Careless + Cautious + Thorough + Thrifty;
  neu =~ Moody + Worrying + Nervous + Calm;
  anx =~ Anxiety_1 + Anxiety_2 + Anxiety_3 + Anxiety_4 + Anxiety_5;
  dep =~ Depression_1 + Depression_2 + Depression_3 + Depression_4 + Depression_5 + Depression_6 + Depression_7 + Depression_8;
  
  procras_w3 =~ Procras_1 + Procras_2 + Procras_3 + Procras_4 + Procras_5 + Procras_6 + Procras_7 + Procras_8 + Procras_9 + Procras_10 + Procras_11 + Procras_12;
  
  # Direct Effect
  procras_w3 ~ c*Age_w3 + b1*con + b2*neu + Gender + Marital_status + Education + dep + anx;
  
  # A Paths
  con ~ a1*Age_w3;
  neu ~ a2*Age_w3;

  # Indirect effect
  CIDE := a1*b1;
  NIDE := a2*b2;

  Total_Indirect_Effect := (a1*b1) + (a2*b2)
  
  # Total Effect
  
  Total_Effect := c + (a1*b1)+ (a2*b2)

'

# Model Fitting ----------------------------------------------------------------
fit <- sem(pp_model, data = hrs_complete, estimator = "ML", missing = "fiml")
summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)

# Exporting -------------------------------------------------------------------
export_path <- "./02__Models/03__Growth_Model/00__Experimenting/"

output <- capture.output(summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE))
writeLines(output, file.path(export_path, "results/02_Model2_Summary.txt"))

