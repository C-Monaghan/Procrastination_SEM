rm(list=ls()) # Clearing work space

library(lavaan)
library(lavaanPlot)
library(mice)

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Imputation ------------------------------------------------------------------
hrs_imputed <- hrs_data |>
  dplyr::select(!c("HHID", "ID", "Gender", "Birth_year", "Education", "Marital_status", "Age_w2", "Age_w3")) |>
  mice(m = 5, maxit = 50, method = "pmm", seed = 420)

hrs_complete <- complete(hrs_imputed, 1)

# Merging back in the removed columns
hrs_complete <- cbind(
  hrs_data[, c("HHID", "ID", "Gender", "Birth_year", "Education", "Marital_status", "Age_w2", "Age_w3")],
  hrs_complete
)

# Creating Model --------------------------------------------------------------
pp_model <- '
  # Latent Variables
  con_w2 =~ Reckless_w2 + Organised_w2 + Hardworking_w2 + Self_disiplined_w2 + Careless_w2 + Cautious_w2 + Thorough_w2 + Thrifty_w2;
  neu_w2 =~ Moody_w2 + Worrying_w2 + Nervous_w2 + Calm_w2;
  
  procras_w3 =~ Procras_1 + Procras_2 + Procras_3 + Procras_4 + Procras_5 + Procras_6 + Procras_7 + Procras_8 + Procras_9 + Procras_10 + Procras_11 + Procras_12;
  
  # Direct Effect
  procras_w3 ~ c*Age_w2 + b1*con_w2 + b2*neu_w2 + Gender;
  
  # A Paths
  con_w2 ~ a1*Age_w2;
  neu_w2 ~ a2*Age_w2;

  # Indirect effect
  CIDE := a1*b1;
  NIDE := a2*b2;

  Total_Indirect_Effect := (a1*b1) + (a2*b2)
  
  # Total Effect
  
  Total_Effect := c + (a1*b1)+ (a2*b2)

'

# Model Fitting ----------------------------------------------------------------
fit <- growth(pp_model, data = hrs_complete, estimator = "ML", missing = "fiml")
summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)


# Exporting -------------------------------------------------------------------
export_path <- "./02__Models/03__Growth_Model/00__Experimenting/"

output <- capture.output(summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE))
writeLines(output, file.path(export_path, "results/01_Model1_Summary.txt"))
