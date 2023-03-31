rm(list=ls()) # Clearing work space

library(lavaan)
library(lavaanPlot)
library(mice)

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Imputation ------------------------------------------------------------------
# Three columns are imputed using Logistic Regressions while the rest of imputed using
# Predictive Means Matching
imputation_method <- c(rep("logreg", 3), rep("pmm", 37))

hrs_imputed <- hrs_data |>
  dplyr::select(!c("HHID", "ID", "Gender", "Birth_year", "Age_w2", "Age_w3")) |>
  dplyr::mutate(Education = as.factor(Education)) |>
  dplyr::mutate(Marital_status = as.factor(Marital_status)) |>
  dplyr::mutate(Job_status = as.factor(Job_status)) |>
  mice(m = 5, maxit = 50, method = imputation_method, seed = 520)


hrs_complete <- complete(hrs_imputed, 2)

# Merging back in the removed columns
hrs_complete <- cbind(
  hrs_data[, c("HHID", "ID", "Gender", "Birth_year", "Age_w2", "Age_w3")],
  hrs_complete
)

# Reverting factor columns back into numeric columns
hrs_complete <- hrs_complete |>
  dplyr::mutate(Education = as.numeric(Education)) |>
  dplyr::mutate(Marital_status = as.numeric(Marital_status)) |>
  dplyr::mutate(Job_status = as.numeric(Job_status)) |>
  dplyr::mutate(across(c(Education, Marital_status, Job_status), ~ ifelse(. == 2, 1, 0)))


# Creating Model --------------------------------------------------------------
pp_model <- '
  # Latent Variables
  # con_w2 =~ Reckless_w2 + Organised_w2 + Hardworking_w2 + Self_disiplined_w2 + Careless_w2 + Cautious_w2 + Thorough_w2 + Thrifty_w2;
  neu_w2 =~ Moody_w2 + Worrying_w2 + Nervous_w2 + Calm_w2;
  dep_w2 =~ Depression_1 + Depression_2 + Depression_3 + Depression_4 + Depression_5 + Depression_6 + Depression_7 + Depression_8;
  lon_w2 =~ Loneliness_1 + Loneliness_2 + Loneliness_3;

  procras_w3 =~ Procras_1 + Procras_2 + Procras_3 + Procras_4 + Procras_5 + Procras_6 + Procras_7 + Procras_8 + Procras_9 + Procras_10 + Procras_11 + Procras_12;
  
  # Residuals
  dep_w2 ~~ lon_w2;
  dep_w2 ~~ neu_w2;
  
  # Direct Effect
  procras_w3 ~ c*Age_w2 + b1*dep_w2 +b2*lon_w2 + Gender + Education + Marital_status + neu_w2;
  
  # A Paths
  dep_w2 ~ a1*Age_w2;
  lon_w2 ~ a2*Age_w2;

  # Indirect effect
  DIDE := a1*b1;
  LIDE := a2*b2;

  Total_Indirect_Effect := (a1*b1) + (a2*b2)
  
  # Total Effect
  
  Total_Effect := c + (a1*b1) + (a2*b2)

'

# Model Fitting ----------------------------------------------------------------
fit <- growth(pp_model, data = hrs_complete, estimator = "ML", missing = "fiml")
summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)

# Exporting -------------------------------------------------------------------
export_path <- "./02__Models/03__Growth_Model/00__Experimenting/"

output <- capture.output(summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE))
writeLines(output, file.path(export_path, "results/01_Model1_Summary.txt"))

# THIS CODE DID WORK AND NOW IT DOESN'T ---------------------------------------
# Pooling the multiple data sets together
# hrs_list <- list()
# 
# for(i in 1:5){
#   hrs_complete_i <- complete(hrs_imputed, i)
#   
#   # Merging back in the removed columns
#   hrs_complete_i <- cbind(
#     hrs_data[, c("HHID", "ID", "Gender", "Birth_year", "Age_w2", "Age_w3")],
#     hrs_complete_i
#   )
#   
#   # Reverting factor columns back into numeric columns
#   hrs_complete_i <- hrs_complete_i |>
#     dplyr::mutate(Education = as.numeric(Education)) |>
#     dplyr::mutate(Marital_status = as.numeric(Marital_status)) |>
#     dplyr::mutate(Job_status = as.numeric(Job_status)) |>
#     dplyr::mutate(across(c(Education, Marital_status, Job_status), ~ ifelse(. == 2, 1, 0)))
#   
#   hrs_list[[i]] <- hrs_complete_i
# }

# fit <- semTools::growth.mi(pp_model, data = hrs_list, estimator = "ML", missing = "fiml")
# summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)
