rm(list=ls()) # Clearing work space

library(lavaan)
library(lavaanPlot)

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Creating Model --------------------------------------------------------------
pp_model <- '
  # Latent Variables
  dep_w2 =~ Depression_1 + Depression_2 + Depression_3 + Depression_4 + Depression_5 + Depression_6 + Depression_7 + Depression_8;
  lon_w2 =~ Loneliness_1 + Loneliness_2 + Loneliness_3;
  procras_w3 =~ Procras_1 + Procras_2 + Procras_3 + Procras_4 + Procras_5 + Procras_6 + Procras_7 + Procras_8 + Procras_9 + Procras_10 + Procras_11 + Procras_12;
  
  # Residuals
  # Find a reason to correlate these varibales
  dep_w2 ~~ lon_w2;
  
  # Individual question items
  Procras_1 ~~ Procras_2;
  Procras_10 ~~ Procras_11;
  Depression_1 ~~ Depression_7;
  Depression_4 ~~ Depression_6;
  
  # Direct Effect
  procras_w3 ~ c*Age_w2 + b1*dep_w2 +b2*lon_w2 + Gender + Education + Marital_status + Job_status + Life_satisfaction + Health_assessment;
  
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
fit <- growth(pp_model, data = hrs_data, estimator = "ML", missing = "fiml")
summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)


# Plotting --------------------------------------------------------------------
# Creating a list of names 
sem_names <- list(Age_w2 = "Age", dep_w2 = "Depression", lon_w2 = "Loneliness", procras_w3 = "Procrastination",
                   Depression_1 = "D1", Depression_2 = "D2", Depression_3 = "D3", Depression_4 = "D4",
                   Depression_5 = "D5", Depression_6 = "D6", Depression_7 = "D7", Depression_8 = "D8",
                   Loneliness_1 = "L1", Loneliness_2 = "L2", Loneliness_3 = "L3",
                   Procras_1 = "P1", Procras_2 = "P2", Procras_3 = "P3", Procras_4 = "P4", Procras_5 = "P5",
                   Procras_6 = "P6", Procras_7 = "P7", Procras_8 = "P8", Procras_9 = "P9", Procras_10 = "P10",
                   Procras_11 = "P11", Procras_12 = "P12",
                   Gender = "Gender", Education = "Education", Marital_status = "MS", Job_status = "JS",
                   Life_satisfaction = "LS", Health_assessment = "HS")

# Creating SEM Plot
sem_plot <- lavaanPlot(model = fit, labels = sem_names, 
                       node_options = list(shape = "box", fontname = "Helvetica"), 
                       edge_options = list(color = "grey"), 
                       coefs = TRUE, stand = TRUE, stars = "regress")

# Exporting -------------------------------------------------------------------
export_path <- "./02__Models/03__Growth_Model/00__Experimenting/"

# Results
output <- capture.output(summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE))
writeLines(output, file.path(export_path, "results/01_Model1_Summary.txt"))

# Plot
save_png(sem_plot, file.path(export_path, "results/SEM_plot.png"))


# Old Code ---------------------------------------------------------------------
# Imputation ------------------------------------------------------------------
# Three columns are imputed using Logistic Regressions while the rest of imputed using
# Predictive Means Matching
# imputation_method <- c(rep("logreg", 3), rep("pmm", 37))
# 
# hrs_imputed <- hrs_data |>
#   dplyr::select(!c("HHID", "ID", "Gender", "Birth_year", "Age_w2", "Age_w3")) |>
#   dplyr::mutate(Education = as.factor(Education)) |>
#   dplyr::mutate(Marital_status = as.factor(Marital_status)) |>
#   dplyr::mutate(Job_status = as.factor(Job_status)) |>
#   mice(m = 5, maxit = 50, method = imputation_method, seed = 520)
# 
# 
# hrs_complete <- complete(hrs_imputed, 1)
# 
# # Merging back in the removed columns
# hrs_complete <- cbind(
#   hrs_data[, c("HHID", "ID", "Gender", "Birth_year", "Age_w2", "Age_w3")],
#   hrs_complete
# )
# 
# # Reverting factor columns back into numeric columns
# hrs_complete <- hrs_complete |>
#   dplyr::mutate(Education = as.numeric(Education)) |>
#   dplyr::mutate(Marital_status = as.numeric(Marital_status)) |>
#   dplyr::mutate(Job_status = as.numeric(Job_status)) |>
#   dplyr::mutate(across(c(Education, Marital_status, Job_status), ~ ifelse(. == 2, 1, 0)))
# 
# # Model Fitting ----------------------------------------------------------------
# fit <- growth(pp_model, data = hrs_complete, estimator = "ML", missing = "fiml")
# summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)

# Pooling idea (Code is acting up for unknown reason) ------------------------
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
# 
# fit <- semTools::growth.mi(pp_model, data = hrs_list, estimator = "ML", missing = "fiml")
# summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)ca
