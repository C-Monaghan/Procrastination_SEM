rm(list=ls()) # Clearing work space

library(lavaan)
library(lavaanPlot)

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Creating Model --------------------------------------------------------------
pp_model <- '
  # LATENT VARIABLES
  dep_w2 =~ Depression_1 + Depression_2 + Depression_3 + Depression_4 + Depression_5 + Depression_6 + Depression_7 + Depression_8;
  procras_w3 =~ Procras_1 + Procras_2 + Procras_3 + Procras_4 + Procras_5 + Procras_6 + Procras_7 + Procras_8 + Procras_9 + Procras_10 + Procras_11 + Procras_12;
  
  # RESIDUALS
  dep_w2 ~~ Health_assessment;

  # Individual question items
  Procras_1 ~~ Procras_2;
  Procras_10 ~~ Procras_11;
  Depression_1 ~~ Depression_7;
  Depression_4 ~~ Depression_6;

  # DIRECT EFFECT
  procras_w3 ~ c*Age_w2 + b1*dep_w2 + b2*Health_assessment + Gender + Education + Marital_status + Job_status;
  
  # A PATHS
  dep_w2 ~ a1*Age_w2;
  Health_assessment ~ a2*Age_w2;

  # INDIRECT EFFECT
  DIDE := a1*b1;
  HIDE := a2*b2;

  Total_IE := (a1*b1) + (a2*b2)
  
  # TOTAL EFFECT
  
  Total_Effect := c + (a1*b1) + (a2*b2)

'

# Model Fitting ----------------------------------------------------------------
fit <- sem(pp_model, data = hrs_data, estimator = "ML", missing = "fiml")
summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)


# Plotting --------------------------------------------------------------------
# Creating a list of names 
sem_names <- list(Age_w2 = "Age", dep_w2 = "Depression", procras_w3 = "Procrastination",
                   Depression_1 = "D1", Depression_2 = "D2", Depression_3 = "D3", Depression_4 = "D4",
                   Depression_5 = "D5", Depression_6 = "D6", Depression_7 = "D7", Depression_8 = "D8",
                   Procras_1 = "P1", Procras_2 = "P2", Procras_3 = "P3", Procras_4 = "P4", Procras_5 = "P5",
                   Procras_6 = "P6", Procras_7 = "P7", Procras_8 = "P8", Procras_9 = "P9", Procras_10 = "P10",
                   Procras_11 = "P11", Procras_12 = "P12",
                   Gender = "Gender", Education = "Education", Marital_status = "MS", 
                  Job_status = "JS", Health_assessment = "HS")

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

