# Some warning messages in model - need to come back and investigate

rm(list=ls()) # Clearing work space

library(lavaan)
library(lavaanPlot)

path_data <- "./01__Data/02__Processed_data/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Attempts to handle the missing values ---------------------------------------
# Removing all missing values (very strict)
# hrs_data_no_missing <- na.omit(hrs_data) 237 Participants

# Using the a fixed rate of missing based off personality questionnaire (quite strict) - see note 1
# hrs_data_reduced_1 <- hrs_data[rowSums(is.na(hrs_data)) <= 14, ] 307 Participants

# Using the cross wave rate of missingness in the life satisfaction (less strict) - see note 2
hrs_data_reduced_2 <- hrs_data[rowSums(is.na(hrs_data[11:15])) < 5 | rowSums(is.na(hrs_data[30:34])) < 5, ] # 406 Participants

# Creating Model --------------------------------------------------------------
# Note: LS = Life Satisfaction // Con = Conscientiousness // Neu = Neuroticism // Procra = Procrastination
pp_model <- '
  # Latent Variables:
  
  Con_w2 =~ v6*Reckless_w2 + v7*Organised_w2 + v8*Responsible_w2 + v9*Hardworking_w2 + v10*Self_disiplined_w2 + v11*Careless_w2 + v12*Impulsive_w2 + v13*Cautious_w2 + v14*Thorough_w2 + v15*Thrifty_w2;
  
  Neu_w2 =~ v16*Moody_w2 + v17*Worrying_w2 + v18*Nervous_w2 + v19*Calm_w2;
  
  Procra =~ Procras_1 + Procras_2 + Procras_3 + Procras_4 + Procras_5 + Procras_6 + Procras_7 + Procras_8 + Procras_9 + Procras_10 + Procras_11 + Procras_12;

  # Direct Effect
  Procra ~ c*Age_w2 + b1*Neu_w2 + b2*Con_w2;
  
  # A Paths
  Neu_w2 ~ a1*Age_w2;
  Con_w2 ~ a2*Age_w2;
  
  # Indirect effects
  NIDE := a1*b1;
  CIDE := a2*b2;
  
  Total_Indirect_Effect := (a1*b1) + (a2*b2) 
  
  # Total Effect
  Total_Effect := c + (a1*b1)+ (a2*b2) 
  
  '

# Fitting Model
pp_model_fit <- growth(pp_model, data = hrs_data_reduced_2, estimator = 'ML', missing = "fiml")
model_summary <- summary(pp_model_fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)

# Making a SEM Plot (Check if better way to do this)
sem_plot <- lavaanPlot(model = pp_model_fit_3,
                       node_options = list(shape = "box", fontname = "Helvetica"),
                       edge_options = list(color = "grey"),
                       coefs = TRUE)
# 
# # Exporting -------------------------------------------------------------------
export_path <- "./02__Models/03__Growth_Model/"

save_png(sem_plot, file.path(export_path, "Results/SEM_plot.png"))

# Notes -----------------------------------------------------------------------

# Note 1
# Due to the leave behind questionnaire being answered only by certain participants
# in certain waves we need a way to filter out the high prevalence of missing values
# whilst still retaining data with smaller prevelances of missing values. To do this we 
# will use the number of items in the personality scales (14). Participants with 14 or more 
# missing values tended to be the participants who were not given the leave behind questionnaire in both waves

# Note 2
# If participants had 5 missing values in both life satisfaction wave 1 & 2 they were removed from analysis
# This left participants who:
# 1. Answered the questionnaire in both years
# 2. Answered the questionnaire in 2012 but not 2016
# 3. Answered the questionnaire in 2016 but not 2012

# The overall hope was to use MICE to impute this missingness

# TESTING ----------------------------------------------------------------------

# cov_mat <- lavInspect(pp_model_fit_3, "vcov")
# 
# e_values <- eigen(cov_mat)$values
# e_values <- Re(e_values)
# e_values_neg <- e_values[e_values < 0]
# 
# print(e_values)
# print(e_values_neg)