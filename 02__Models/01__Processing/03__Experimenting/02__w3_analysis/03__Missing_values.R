rm(list=ls()) # Clearing work space

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal(2).xlsx"))

# Replacing preassigned missing values integers with the actual NA value ------ 
hrs_data <- hrs_data |>
  dplyr::mutate(across(c(paste0("Procras_", 1:12)), ~ ifelse(. %in% c(-8, 8, 9), NA, .)))

# Exporting -------------------------------------------------------------------
writexl::write_xlsx(hrs_data, path = file.path(path_data, "HRS_Data_Longitudinal(2).xlsx"))