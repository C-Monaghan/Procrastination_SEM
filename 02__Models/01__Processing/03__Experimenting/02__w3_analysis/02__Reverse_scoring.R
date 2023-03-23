# The below code reverse scores necessary data columns
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal(2).xlsx"))

# Calculating personality_maximum values for relevant variables
personality_max <- max(hrs_data$Careless, na.rm = TRUE)

hrs_data <- hrs_data |>
  dplyr::mutate(Organised = personality_max + 1 - Organised) |>
  dplyr::mutate(Hardworking = personality_max + 1 - Hardworking) |>
  dplyr::mutate(Self_disiplined = personality_max + 1 - Self_disiplined) |>
  dplyr::mutate(Cautious = personality_max + 1 - Cautious) |>
  dplyr::mutate(Thorough = personality_max + 1 - Thorough) |>
  dplyr::mutate(Thrifty = personality_max + 1 - Thrifty) |>
  dplyr::mutate(Moody = personality_max + 1 - Moody) |>
  dplyr::mutate(Worrying = personality_max + 1 - Worrying) |>
  dplyr::mutate(Nervous = personality_max + 1 - Nervous)

# Exporting -------------------------------------------------------------------
writexl::write_xlsx(hrs_data, path = file.path(path_data, "HRS_Data_Longitudinal(2).xlsx"))