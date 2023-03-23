# The below code reverse scores necessary data columns
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Calculating personality_maximum values for relevant variables
personality_max <- max(hrs_data$Careless_w2, na.rm = TRUE)

hrs_data <- hrs_data |>
  dplyr::mutate(Organised_w2 = personality_max + 1 - Organised_w2) |>
  dplyr::mutate(Hardworking_w2 = personality_max + 1 - Hardworking_w2) |>
  dplyr::mutate(Self_disiplined_w2 = personality_max + 1 - Self_disiplined_w2) |>
  dplyr::mutate(Cautious_w2 = personality_max + 1 - Cautious_w2) |>
  dplyr::mutate(Thorough_w2 = personality_max + 1 - Thorough_w2) |>
  dplyr::mutate(Thrifty_w2 = personality_max + 1 - Thrifty_w2) |>
  dplyr::mutate(Moody_w2 = personality_max + 1 - Moody_w2) |>
  dplyr::mutate(Worrying_w2 = personality_max + 1 - Worrying_w2) |>
  dplyr::mutate(Nervous_w2 = personality_max + 1 - Nervous_w2)

# Exporting -------------------------------------------------------------------
export_path <- "./01__Data/03__Experimenting/"

writexl::write_xlsx(hrs_data, path = file.path(export_path, "HRS_Data_Longitudinal.xlsx"))