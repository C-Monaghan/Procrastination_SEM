# The below code reverse scores necessary data columns
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal(2).xlsx"))

# Calculating personality_maximum values for relevant variables
personality_max <- max(hrs_data$Careless, na.rm = TRUE)

# Reverse scoring and recoding certain variables
hrs_data <- hrs_data  |>
  dplyr::mutate(Gender = ifelse(Gender == 1, 0, 1),
                Education = ifelse(Education %in% c(0, 1, 2), 0, 1),
                Marital_status = ifelse(Marital_status == 1, 1, 0),
                Job_status = ifelse(Job_status == 5, 1, 0),
                Depression_1 = ifelse(Depression_1 == 5, 1, 5),
                Depression_2 = ifelse(Depression_2 == 5, 1, 5),
                Depression_3 = ifelse(Depression_3 == 5, 1, 5),
                Depression_4 = ifelse(Depression_4 == 5, 5, 1),
                Depression_5 = ifelse(Depression_5 == 5, 1, 5),
                Depression_6 = ifelse(Depression_6 == 5, 5, 1),
                Depression_7 = ifelse(Depression_7 == 5, 1, 5),
                Depression_8 = ifelse(Depression_8 == 5, 1, 5),
                Organised = personality_max + 1 - Organised,
                Hardworking = personality_max + 1 - Hardworking,
                Self_disiplined = personality_max + 1 - Self_disiplined,
                Cautious = personality_max + 1 - Cautious,
                Thorough = personality_max + 1 - Thorough,
                Thrifty = personality_max + 1 - Thrifty,
                Moody = personality_max + 1 - Moody,
                Worrying = personality_max + 1 - Worrying,
                Nervous = personality_max + 1 - Nervous)

# Exporting -------------------------------------------------------------------
writexl::write_xlsx(hrs_data, path = file.path(path_data, "HRS_Data_Longitudinal(2).xlsx"))