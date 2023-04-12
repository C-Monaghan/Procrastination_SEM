# The below code reverse scores necessary data columns
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Calculating personality_maximum values for relevant variables
personality_max <- max(hrs_data$Careless_w2, na.rm = TRUE)

# Reverse scoring and recoding certain variables
hrs_data <- hrs_data  |>
  dplyr::mutate(Gender = ifelse(Gender == 1, 0, 1),
                Education = ifelse(Education %in% c(0, 1, 2), 0, 1),
                Marital_status = ifelse(Marital_status == 1, 1, 0),
                Life_satisfaction = ifelse(Life_satisfaction == c(1, 2, 3), 1, 0),
                Health_assessment = ifelse(Health_assessment ==  c(1, 2, 3), 1, 0),
                Job_status = ifelse(Job_status == 5, 1, 0),
                Depression_1 = ifelse(Depression_1 == 5, 1, 2),
                Depression_2 = ifelse(Depression_2 == 5, 1, 2),
                Depression_3 = ifelse(Depression_3 == 5, 1, 2),
                Depression_4 = ifelse(Depression_4 == 5, 2, 1),
                Depression_5 = ifelse(Depression_5 == 5, 1, 2),
                Depression_6 = ifelse(Depression_6 == 5, 2, 1),
                Depression_7 = ifelse(Depression_7 == 5, 1, 2),
                Depression_8 = ifelse(Depression_8 == 5, 1, 2),
                Organised_w2 = personality_max + 1 - Organised_w2,
                Hardworking_w2 = personality_max + 1 - Hardworking_w2,
                Self_disiplined_w2 = personality_max + 1 - Self_disiplined_w2,
                Cautious_w2 = personality_max + 1 - Cautious_w2,
                Thorough_w2 = personality_max + 1 - Thorough_w2,
                Thrifty_w2 = personality_max + 1 - Thrifty_w2,
                Moody_w2 = personality_max + 1 - Moody_w2,
                Worrying_w2 = personality_max + 1 - Worrying_w2,
                Nervous_w2 = personality_max + 1 - Nervous_w2,
                Loneliness_1 = dplyr::recode(Loneliness_1, '1' = 3, '2' = 2, '3' = 1),
                Loneliness_2 = dplyr::recode(Loneliness_2, '1' = 3, '2' = 2, '3' = 1),
                Loneliness_3 = dplyr::recode(Loneliness_3, '1' = 3, '2' = 2, '3' = 1),
  )

# Exporting -------------------------------------------------------------------
export_path <- "./01__Data/03__Experimenting/"

writexl::write_xlsx(hrs_data, path = file.path(export_path, "HRS_Data_Longitudinal.xlsx"))