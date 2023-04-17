# The below code generated descriptive statistics
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

library(apaTables)

path_data <- "./01__Data/03__Experimenting/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Creating missing value counter (for later use) ------------------------------
count_missing <- function(row) {
  sum(is.na(row))
}

# Creating initial total columns ----------------------------------------------
hrs_data$Depression_total <- rowSums(hrs_data[, 11:18], na.rm = TRUE)
hrs_data$Loneliness_total <- rowSums(hrs_data[, 19:21], na.rm = TRUE)
hrs_data$Procrastination_total <- rowSums(hrs_data[, 22:33], na.rm = TRUE)

# Any total columns equal to 0 are assigned as missing (NA)
hrs_data <- hrs_data |>
  dplyr::mutate(across(c("Depression_total","Loneliness_total", "Procrastination_total"), 
                       ~ ifelse(. %in% 0, NA, .)))

# Correcting total column for loneliness --------------------------------------
# According to the HRS Leave Behind Questionnaire handbook, to properly scale the 
# values from the 3-Item UCLA Loneliness Scale any final score with 1 or more missing items 
# should be assigned as missing itself.

missing_count <- apply(hrs_data[, 19:21], 1, count_missing)

hrs_data$Loneliness_total[missing_count >= 1] <- NA

# Creating a descriptive statistics table
hrs_data_table <- hrs_data |>
  dplyr::select(Gender, Education, Marital_status, Age_w2, Life_satisfaction, 
                Health_assessment, Job_status, Depression_total, 
                Loneliness_total, Procrastination_total)

apa

