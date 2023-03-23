rm(list=ls()) # Clearing work space

path_data <- "./01__Data/01__Raw_data/"

# Reading in Data -------------------------------------------------------------
# Tracker file containing information on each participant
tracker <- haven::read_sav(file.path(path_data, "Tracker.sav"))

# HRS data (2016)
lbq_w2 <- haven::read_sav(file.path(path_data, "HRS_2016/LB_Question_2016.sav"))

# HRS data (2020)
mod_v <- haven::read_sav(file.path(path_data, "HRS_2020/Module_V_2020.sav")) # Experimental module with the procrastination data

# Filtration ------------------------------------------------------------------
# Out of the 15723 participants in the data set only 1368 answered the procrastination scale
mod_v <- mod_v|>
  dplyr::filter(!is.na(RV155))

# Each data file is now filtered to only focus on those participants per wave
# This is done by matching the HHID and PN numbers
tracker <- dplyr::semi_join(tracker, mod_v, by = c("HHID", "PN"))

lbq_w2 <- dplyr::semi_join(lbq_w2, mod_v, by = c("HHID", "PN")) # 2016

# Since the data frames do not match in dimensions we need to filter down to the 
# exact participants per wave with the smallest dimension
tracker <- dplyr::semi_join(tracker, lbq_w2, by = c("HHID", "PN"))
mod_v <- dplyr::semi_join(mod_v, lbq_w2, by = c("HHID", "PN"))

# Creating singular data set of relevant data ----------------------------------
hrs_data <- cbind(
  tracker[, c("HHID", "PN", "GENDER", "BIRTHYR", "DEGREE", "RMARST", "PAGE", "RAGE")],
  lbq_w2[, c("PLB031C", "PLB031E", "PLB031I", "PLB031N", "PLB031R", "PLB031V", "PLB031X", "PLB031Z_1", "PLB031Z_5", "PLB031Z_6",
             "PLB031D", "PLB031H", "PLB031L", "PLB031Q")],
  mod_v[, c(paste0("RV", 156:167))]
  )

# Renaming the variables
hrs_data <- hrs_data |>
  dplyr::rename(
    HHID = "HHID",
    ID = "PN",
    Gender = "GENDER",
    Birth_year = "BIRTHYR",
    Education = "DEGREE",
    Marital_status = "RMARST",
    Age_w2 = "PAGE",
    Age_w3 = "RAGE",
    Reckless_w2 = "PLB031C", # Conscientiousness (Wave 2)
    Organised_w2 = "PLB031E",
    Responsible_w2 = "PLB031I",
    Hardworking_w2 = "PLB031N",
    Self_disiplined_w2 = "PLB031R",
    Careless_w2 = "PLB031V",
    Impulsive_w2 = "PLB031X",
    Cautious_w2 = "PLB031Z_1",
    Thorough_w2 = "PLB031Z_5",
    Thrifty_w2 = "PLB031Z_6", 
    Moody_w2 = "PLB031D", # Neuroticism (Wave 2)
    Worrying_w2 = "PLB031H",
    Nervous_w2 = "PLB031L",
    Calm_w2 = "PLB031Q",
    Procras_1 = "RV156", # Procrastination (Wave 3)
    Procras_2 = "RV157",
    Procras_3 = "RV158",
    Procras_4 = "RV159",
    Procras_5 = "RV160",
    Procras_6 = "RV161",
    Procras_7 = "RV162",
    Procras_8 = "RV163",
    Procras_9 = "RV164",
    Procras_10 = "RV165",
    Procras_11 = "RV166",
    Procras_12 = "RV167"
  )

# Exporting -------------------------------------------------------------------
export_path <- "./01__Data/03__Experimenting/"

writexl::write_xlsx(hrs_data, path = file.path(export_path, "HRS_Data_Longitudinal.xlsx"))