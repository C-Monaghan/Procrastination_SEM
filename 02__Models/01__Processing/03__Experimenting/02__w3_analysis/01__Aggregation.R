rm(list=ls()) # Clearing work space

path_data <- "./01__Data/01__Raw_data/"

# Reading in Data -------------------------------------------------------------
# Tracker file containing information on each participant
tracker <- haven::read_sav(file.path(path_data, "Tracker.sav"))

# HRS data (2020)
lbq_w3 <- haven::read_sav(file.path(path_data, "HRS_2020/LB_Question_2020.sav"))
mod_v <- haven::read_sav(file.path(path_data, "HRS_2020/Module_V_2020.sav")) # Experimental module with the procrastination data

# Filtration ------------------------------------------------------------------
# Out of the 15723 participants in the data set only 1368 answered the procrastination scale
mod_v <- mod_v|>
  dplyr::filter(!is.na(RV155))

# Each data file is now filtered to only focus on those participants per wave
# This is done by matching the HHID and PN numbers
tracker <- dplyr::semi_join(tracker, mod_v, by = c("HHID", "PN"))
lbq_w3 <- dplyr::semi_join(lbq_w3, mod_v, by = c("HHID", "PN"))

# Creating singular data set of relevant data ----------------------------------
hrs_data <- cbind(
  tracker[, c("HHID", "PN", "GENDER", "BIRTHYR", "PAGE", "RAGE")],
  lbq_w3[, c("RLB031C", "RLB031E", "RLB031I", "RLB031N", "RLB031R", "RLB031V", "RLB031X", "RLB031Z1", "RLB031Z5", "RLB031Z6",
             "RLB031D", "RLB031H", "RLB031L", "RLB031Q")],
  mod_v[, c(paste0("RV", 156:167))]
)

# Renaming the variables
hrs_data <- hrs_data |>
  dplyr::rename(
    HHID = "HHID",
    ID = "PN",
    Gender = "GENDER",
    Birth_year = "BIRTHYR",
    Age_w2 = "PAGE",
    Age_w3 = "RAGE",
    Reckless = "RLB031C", # Start of Conscientiousness
    Organised = "RLB031E",
    Responsible = "RLB031I",
    Hardworking = "RLB031N",
    Self_disiplined = "RLB031R",
    Careless = "RLB031V",
    Impulsive = "RLB031X",
    Cautious = "RLB031Z1",
    Thorough = "RLB031Z5",
    Thrifty = "RLB031Z6", # End of Conscientiousness
    Moody = "RLB031D", # Start of Neuroticism
    Worrying = "RLB031H",
    Nervous = "RLB031L",
    Calm = "RLB031Q", # End of Neuroticism
    Procras_1 = "RV156",
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

writexl::write_xlsx(hrs_data, path = file.path(export_path, "HRS_Data_Longitudinal(2).xlsx"))
