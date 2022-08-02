#This command show your available COM ports
listPorts()

reciver_serial_port <- "COM9" #define pneumatron receiver COM port
file_name <- "2022_7_20" #define where to save your files. change for week files?

#To start reading the pneumatron
jobRunScript("scripts/00-read_pneumatron.R")

#To keep tracking current curves
jobRunScript("scripts/01-running_curves.R",
             workingDir = getwd(),
             exportEnv = "R_GlobalEnv",
             importEnv = TRUE)
plot(p)

#this will join water potential measurements to your experiment,
#creating plots and tables for further analysis

#To save different experiments
database_name <- "2022_7_20" #define where the data was saved
#define start and end datetime, year-month-day hour-minute
date_start <- "2022-07-20 09:50"
date_end <- "2022-07-28 17:00"
pneumatron_id <- 12
data_psi_file <- "2022_7_20_p_12" #file with water potential measurements

jobRunScript("scripts/02-air_discharge.R",
             workingDir = getwd(),
             importEnv = TRUE)


