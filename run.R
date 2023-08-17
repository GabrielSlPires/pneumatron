#Run this line only the first time to install all packages used in this script
source("scripts/required_libs.R")

#This command show your available COM ports
serial::listPorts()

reciver_serial_port <- c("COM12") #define pneumatron receiver COM port
file_name <- "pneumatron_database" #define where to save your files. 

#To start reading the pneumatron
rstudioapi::jobRunScript("scripts/00-read_pneumatron.R", importEnv = TRUE)

#Launch Dashboard 
rstudioapi::jobRunScript("shiny/app_bootstrap.R")

