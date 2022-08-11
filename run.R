#Run this line only the first time to install all packages used in this script
#source("scripts/required_libs.R")

#This command show your available COM ports
serial::listPorts()

reciver_serial_port <- "COM9" #define pneumatron receiver COM port
file_name <- "2022_8_01" #define where to save your files. change for week files?

#To start reading the pneumatron
rstudioapi::jobRunScript("scripts/00-read_pneumatron.R")

#Launch Dashboard 
rstudioapi::jobRunScript("shiny/app_bootstrap.R")
