#This command show your available COM ports
listPorts()

reciver_serial_port <- "COM9" #define pneumatron receiver COM port
file_name <- "2022_7_20" #define where to save your files. change for week files?

#To start reading the pneumatron
jobRunScript("scripts/00-read_pneumatron.R")

