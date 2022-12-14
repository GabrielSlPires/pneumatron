require(serial, quietly = TRUE)

for (reciver in reciver_serial_port) {
  message("Reading Pneumatron from port: ",
          reciver,
          "\n")
}
message("Saving data to file: ",
        file_name,
        ".csv\n\n")
#Create file with headers in here

#Create a list of COM ports
con <- list()
for (reciver in reciver_serial_port) {
  con[[match(reciver, reciver_serial_port)]] <- serialConnection(
    port = reciver,
    mode = "115200,n,8,1",
    newline = 1,
    translation = "crlf")
}

#close all COM ports to avoid errors
for (reciver in reciver_serial_port) close(con[[match(reciver, reciver_serial_port)]])

#open all COM ports
for (reciver in reciver_serial_port) open(con[[match(reciver, reciver_serial_port)]])

while (1) {
  #read each COM port and append data to file
  for (reciver in reciver_serial_port) {
    message("\rReading Pneumatron   ", appendLF = FALSE)
    Sys.sleep(0.2)
    message("\rReading Pneumatron.  ", appendLF = FALSE)
    Sys.sleep(0.2)
    message("\rReading Pneumatron.. ", appendLF = FALSE)
    Sys.sleep(0.2)
    message("\rReading Pneumatron...", appendLF = FALSE)
    Sys.sleep(0.2)
    tryCatch({
      serial_messages <- read.serialConnection(con[[match(reciver, reciver_serial_port)]])
      if (serial_messages != "") {
        time <- Sys.time()
        serial_messages <- unlist(strsplit(serial_messages, "\n"))
        for (split_message in serial_messages) {
          line <- paste(split_message, lubridate::ymd_hms(time), sep = ",")
          write(line,
                file = paste0("../data/raw_pneumatron/", file_name, ".csv"),
                append = TRUE)
        }
      }
    }, error = function(e){})
  }
}