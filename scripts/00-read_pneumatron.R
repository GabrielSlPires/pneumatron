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
  con[[match(reciver, reciver_serial_port)]] <- serial::serialConnection(
    port = reciver,
    mode = "115200,n,8,1",
    newline = 1,
    translation = "auto")
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
      buffer <- serial::nBytesInQueue(con[[match(reciver, reciver_serial_port)]])
      while (buffer[1] > 0) {
        read_serial <- paste("gets ${sdev_", reciver_serial_port, "}", sep = "")
        serial_message <- tcltk::tclvalue(tcltk::.Tcl(read_serial))
        if (serial_message == "") break
        time <- Sys.time()
        line <- paste(serial_message, lubridate::ymd_hms(time), sep = ",")
        write(line,
              file = paste0("data/raw_pneumatron/", file_name, ".csv"),
              append = TRUE)
        buffer <- serial::nBytesInQueue(con[[match(reciver, reciver_serial_port)]])
      }
    }, error = function(e){message(e)})
  }
}