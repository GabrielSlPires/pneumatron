#!/usr/bin/env Rscript
# Receive file name from command line
#file_name <- commandArgs(trailingOnly = TRUE)

file_name <- "data/IAC/data_iac/pneumatron_database-v4.csv"


if (length(file_name) == 0) stop("File name must be supplied (input file)", call. = FALSE)

#I need to do it in a smarter/faster way.
#This procedure works, but is time consuming as database starts to increase
need_correction <- TRUE
while (need_correction) {
  need_correction <- tryCatch({
    #Try to open database, if finds an line with bug it returns a warning
    data <- data.table::fread(file_name, showProgress = FALSE)
    need_correction <- FALSE #This line runs with no warning is triggered
  }, warning = function(w) {
    #Get the line number with error
    line_to_remove <- stringr::str_extract(as.character(w),
                                           "(?<=Stopped early on line )[0-9]*")
    #When using fread() inside tryCatch, connection to file is left open
    #reopen file with fread() properly closes file connection
    try(data <- data.table::fread(file_name, nrows = 100), silent = TRUE)
    
    #read all lines from file
    con <- file(file_name, "r", blocking = FALSE)
    data_to_correct <- readLines(con)
    close(con)
    
    #remove line with bug and rewrite file
    con <- file(file_name, "w")
    writeLines(data_to_correct[-as.numeric(line_to_remove)], con)
    close(con)
    
    #message line number which was removed
    message("removed line ", line_to_remove, ": ", data_to_correct[as.numeric(line_to_remove)]) #Mostrar conteudo da linha
    
    return(TRUE)
  })
}

#Clear environment
rm(data,
   data_to_correct,
   line_to_remove,
   file_name,
   need_correction)

message("\nAll lines with bugs were removed, database is ready for usage!")
