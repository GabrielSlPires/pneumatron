#!/usr/bin/env Rscript
#Receive file name from command line - for future updates
#file_name <- commandArgs(trailingOnly = TRUE)

#Database file to be cleaned
file_name <- "data/IAC/pneumatron_database-03-06.csv"
#File to save non matched measures
file_trash <- "data/IAC/trash.csv"

#regex to only allowed formats
#^(-?[0-9]+,){5}(([0-9]+\.[0-9]+,){4}v3|,([0-9]+\.[0-9]+),,,v1),[0-9]{4}(-[0-9]+){2} [0-9]+(:[0-9]+){2}

#R nedds to duplicate \ to understand this character
allowed_regex <- "^(-?[0-9]+,){5}(([0-9]+\\.[0-9]+,){4}v3|,([0-9]+\\.[0-9]+),,,v1),[0-9]{4}(-[0-9]+){2} [0-9]+(:[0-9]+){2}"

#read all lines from database file
con <- file(file_name, "r", blocking = FALSE)
data_to_correct <- readLines(con)
close(con)

#match lines with regex format
matched_lines <- stringr::str_detect(data_to_correct, allowed_regex)

#write only lines which matched regex
con <- file(file_name, "w")
writeLines(data_to_correct[matched_lines], con)
close(con)

#write only lines which mismatched regex
con <- file(file_trash, "w")
writeLines(data_to_correct[!matched_lines], con)
close(con)

message("Removed ", sum(!matched_lines), " line(s) with different format than expected")

#Clear environment
rm(file_name,
   file_trash,
   allowed_regex,
   con,
   data_to_correct,
   matched_lines)


message("\nAll lines with bugs were removed, database is ready for usage!\n")
