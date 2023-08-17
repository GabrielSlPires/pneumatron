#!/usr/bin/env Rscript
#Receive file name from command line - for future updates
#file_name <- commandArgs(trailingOnly = TRUE)

#Clean database file por version 3

#File received from git to be cleaned
file_name <- "data/UNIR/Cunia_L3_1500_Pneumatron_database.csv"
#File to save removed measures
file_trash <- "data/UNIR/trash.csv"

message("\nStarting to update database with new data from git...\n")
message("New data from git: ", file_name)
message("File to store removed data: ", file_trash)

#regex to only allowed formats
#^(-?[0-9]+,){2}([0-9]+\.[0-9]+,){6}([0-9]+,){3}[0-9]\.[0-9]+,[0-9]{4}(-[0-9]+){2} [0-9]+(:[0-9]+){2}

#R nedds to duplicate \ to understand this character
allowed_regex <- "^(-?[0-9]+,){2}([0-9]+\\.[0-9]+,){6}([0-9]+,){3}[0-9]\\.[0-9]+,[0-9]{4}(-[0-9]+){2} [0-9]+(:[0-9]+){2}"

#read all lines from git file
con <- file(file_name, "r", blocking = FALSE)
new_data_from_git <- readLines(con)
close(con)

#match lines with regex format
matched_lines <- stringr::str_detect(new_data_from_git, allowed_regex)
new_data_from_git_fixed <- new_data_from_git[matched_lines]

#write only lines which matched regex
con <- file(file_name, "w")
writeLines(new_data_from_git_fixed, con)
close(con)

#write only lines which mismatched regex
con <- file(file_trash, "w")
writeLines(new_data_from_git[!matched_lines], con)
close(con)

#report what as made

message("\nRemoved ", sum(!matched_lines), " line(s) with different format than expected")