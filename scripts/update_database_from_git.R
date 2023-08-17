#!/usr/bin/env Rscript
#Receive file name from command line - for future updates
#file_name <- commandArgs(trailingOnly = TRUE)


#File received from git to be cleaned
file_name <- "data/IAC/data_iac/pneumatron_database-v4.csv"
#File to save removed measures
file_trash <- "data/IAC/data_iac/trash.csv"
#complete database file
database_name <- "data/IAC/pneumatron_database-03-06.csv"

message("\nStarting to update database with new data from git...\n")
message("New data from git: ", file_name)
message("File to store removed data: ", file_trash)
message("Database: ", database_name)

#regex to only allowed formats
#^(-?[0-9]+,){5}(([0-9]+\.[0-9]+,){4}v3|,([0-9]+\.[0-9]+),,,v1),[0-9]{4}(-[0-9]+){2} [0-9]+(:[0-9]+){2}

#R nedds to duplicate \ to understand this character
allowed_regex <- "^(-?[0-9]+,){5}(([0-9]+\\.[0-9]+,){4}v3|,([0-9]+\\.[0-9]+),,,v1),[0-9]{4}(-[0-9]+){2} [0-9]+(:[0-9]+){2}"

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

#read lines from database - I want just the last line, I need to check if there is a easier way to get it 
con <- file(database_name, "r", blocking = FALSE)
database <- readLines(con)
close(con)

#get database last line position in new data from git
position_in_git_table <- stringr::str_which(new_data_from_git_fixed, tail(database, 1)) + 1

#Check if database is already updated
if (position_in_git_table >= length(new_data_from_git_fixed)) stop("Database already updated")

#select new data to add in database
new_data <- new_data_from_git_fixed[position_in_git_table:length(new_data_from_git_fixed)]

#Append new data in database
con <- file(database_name, "a")
writeLines(c("", new_data), con)
close(con)

#report what as made

message("\nRemoved ", sum(!matched_lines), " line(s) with different format than expected")

message("Added ", length(new_data), " row(s) to database: ", database_name, "\n")