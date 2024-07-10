# This scripts filter a pneumatron database file, removing lines that do not follow a regular expression patern
# Steps for database fix:
# 1 - change file_name and file_trash
# 2 - press Ctrl + Shift + S
# 3 - check file_trash if any important data was deleted

#File received  to be cleaned
file_name <- "data/raw_pneumatron/pneumatron_database.csv"

#File to save removed measures
file_trash <- "data/raw_pneumatron/trash.csv"

message("\nStarting to update database with new data from googledrive...\n")
message("New data from git: ", file_name)
message("File to store removed data: ", file_trash)

#regex to only allowed formats
#^(-?[0-9]+,){2}([0-9]+\.[0-9]+,){6}([0-9]+,){3}[0-9]\.[0-9]+,[0-9]{4}(-[0-9]+){2} [0-9]+(:[0-9]+){2}

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

#report what as made

message("\nRemoved ", sum(!matched_lines), " line(s) with different format than expected")
rm(allowed_regex, con, file_name, file_trash, matched_lines, new_data_from_git, new_data_from_git_fixed)
