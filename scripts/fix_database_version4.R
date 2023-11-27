#library("googledrive")
#drive_download(
#  as_id("1MJxkGqGkaaNUOa1yGP0Tt4etdVHzex1M"),
#  path = "data/IAC/data_iac/pneumatron_database-v4.csv",
#  overwrite = TRUE
#)


# primeiro troca os nomes dos arquivos
# depois aperta:
# Ctrl + Shift + S


#File received  to be cleaned
file_name <- "data/IAC/data_iac/pneumatron_database-v4.csv"

#File to save removed measures
file_trash <- "data/IAC/data_iac/trash.csv"


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
