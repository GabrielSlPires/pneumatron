data <- data.table::fread("https://raw.githubusercontent.com/GabrielSlPires/pneumatron_workshop_2024_03/main/pneumatron_database.csv")
data.table::fwrite(data, file = "data/workshop_data.csv", col.names = FALSE)
