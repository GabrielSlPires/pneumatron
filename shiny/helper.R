get_pneumatron_ad <- function(file_name) {
    data <- data.table::fread(file_name)
    colnames(data) <- c("id",
                        "ms",
                        "temp1",
                        "atm_pres1",
                        "humid1",
                        "temp2",
                        "atm_pres2",
                        "humid2",
                        "seq",
                        "measure",
                        "log_line",
                        "pressure",
                        "pressure2",
                        "co2",
                        "voc",
                        "co2_cozir",
                        "light",
                        "datetime")
    return(pneumatron_air_discharge(data))
}