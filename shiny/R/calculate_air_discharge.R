#' Calculate air discharged from Pneumatron output
#'
#' @param data pneumatron output object, or data frame with columns named properly. For columns names see ...
#' @param pi_s initial pressure time.
#' @param pf_s final pressure time.
#' @param reservoir tubing volume (in mL)
#' @param p_atm atmospheric pressure (in kPa).
#' @param R Ideal gas law constant.
#' @param temp Temperature.
#' @returns A data frame with calculated air discharged
#' @examples
#' data <- open_pneumatron_file("file.csv")
#' calculate_air_discharge(data)
#'
calculate_air_discharge <- function(
    data,
    pi_s = 1.5,
    pf_s = 15,
    reservoir = 2.6,
    p_atm = 101.3,
    R = 8.3144621,
    temp = 293.15) {

  setDT(data)
  reservoir_volume = reservoir*10^-6
  r_temp <- R*temp

  data_prepared <- prepare_data_for_air_discharge(data, pf_s)
  data_prepared <- data_prepared[n %between% c(60, 120)]
  data_prepared[, pressure_diff := abs(pi - pf)]
  data_prepared[, ad_mol := (pressure_diff*100*reservoir_volume)/(r_temp)]
  data_prepared[, ad_ul := (ad_mol*r_temp/(p_atm*100))*1e+09]
  data_prepared[, c := pressure_diff/(r_temp)]
  data_prepared[, n_mol := (p_atm*1000*reservoir_volume)/r_temp]

  return(data_prepared)
}

#' Prepare data for air discharge calculations
#'
#' It is used for internal calculations only
#'
#' @param data pneumatron output object, or data frame with columns named properly. For columns names see ...
#' @param pf_s final pressure time.
#' @returns A data frame to calculate air discharged
#'
prepare_data_for_air_discharge <- function(data, pf_s) {
  setDT(data)

  if ("group" %in% colnames(data)) {
    data_prepared <- data[,
                          .(
                            datetime = first(datetime),
                            n = .N,
                            pi = pressure[which.min(abs(log_line - (get_initial_pressure(log_line, pressure) + 1)))],
                            pf = pressure[which.min(abs(log_line - (get_initial_pressure(log_line, pressure) + pf_s*2)))]
                          ),
                          by = list(id,
                                  measure,
                                  group,
                                  day = lubridate::day(datetime),
                                  version)]
  } else {
    data_prepared <- data[,
                          .(
                            datetime = first(datetime),
                            n = .N,
                            pi = pressure[which.min(abs(log_line - (get_initial_pressure(log_line, pressure) + 1)))],
                            pf = pressure[which.min(abs(log_line - (get_initial_pressure(log_line, pressure) + pf_s*2)))]
                          ),
                          by = list(id,
                                  measure,
                                  datetime_group = lubridate::floor_date(datetime,
                                                                         unit = "hour"))]
  }
  return(data_prepared)
}

#' Get initial pressure for air discharge calculation
#'
#' Returns lowest pressure position enabling to filter only the necessary points for air discharge calculations within a measurement.
#'
#' @param log_line integer. log line of measured pressure
#' @param pressure measured pressure from pneumatron
#' @returns Position of lowest pressure in a list
#'
get_initial_pressure <- function(log_line, pressure) {
  tryCatch({
    init <- max(log_line[which(log_line < 6)][which(pressure == min(pressure,
                                                                    na.rm = TRUE))],
                na.rm = TRUE)
  },
  error = function(e) init <- 3,
  warning = function(w) init <- 3)
}
