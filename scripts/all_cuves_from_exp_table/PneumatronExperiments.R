# Define a class 'PneumatronExperiments' that inherits from 'data.frame'
setOldClass("data.frame")
setClass(Class = "PneumatronExperiments", contains = "data.frame")

# create and open experiments table in future

#' Check Column Classes
#'
#' Check if the columns in the data frame have the expected classes.
#'
#' @param df Data frame whose columns are to be checked.
#' @param expected_classes A list of expected classes for each column.
#' @return TRUE if all columns match the expected classes, otherwise stops with an error.
check_experiment_table_classes <- function(df) {
  # Expected column types
  expected_classes <- list(
    id = c("numeric", "integer"),
    pneumatron_file = "character",
    water_potential_file = "character",
    pneumatron_id = c("numeric", "integer"),
    begin = c("POSIXct", "POSIXt"),
    end = c("POSIXct", "POSIXt"),
    keep_ip = c("numeric", "integer"),
    min_datetime = c("POSIXct", "POSIXt"),
    max_datetime = c("POSIXct", "POSIXt"),
    min_gas_discharge = c("numeric", "integer"),
    max_gas_discharge = c("numeric", "integer"),
    max_wp = c("numeric", "integer"),
    obs = "character"
  )
  
  for (col in names(expected_classes)) {
    if (col %in% colnames(df)) {
      actual_class <- class(df[[col]])
      if (length(setdiff(actual_class, expected_classes[[col]])) > 0) {
        message(
          paste(
            "Column", col, "is of class", paste(actual_class, collapse = ", "),
            "but expected one of", paste(expected_classes[[col]], collapse = ", ")
          )
        )
      }
    }
  }
  return(TRUE)
}

#' Convert Data Frame to PneumatronExperiments
#'
#' Internal function to convert a data frame to PneumatronExperiments, adding ID if necessary.
#'
#' @param df Data frame to be converted.
#' @param start_id Initial ID for the experiments.
#' @return An object of class PneumatronExperiments.
convert_to_pneumatron_experiment <- function(df, start_id) {
  df <- as.data.frame(df)
  required_cols <- c("pneumatron_file", "water_potential_file", 
                     "pneumatron_id", "begin", "end")
  optional_datetime_cols <- c("min_datetime", "max_datetime")
  optional_numeric_cols <- c("keep_ip", "min_gas_discharge", "max_gas_discharge", "max_wp")
  optional_text_cols <- c("obs")
  
  if (length(setdiff(required_cols, colnames(df))) > 0) {
    stop(
      paste(
        "Data frame does not have the required columns:",
        paste(required_cols, collapse = ", ")
      )
    )
  }
  
  # If empty, needs to be max and min values. Check this if column exists
  for (col in optional_datetime_cols) {
    if (!col %in% colnames(df)) {
      df[[col]] <- as.POSIXct(NA)
    }
  }

  for (col in optional_numeric_cols) {
    if (!col %in% colnames(df)) {
      df[[col]] <- as.numeric(NA)
    }
  }
  
  for (col in optional_text_cols) {
    if (!col %in% colnames(df)) {
      df[[col]] <- as.character(NA)
    }
  }
  
  df$id <- seq_len(nrow(df)) + start_id
  
  all_cols <- c("id", required_cols, optional_datetime_cols,
                optional_numeric_cols, optional_text_cols)
  df <- df[, all_cols]
  
  if (!check_experiment_table_classes(df)) stop("Check classes")
  
  new("PneumatronExperiments", df)
}

#' Create Experiments Table
#'
#' Create a table of Pneumatron experiments.
#'
#' @param pneumatron_file Path to the Pneumatron file.
#' @param water_potential_file Path to the water potential file.
#' @param pneumatron_id Device ID.
#' @param begin Start datetime of the experiment.
#' @param end End datetime of the experiment.
#' @param keep_ip (Optional) Initial Pressure to be kept.
#' @param min_datetime (Optional) Minimum datetime.
#' @param max_datetime (Optional) Maximum datetime.
#' @param min_gas_discharge (Optional) Minimum gas discharge.
#' @param max_gas_discharge (Optional) Maximum gas discharge.
#' @param max_wp (Optional) Minimum water potential.
#' @return An object of class PneumatronExperiments.
#' @export
create_experiments_table <- function(
    pneumatron_file, 
    water_potential_file, 
    pneumatron_id, 
    begin, 
    end, 
    keep_ip = NA,
    min_datetime = NA, 
    max_datetime = NA, 
    min_gas_discharge = NA, 
    max_gas_discharge = NA,
    max_wp = NA,
    obs = NA
) {
  df <- data.frame(
    pneumatron_file = pneumatron_file,
    water_potential_file = water_potential_file,
    pneumatron_id = pneumatron_id,
    begin = begin,
    end = end,
    keep_ip = keep_ip,
    min_datetime = min_datetime,
    max_datetime = max_datetime,
    min_gas_discharge = min_gas_discharge,
    max_gas_discharge = max_gas_discharge,
    max_wp = max_wp,
    obs = obs
  )
  convert_to_pneumatron_experiment(df, start_id = 0)
}

#' Convert Data Frame to PneumatronExperiments
#'
#' User-facing function to convert a data frame into PneumatronExperiments.
#'
#' @param df Data frame to be converted.
#' @return An object of class PneumatronExperiments.
#' @export
as_pneumatron_experiment <- function(df) {
  convert_to_pneumatron_experiment(df, start_id = 0)
}

#' Add New Experiments
#'
#' Add new experiments to the existing table.
#'
#' @param experiments_table Existing table of experiments.
#' @param pneumatron_file Path to the Pneumatron file.
#' @param water_potential_file Path to the water potential file.
#' @param pneumatron_id Device ID.
#' @param begin Start datetime of the experiment.
#' @param end End datetime of the experiment.
#' @param keep_ip (Optional) Initial Pressure to be kept.
#' @param min_datetime (Optional) Minimum datetime.
#' @param max_datetime (Optional) Maximum datetime.
#' @param min_gas_discharge (Optional) Minimum gas discharge.
#' @param max_gas_discharge (Optional) Maximum gas discharge.
#' @param max_wp (Optional) Minimum water potential.
#' @param obs (Optional) Additional experiment observation.
#' @return The updated table of experiments.
#' @export
add_new_experiments <- function(
    experiments_table,
    pneumatron_file,
    water_potential_file,
    pneumatron_id,
    begin,
    end,
    keep_ip = NA,
    min_datetime = NA,
    max_datetime = NA,
    min_gas_discharge = NA,
    max_gas_discharge = NA,
    max_wp = NA,
    obs = NA
) {
  new_rows <- data.frame(
    pneumatron_file = pneumatron_file,
    water_potential_file = water_potential_file,
    pneumatron_id = pneumatron_id,
    begin = begin,
    end = end,
    keep_ip = Keep_ip,
    min_datetime = min_datetime,
    max_datetime = max_datetime,
    min_gas_discharge = min_gas_discharge,
    max_gas_discharge = max_gas_discharge,
    max_wp = max_wp,
    obs = obs
  )
  new_rows <- convert_to_pneumatron_experiment(
    new_rows, 
    start_id = max(experiments_table$id)
  )
  
  experiments_table <- rbind(experiments_table, new_rows)
  return(experiments_table)
}

#' Add Experiments from Data Frame
#'
#' Add experiments from a data frame to the existing table.
#'
#' @param experiments_table Existing table of experiments.
#' @param new_experiments_df Data frame with new experiments.
#' @return The updated table of experiments.
#' @export
add_experiments_from_df <- function(experiments_table, new_experiments_df) {
  new_experiments_df <- convert_to_pneumatron_experiment(
    new_experiments_df, 
    start_id = max(experiments_table$id)
  )
  
  experiments_table <- rbind(experiments_table, new_experiments_df)
  return(experiments_table)
}

#' Update an Experiment
#'
#' Update an existing experiment in the table.
#'
#' @param experiments_table Existing table of experiments.
#' @param experiment_id ID of the experiment to be updated.
#' @param pneumatron_file (Optional) Path to the Pneumatron file.
#' @param water_potential_file (Optional) Path to the water potential file.
#' @param pneumatron_id (Optional) Device ID.
#' @param begin (Optional) Start datetime of the experiment.
#' @param end (Optional) End datetime of the experiment.
#' @param keep_ip (Optional) Initial Pressure to be kept.
#' @param min_datetime (Optional) Minimum datetime.
#' @param max_datetime (Optional) Maximum datetime.
#' @param min_gas_discharge (Optional) Minimum gas discharge.
#' @param max_gas_discharge (Optional) Maximum gas discharge.
#' @param max_wp (Optional) Minimum water potential.
#' @param obs (Optional) Additional experiment observation.
#' @return The updated table of experiments.
#' @export
update_experiment <- function(
    experiments_table,
    experiment_id,
    pneumatron_file = NA,
    water_potential_file = NA,
    pneumatron_id = NA,
    begin = NA,
    end = NA,
    keep_ip = NA,
    min_datetime = NA,
    max_datetime = NA,
    min_gas_discharge = NA,
    max_gas_discharge = NA,
    max_wp = NA,
    obs = NA
) {
  idx <- which(experiments_table$id == experiment_id)
  if (length(idx) == 0) {
    stop("Experiment ID not found")
  }
  
  if (!is.na(pneumatron_file)) 
    experiments_table$pneumatron_file[idx] <- pneumatron_file
  if (!is.na(water_potential_file)) 
    experiments_table$water_potential_file[idx] <- water_potential_file
  if (!is.na(pneumatron_id)) 
    experiments_table$pneumatron_id[idx] <- pneumatron_id
  if (!is.na(begin)) 
    experiments_table$begin[idx] <- begin
  if (!is.na(end)) 
    experiments_table$end[idx] <- end
  if (!is.na(keep_ip)) 
    experiments_table$keep_ip[idx] <- keep_ip
  if (!is.na(min_datetime)) 
    experiments_table$min_datetime[idx] <- min_datetime
  if (!is.na(max_datetime)) 
    experiments_table$max_datetime[idx] <- max_datetime
  if (!is.na(min_gas_discharge)) 
    experiments_table$min_gas_discharge[idx] <- min_gas_discharge
  if (!is.na(max_gas_discharge)) 
    experiments_table$max_gas_discharge[idx] <- max_gas_discharge
  if (!is.na(max_wp)) 
    experiments_table$max_wp[idx] <- max_wp
  if (!is.na(obs)) 
    experiments_table$obs[idx] <- obs
  
  if (!check_experiment_table_classes(experiments_table)) stop("Check classes")
  
  return(experiments_table)
}

fill_min_max_columns <- function(df) {
  df$min_datetime <- ifelse(is.na(df$min_datetime), df$begin, df$min_datetime)
  df$max_datetime <- ifelse(is.na(df$max_datetime), df$end, df$max_datetime)
  df$min_gas_discharge <- ifelse(is.na(df$min_gas_discharge), -Inf, df$min_gas_discharge)
  df$max_gas_discharge <- ifelse(is.na(df$max_gas_discharge), Inf, df$max_gas_discharge)
  df$max_wp <- ifelse(is.na(df$max_wp), 0, df$max_wp)
  
  df$min_datetime <- as.POSIXct(df$min_datetime)
  df$max_datetime <- as.POSIXct(df$max_datetime)
  
  return(df)
}

#' Extract a Single Experiment
#'
#' Extract a single experiment from experiments table.
#'
#' @param experiments_table Existing table of experiments.
#' @param experiment_id ID of the experiment to be extracted.
#' @return The single experiments.
#' @export
extract_experiment <- function(
    experiments_table,
    experiment_id
) {
  idx <- which(experiments_table$id == experiment_id)
  if (length(idx) == 0) {
    stop("Experiment ID not found")
  }
  
  data <- experiments_table[idx,]
  data <- fill_min_max_columns(data)
  
  return(data)
}