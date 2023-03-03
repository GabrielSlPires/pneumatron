get_pneumatron_ad <- function(file_name) {
    data <- open_pneumatron_db(file_name)
    data <- pneumatron_air_discharge(data)

    return(data)
}

open_pneumatron_db <- function(file_name) {
    open <- TRUE
    
    #Pneumatron V2
    try({
      data <- data.table::fread(file_name,
                                select = 1:18,
                                col.names = c("id",
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
                                              "datetime"))
      #relative pressure to absolute pressure
      data$pressure <- 101.325 - data$pressure
      open <- FALSE
      message("data opened - v2")
    }, silent = TRUE)
    
    #Pneumatron V3
    try({
      if (open) {
        data <- data.table::fread(file_name,
                                  select = 1:13,
                                  col.names = c("id",
                                                "ms",
                                                "temp1",
                                                "pressure",
                                                "humid1",
                                                "temp2",
                                                "pressure2",
                                                "humid2",
                                                "seq",
                                                "measure",
                                                "log_line",
                                                "volt",
                                                "datetime")) 
        data$pressure = data$pressure/10
        open <- FALSE
        message("data opened - v3")
      }
    }, silent = TRUE)

    #Pneumatron V4
    try({
      if (open) {
        data <- data.table::fread(file_name,
                                  select = 1:11,
                                  col.names = c("id",
                                                "group",
                                                "seq",
                                                "measure",
                                                "log_line",
                                                "humid",
                                                "pressure",
                                                "temp1",
                                                "volt",
                                                "version",
                                                "datetime")) 
        open <- FALSE
        message("data opened - v4")
      }
    }, silent = TRUE)
    
    #Pneumatron V2 - update
    try({
      if (open) {
        data <- data.table::fread(file_name,
                                  select = 1:6,
                                  col.names = c("id",
                                                "seq",
                                                "measure",
                                                "log_line",
                                                "pressure",
                                                "datetime"))
        #relative pressure to absolute pressure
        data$pressure <- 101.325 - data$pressure
        open <- FALSE
        message("data opened - v2 - update")
      }
    }, silent = TRUE)


    #Pneumatron V3 if read with old script
    try({
      if (open) {
        bkp <- data
        data <- tryCatch({
          data <- data.table::fread(file_name,
                                  header = FALSE)
          data <- data.frame(
            reshape2::colsplit(string = data$V1,
                               pattern = ",",
                               names = c("id",
                                         "ms",
                                         "temp1",
                                         "pressure",
                                         "humid1",
                                         "temp2",
                                         "atm_pres2",
                                         "humid2",
                                         "seq",
                                         "measure",
                                         "log_line",
                                         "voltage"
                               )
            ),
            datetime = lubridate::ymd_hm(data$V2))
          data$voltage = as.numeric(gsub("\n", "", data$voltage))
          data <- dplyr::filter(data, !is.na(datetime))
          open <- FALSE
          message("data opened - v3 old")
        }, warning = function(w) {
          data <- bkp
          return(data)
          stop()
        })
      }
    }, silent = TRUE)
    
    if (!is.data.frame(data)) stop("Failed to open Pneumatron database")
    return(data)
}

try.nls <- function(work.table,
                    model,
                    start.values,
                    try.times = 100){
  for (times in 1:try.times) { #try the fit "try.times" times
    start <- list() #empty list to save store values
    for (n in 1:nrow(start.values)) { #get store values in the min-max interval in the start.values data frame
      start[n] <- sample(seq(start.values$min[n],
                             start.values$max[n],
                             abs(start.values$min[n] - start.values$max[n])/try.times),
                         1)
      names(start)[n] <- as.character(start.values$parameter[n]) #makes it a names list (necessary for nls function)
    }
    fit <- NA
    try(fit <- nls(model,
                   work.table,
                   start = start),
        silent = TRUE) #tries the nls model
    if (any(!is.na(fit))) break #if the model was suscesfully fit break out of the loop
  }
  return(fit) #returns nls fit. If fit was not sucesfull returns NA
}

pneumatron_p50 <- function(data) {
  fit.pad <- try.nls(work.table =  data,
                     model = pad ~ 100/(1 + exp(a*(psi - p50))),
                     start.values = data.frame(parameter = c("a","p50"),
                                               min = c(0,-10),
                                               max = c(5,0)))
  
  a = summary(fit.pad)$coefficients[1]
  p50 = summary(fit.pad)$coefficients[2]
  p88 = log(12/88,exp(1))/a + p50
  p12 = log(88/12,exp(1))/a + p50

  p50_table <- c("a" = a,
                 "p12" = p12,
                 "p50" = p50,
                 "p88" = p88)

  return(p50_table)
}

pneumatron_px_proximity <- function(data, p) {
  px = data %>% 
    slice(which.min(abs(pad - p))) %>% 
    select(psi) %>% 
    as.numeric()
  return(px)
}

initial_pressure <- function(log_line, pressure) {
  tryCatch({
    init <- max(log_line[which(log_line < 6)][which(pressure == min(pressure))])
  },
  error = function(e) init <- 3,
  warning = function(w) init <- 3)
}

pneumatron_air_discharge <- function(pneumatron_data,
                                     pi_s = 1.5, #initial pressure time
                                     pf_s = 15, #final pressure time
                                     reservoir = 2.6, #tubing volume (in mL)
                                     p_atm = 101.3, #atmospheric pressure (in kPa)
                                     R = 8.3144621,
                                     temp = 293.15) {
  library(dplyr)
  
  Vr = reservoir*10^-6
  #calculate air discharged (AD) in mols, uL, and the percentage of air discharged (PAD) and concentration per m^3 at the final pressure 
  
  data <- pneumatron_data %>%
    dplyr::filter(pressure != "NaN",
                  !is.na(id)) 

  data <- tryCatch({ #separete measures and plants
    data <- data %>% 
      dplyr::group_by(id,
                      measure,
                      group,
                      day = lubridate::day(datetime),
                      version) 
   }, error = function(e) {
     data <- data %>% 
       #dplyr::mutate(datetime = lubridate::dmy_hm(datetime)) %>% 
      dplyr::group_by(id,
                      measure,
                      datetime_group = lubridate::floor_date(datetime,
                                                       unit = "hour")
                      )
   })
   
   data <- data %>% 
   dplyr::filter(between(log_line,
                         initial_pressure(log_line, pressure) + 1,
                         initial_pressure(log_line, pressure) + pf_s*2),
                 between(n(), 60, 120)) %>% 
    dplyr::summarise(slope = lm(pressure ~ log_line)$coefficients[[2]],
                     r_squared = summary(lm(pressure ~ log_line))$r.squared,
                     p_value = summary(lm(pressure ~ log_line))$coefficients[,4][[2]],
                     pressure_diff = slope*(pf_s*2 - pi_s*2),
                     ad_mol = (pressure_diff*100*Vr)/(R*temp), 
                     ad_ul = (ad_mol*R*temp/(p_atm*100))*1000*1000*1000,
                     c = (pressure_diff)/(R*temp),
                     n_mol = (p_atm*1000*Vr)/(R*temp),
                     datetime = min(datetime),
                     .groups = "drop") %>% 
    dplyr::filter(r_squared >= 0.85, p_value <= 0.01) %>%
    dplyr::group_by(id) %>% 
    dplyr::mutate(pad = ((ad_ul - min(ad_ul))/(max(ad_ul) - min(ad_ul)))*100) %>% 
    dplyr::ungroup()
  return(data)
}

extrapolated_wp <- function(pneumatron, #pneumatron data file
                               data_psi, #pressure pump data
                               pneumatron_time_column = "datetime", #pneumatron time column name
                               data_psi_time_column = "time", #data_psi time column name
                               data_psi_pot_column = "pot" #data_psi pressure column name
                      ){
  library(dplyr)
  
  #fix column name for pneumatron table
  pneumatron <- pneumatron %>%
    rename(psi_time_estimate = all_of(pneumatron_time_column)) %>% 
    arrange(psi_time_estimate)
  
  #fix column name for data_psi table
  data_psi <- data_psi %>%
    rename(psi_time_estimate = all_of(data_psi_time_column),
           pot = all_of(data_psi_pot_column))
  
  psi_predict <- c()
  
  model <- lm(pot ~ psi_time_estimate, data = data_psi[c(1, 2),])
  a <- pneumatron %>% 
    filter(psi_time_estimate < data_psi$psi_time_estimate[1]) %>% 
    select(psi_time_estimate)
  #predict psi in function of time
  psi_predict <- c(psi_predict, predict(model, a))

  #for each line of data_psi, predict psi value with linear model
  for (i in seq(2, nrow(data_psi))) {
    #generate model of psi in function of time
    model <- lm(pot ~ psi_time_estimate, data = data_psi[c(i - 1, i),])
    a <- pneumatron %>% 
      #filter within pressure pump measured time
      filter(psi_time_estimate >= data_psi$psi_time_estimate[i - 1],
             psi_time_estimate < data_psi$psi_time_estimate[i]) %>% 
      select(psi_time_estimate)
    #predict psi in function of time
    psi_predict <- c(psi_predict, predict(model, a))
  }
  
  #same prection, but for points after the last pressure pump measurement
  a <- pneumatron %>% 
    filter(psi_time_estimate >= data_psi$psi_time_estimate[i]) %>% 
    select(psi_time_estimate)
  psi_predict <- c(psi_predict, predict(model, a))
  #add psi column 
  pneumatron$psi <- as.numeric(psi_predict)
  #return column names to original form
  pneumatron <- pneumatron %>%
    rename(!!pneumatron_time_column := psi_time_estimate)
  
  return(pneumatron)
}

validate_data_psi <-function(file) {
  df <- data.table::fread(file, nrows = 2)
  validation <- any(colnames(df) %in% c("id", "time", "pot")) #check col names

  return(validation)
}
