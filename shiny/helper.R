get_pneumatron_ad <- function(file_name) {
    open <- TRUE
    
    #Pneumatron V2
    try({
      message("try V2")
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
      open <- FALSE
      message("data opened")
    }, silent = FALSE)

    #Pneumatron V3
    try({
      if (open) {
        message("try V3")
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
        message("data opened")
      }
    }, silent = TRUE)

    #Pneumatron V2 - update
    try({
      if (open) {
        message("try V2 - update")
        data <- data.table::fread(file_name,
                                  select = 1:6,
                                  col.names = c("id",
                                                "seq",
                                                "measure",
                                                "log_line",
                                                "pressure",
                                                "datetime")) 
        open <- FALSE
        message("data opened")
      }
    }, silent = TRUE)


    #Pneumatron V3 if read with old script
    try({
      if (open) {
        message("try v3 old")
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
        message("data opened")
      }
    }, silent = TRUE)

    return(pneumatron_air_discharge(data))
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
                   start = start)) #tries the nls model
    if (any(!is.na(fit))) break #if the model was suscesfully fit break out of the loop
  }
  return(fit) #returns nls fit. If fit was not sucesfull returns NA
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
    dplyr::filter(log_line %in% c(pi_s*2, pf_s*2),
                  !is.na(id)) %>% 
    dplyr::group_by(id,
                    measure,
                    datetime_group = lubridate::floor_date(datetime,
                                                     unit = "hour")
                    ) %>% #separete measures and plants
    dplyr::filter(n() == 2) %>% 
    dplyr::summarise(pf = pressure[which(log_line == pf_s*2)], #final pressure
                     pi = pressure[which(log_line == pi_s*2)], #initial pressure
                     #using abs(), due to devices with relative and absotute pressures
                     ad_mol = (abs(pf - pi)*100*Vr)/(R*temp), 
                     ad_ul = (ad_mol*R*temp/(p_atm*100))*1000*1000*1000,
                     c = (abs(pf - pi))/(R*temp),
                     n_mol = (p_atm*1000*Vr)/(R*temp),
                    datetime = datetime[which(log_line == pi_s*2)],
                     .groups = "drop") %>% 
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
