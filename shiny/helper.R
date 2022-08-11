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
    dplyr::group_by(id, measure) %>% #separete measures and plants
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

