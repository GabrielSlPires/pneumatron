psi_extraplolation <- function(pneumatron, #pneumatron data file
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
