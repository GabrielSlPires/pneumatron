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
                  !is.na(plant)) %>% 
    dplyr::group_by(plant, measure) %>% #separete measures and plants
    dplyr::summarise(pf = pressure[which(log_line == pf_s*2)], #final pressure
                     pi = pressure[which(log_line == pi_s*2)], #initial pressure
                     #using abs(), due to devices with relative and absotute pressures
                     ad_mol = (abs(pf - pi)*100*Vr)/(R*temp), 
                     ad_ul = (ad_mol*R*temp/(p_atm*100))*1000*1000*1000,
                     c = (abs(pf - pi))/(R*temp),
                     n_mol = (p_atm*1000*Vr)/(R*temp),
                     datetime = datetime[which(log_line == pi_s*2)],
                     .groups = "drop") %>% 
    dplyr::group_by(plant) %>% 
    dplyr::mutate(pad = ((ad_ul - min(ad_ul))/(max(ad_ul) - min(ad_ul)))*100) %>% 
    dplyr::ungroup()
  
  return(data)
}
