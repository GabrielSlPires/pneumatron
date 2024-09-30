# logical para filtar ou não jump inicial
library(data.table)
#library(dplyr)
library(ggplot2)
library(ggside)
#library(purrr)
library(lubridate)
library(ggdaynight)
library(pneumatools)

source("scripts/all_cuves_from_exp_table/utils.R")
source("scripts/all_cuves_from_exp_table/PneumatronExperiments.R")



# exp_data <- fread("data/coffea/my_experiments.csv", blank.lines.skip = TRUE)
# 
# exp_data[, min_gas_discharge := ifelse(is.na(min_gas_discharge), begin, min_gas_discharge)]
# exp_data[, max_gas_discharge := ifelse(is.na(min_gas_discharge), begin, max_gas_discharge)]

exp_data <- fread("data/experiments.csv", blank.lines.skip = TRUE) # need to add sample ID
exp_data <- as_pneumatron_experiment(exp_data)

# começa nessa
curve_id <- 1 # ate ...
curve <- extract_experiment(exp_data, curve_id)

old_file <- tryCatch(old_file, error = function(e) old_file <- "empty")
if (old_file != curve$pneumatron_file) {
  message("changed data file: ", curve$pneumatron_file)
  old_file <- curve$pneumatron_file
  data_format <- detect_data_format(curve$pneumatron_file)
  database <- open_pneumatron_database(curve$pneumatron_file, data_format)
  data_ad <- calculateGasDischarged(database)
  data <- get_data(data_ad)
}

ggplot(data[id == curve$pneumatron_id],
       aes(datetime, gd_ul)) +
  geom_daynight() +
  geom_point(size = 1) +
  geom_hline(yintercept = curve$min_gas_discharge) +
  geom_hline(yintercept = curve$max_gas_discharge) +
  geom_vline(xintercept = curve$begin) +
  geom_vline(xintercept = curve$end)

# max(data$datetime)
# exp_data <- update_experiment(exp_data, curve_id,
#                               # max_datetime = curve$max_datetime - 7.5*24*60*60
#                               min_gas_discharge = 115
#                               )
# curve <- extract_experiment(exp_data, curve_id)
# fwrite(exp_data, "data/experiments.csv")

ggplot(data[
         id == curve$pneumatron_id
         & between(datetime, curve$begin, curve$end)
         # & between(gd_ul, curve$min_gas_discharge, curve$max_gas_discharge)
       ],
       aes(
         datetime,
         gd_ul,
         color =  as.factor(initial_pressure_log_line))) +
  geom_daynight() +
  geom_point(size = 1) +
  geom_vline(xintercept = curve$min_datetime) +
  geom_vline(xintercept = curve$max_datetime) +
  # geom_hline(yintercept = 112) +
  # geom_vline(xintercept = curve$max_datetime - 7.5*24*60*60) +
  theme(legend.position = "none")

curve_data <- data[
  id == curve$pneumatron_id
  & between(datetime, curve$begin, curve$end)
  & between(gd_ul, curve$min_gas_discharge, curve$max_gas_discharge)
]

sort(table(curve_data$initial_pressure_log_line), decreasing = TRUE)
keep_ip <- as.numeric(names(sort(table(curve_data$initial_pressure_log_line), decreasing = TRUE))[1])

curve_data <- data[
  id == curve$pneumatron_id
  & initial_pressure_log_line == keep_ip
  # & initial_pressure_log_line %in% 3:4
  & between(datetime, curve$begin, curve$end)
  & between(gd_ul, curve$min_gas_discharge, curve$max_gas_discharge)
]

ggplot(curve_data, aes(datetime, gd_ul)) +
  geom_daynight() +
  # geom_hline(yintercept = 870) +
  geom_point()

water <- open_data_psi(curve$water_potential_file)
ggplot(water[id == curve$pneumatron_id],
       aes(time, pot)) +
  geom_daynight() +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = curve$inicio_exp) +
  geom_vline(xintercept = curve$final_exp)

filter_water <- water[id == curve$pneumatron_id
                      & between(time, curve$begin, curve$end)
                      & !is.na(pot)]
filter_water

# curve_data <- extrapolated_wp(curve_data, filter_water[-c(6)])
curve_data <- extrapolated_wp(curve_data, filter_water)

ggplot(curve_data,
       aes(psi, gd_ul)) +
  geom_point() +
  geom_vline(xintercept = filter_water$pot)
  
jump_psi <- -1.1
jump_psi <- curve$max_wp

ggplot(vc_derivate(curve_data)[psi > -5],
       aes(psi, gd_ul)) +
  geom_point() +
  geom_line(aes(y = smooth), color = "red") +
  geom_vline(xintercept = jump_psi, color = "red") +
  geom_xsidepoint(aes(y = derivative)) +
  geom_xsidevline(xintercept = jump_psi,
                  color = "red")

ggplot(curve_data,
       aes(psi, gd_ul, color = psi < jump_psi)) +
  geom_point()
# 
# exp_data <- update_experiment(exp_data, curve_id, max_wp = jump_psi)
# curve <- extract_experiment(exp_data, curve_id)
# fwrite(exp_data, "data/coffea/experiments.csv")

model_data <- curve_data[
  psi < curve$max_wp
  & datetime < curve$max_datetime
]

ggplot(model_data[gd_ul < 180 & psi > -9],
       aes(psi, gd_ul)) +
  geom_point()

model_data[, pgd := ((gd_ul - min(gd_ul))/(max(gd_ul) - min(gd_ul)))*100]
model_data[, time := ifelse(between(hour(datetime), 6, 18), "day", "night")]
curve_data[, time := ifelse(between(hour(datetime), 6, 18), "day", "night")]

ggplot(model_data,
       aes(psi, gd_ul, color = time)) +
  geom_blank(aes(x = 0)) +
  # geom_hline(yintercept = 400) +
  geom_vline(xintercept = filter_water$pot, linetype = "dashed") +
  geom_point(size = 1) 

# model_data <- model_data[gd_ul < 400]
# model_data <- model_data[psi > -7]
model_data[, pgd := (gd_ul - min(gd_ul))/(max(gd_ul) - min(gd_ul))*100]

model <- nls(
  pgd ~ 100/(1 + exp(a*(psi - p50))),
  data = model_data[,.(pgd, psi)],
  start = list(a = 1,
               p50 = -5)
)
model_confint <- confint(model)


ggplot(model_data,
       aes(psi, pgd)) +
  geom_point(aes(color = time)) +
  geom_blank(aes(x = 0)) +
  geom_blank(aes(x = get_pammenter_max(model))) +
  geom_blank(aes(x = get_pammenter_min(model))) +
  geom_vulnerability_curve_pammenter(model, model_confint) +
  geom_vline(xintercept = filter_water$pot, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = coef(model)["p50"], linetype = "dashed") +
  scale_color_manual(values = c("day" = "orange", "night" = "blue")) +
  theme_bw()
curve_id
model
