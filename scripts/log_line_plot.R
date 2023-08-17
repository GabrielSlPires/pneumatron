source("shiny/helper.R")
library(ggplot2)
library(dplyr)
library(lubridate)

#colocar no tryCacht pegar warning para deletar linha
data <- open_pneumatron_db("data/IAC/data_iac/pneumatron_database-v4.csv")
#data <- open_pneumatron_db("data/unir/pneumatron_10-06.csv")

curves_05_24 <- c(1:8, 58, 46, 64, 66)
date_05_24 <- dmy_hm("24.05.2023 11:00")
curves_05_31 <- c(4, 5, 54, 13, 48, 31, 55, 64, 67)
date_05_31 <- dmy_hm("31.05.2023 11:00")


data <- data %>% 
  #filter(datetime > dmy_hm("01.06.2023 11:00")) %>% 
  mutate(id_new = case_when(
    id %in% curves_05_31 & datetime > date_05_31 ~ paste(id, "(3)"),
    id %in% curves_05_24 & datetime > date_05_24 ~ paste(id, "(2)"),
    .default = paste(id, "(1)")
  ))

p <- data %>% 
  filter(#id %in% 6:8,
         log_line > 2,
         measure > 1) %>% 
  ggplot() +
  geom_line(aes(log_line,
                pressure,
                color = datetime,
                group = paste(measure))) +
  geom_hline(aes(yintercept = 40)) +
  geom_vline(aes(xintercept = 3)) +
  geom_vline(aes(xintercept = 30)) +
  facet_wrap(~id_new, scales = "free") +
  ylab("Pressure (kPa)") +
  xlab("log line") +
  ggtitle("Pressure difference inside each measurement by Pneumatron") +
  theme_bw() +
  scale_color_datetime(low = "blue", high = "red")
print(p)

data_ad <- pneumatron_air_discharge(data %>% 
                                      filter(!(id == 48 & measure >= 497))) 

data_ad2 <- data_ad %>% 
  mutate(id_new = case_when(
    id %in% curves_05_31 & datetime > date_05_31 ~ paste(id, "(3)"),
    id %in% curves_05_24 & datetime > date_05_24 ~ paste(id, "(2)"),
    .default = paste(id, "(1)")
  ))

p1 <- ggplot(data_ad2,
             aes(datetime, ad_ul)) +
  geom_point(size = 1) +
  #geom_vline(aes(xintercept = lubridate::dmy_h("25.05.2023 6"))) +
  #geom_vline(aes(xintercept = lubridate::dmy_h("24.05.2023 12"))) +
  #geom_vline(aes(xintercept = lubridate::dmy_h("24.05.2023 18"))) +
  #geom_vline(aes(xintercept = lubridate::dmy_h("26.05.2023 10"))) +
  #geom_vline(aes(xintercept = lubridate::dmy_h("31.05.2023 12"))) +
  #geom_vline(aes(xintercept = lubridate::dmy_h("31.05.2023 17"))) +
  #geom_vline(aes(xintercept = lubridate::dmy_h("01.06.2023 10"))) +
  facet_wrap(~id_new, scales = "free") +
  theme_bw() +
  #scale_color_gradientn(colours = terrain.colors(10)) +
  scale_x_datetime(date_labels = "%d/%m")
print(p1)
