source("shiny/helper.R")
library(ggplot2)
library(dplyr)

data <- open_pneumatron_db("https://raw.githubusercontent.com/GabrielSlPires/pneumatron_workshop_2024_03/main/pneumatron_database.csv")

ggplot(data %>% 
         filter(id %in% c(110, 116, 120, 124),
                log_line > 2),
       aes(log_line,
         pressure,
         color = datetime,
         group = paste(measure, group))) +
  geom_line() +
  geom_vline(aes(xintercept = 3)) +
  geom_vline(aes(xintercept = 30)) +
  facet_wrap(~id, scales = "free") +
  ylab("Pressure (kPa)") +
  xlab("log line") +
  ggtitle("Pressure difference inside each measurement by Pneumatron") +
  theme_bw()

data %>% 
  filter(log_line > 3) %>% 
  group_by(id, group, measure) %>% 
  summarize(`pressure diff [kPa]` = max(pressure) - min(pressure), # 3 kPa max
            speed = round(`pressure diff [kPa]`/n(), 3), # 0.02 max
            datetime = first(datetime),
            .groups = "drop") %>% 
  arrange(id, datetime) %>% 
  select(id, measure, `pressure diff [kPa]`, speed, datetime) %>% 
  as.data.frame()

data %>% # mean diff for each id
  filter(log_line > 3) %>% 
  group_by(id, group, measure) %>% 
  summarize(diff_pressure = max(pressure) - min(pressure), # 3 kPa max
            speed = round(diff_pressure/n(), 3), # 0.02 max
            datetime = first(datetime),
            .groups = "drop") %>% 
  group_by(id) %>% 
  summarise(mean = mean(diff_pressure),
            sd = sd(diff_pressure),
            datetime = first(datetime)) %>% 
  arrange(id, datetime)
