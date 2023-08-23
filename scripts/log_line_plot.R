# With this script it is possible to diagnostic pneumatron curves.
# It plots the pressure difference inside a measurent.
# With that, you are able to identify if the vaccum pump is activated durring the 15s region.

source("shiny/helper.R")
library(ggplot2)
library(dplyr)
library(lubridate)


data <- open_pneumatron_db("data/IAC/data_iac/pneumatron_database-v4.csv")

# It's possible to add a datetime filter
#data <- filter(data, datetime > dmy_hm("01.06.2023 11:00"))

p <- data %>% 
  # This filter improve visibility
  filter(log_line > 2,
         measure > 1) %>% 
  ggplot() +
  geom_line(aes(log_line,
                pressure,
                color = datetime,
                group = paste(measure))) +
  geom_vline(aes(xintercept = 3)) +
  geom_vline(aes(xintercept = 30)) +
  geom_rect(aes(xmin = 3, xmax = 30, ymax = Inf, ymin = -Inf), color = "grey70") +
  facet_wrap(~id, scales = "free") +
  ylab("Pressure (kPa)") +
  xlab("log line") +
  ggtitle("Pressure difference inside each measurement by Pneumatron") +
  theme_bw() +
  scale_color_datetime(low = "blue", high = "red")
print(p)