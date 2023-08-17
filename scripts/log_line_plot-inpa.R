source("shiny/helper.R")
library(ggplot2)
library(dplyr)

data_inpa <- open_pneumatron_db("data/inpa/24_04/2023-04-24.csv")

p <- data_inpa %>% 
  filter(measure != 1, #normalmente a primeira medida fica um pouco diferente neste grafico, por isso nao ploto ela
         log_line > 2) %>%  #Como o primeiro log_line da pressao e praticamente a pressao atm tambem filtro para nao mostrar esse valor
  ggplot(aes(log_line,
             pressure,
             color = datetime,
             group = paste(measure, lubridate::day(datetime)))) +
  geom_rect(aes(xmin = 3, xmax = 30, ymax = Inf, ymin = -Inf), color = "grey") +
  geom_line(alpha = 0.3) +
  geom_hline(aes(yintercept = 40)) +
  geom_vline(aes(xintercept = 3)) +
  geom_vline(aes(xintercept = 30)) +
  facet_wrap(~id, scales = "free") +
  ylab("Pressure (kPa)") +
  xlab("log line") +
  theme_bw() +
  scale_color_datetime(low = "deepskyblue", high = "tomato") +
  ggtitle("INPA - file: 24_04")
print(p)
