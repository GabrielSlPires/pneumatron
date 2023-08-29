source("shiny/helper.R")
library(ggplot2)
library(dplyr)
    
while (TRUE) {
  try({
    #data <- filter(open_pneumatron_db("data/raw_pneumatron/leak_test_03-15.csv"))
    
    #colocar no tryCacht pegar warning para deletar linha
    data <- open_pneumatron_db("data/IAC/data_iac/pneumatron_database-v4.csv")
    
    data %>% 
      filter(datetime >= lubridate::ymd_hm("2023-03-24 13:30"),
             pressure < 85) %>% 
      group_by(id) %>% 
      mutate(measure2 = measure - min(measure)) %>% 
      ggplot(aes(log_line,
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

    #print(p)

    print(data %>% 
            group_by(id, group) %>% 
            summarize(`pressure diff [kPa]` = max(pressure) - min(pressure),
                      speed = round(`pressure diff [kPa]`/n(), 3),
                      .groups = "drop") %>% 
            arrange(id, speed) %>% 
            as.data.frame())
    calculate_air_discharge(data)
    
  })
  Sys.sleep(5)
}
