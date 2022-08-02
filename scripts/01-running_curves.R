source("scripts/required_libs.R", local = TRUE)

#file_name <- "2022_08_01"

data <- data.table::fread(paste0("data/raw_pneumatron/", file_name, ".csv"))
#arg device, new or old
#pneumatron add headers automatically
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

data$datetime <- lubridate::dmy_hm(data$datetime)

p <- ggplot(pneumatron_air_discharge(data),
            aes(datetime, pad)) +
  geom_point() +
  scale_x_datetime(date_labels = "%b %d") +
  facet_wrap(~id,
             scales = "free",
             labeller = "label_both") +
  theme_bw()

ggsave(paste0("fig/",file_name, "_running_curve.png"), p)

rm(data)
