source("scripts/required_libs.R", local = TRUE)

data <- data.table::fread(paste0("data/raw_pneumatron/", database_name, ".csv"))
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

data_psi <- data.table::fread(paste0("data/raw_psi/", data_psi_file, ".csv"))
data_psi$time <- lubridate::dmy_hm(data_psi$time)

data_filter <- data %>% 
  filter(datetime >= lubridate::ymd_hm(date_start),
         datetime <= lubridate::ymd_hm(date_end),
         id == pneumatron_id)
write.table(data_filter,
            file = paste("data/raw_table/",
                         database_name,
                         "p",
                         pneumatron_id,
                         ".csv",
                         sep = "_"))

data_ad <- data_filter %>% 
  pneumatron_air_discharge() %>% 
  psi_extraplolation(data_psi)

#need do create a better name index
write.table(data_ad,
            file = paste("result/",
                         database_name,
                         "p",
                         pneumatron_id,
                         ".csv",
                         sep = "_"))

ggplot(data_ad,
       aes(psi, pad)) +
  geom_point() +
  theme_bw()
ggsave(paste("fig/",
             database_name,
             "p",
             pneumatron_id,
             "pad_pot.png",
             sep = "_"))

ggplot(data_ad,
       aes(datetime, psi)) +
  geom_point() +
  theme_bw()
ggsave(paste("fig/",
             database_name,
             "p",
             pneumatron_id,
             "psi.png",
             sep = "_"))

ggplot(data_ad,
       aes(psi, ad_ul)) +
  geom_point() +
  theme_bw()
ggsave(paste("fig/",
             database_name,
             "p",
             pneumatron_id,
             "ad_ul_pot.png",
             sep = "_"))

ggplot(data_ad,
       aes(datetime, ad_ul)) +
  geom_point() +
  theme_bw()
ggsave(paste("fig/",
             database_name,
             "p",
             pneumatron_id,
             "ad_ul.png",
             sep = "_"))


