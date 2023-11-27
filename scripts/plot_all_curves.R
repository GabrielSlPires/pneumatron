library(dplyr)
library(ggplot2)

filenames <-  list.files("all_curves/curves/", full.names = TRUE)
data <- purrr::map_dfr(filenames, data.table::fread)
parameters <- data.table::fread("all_curves/parameters/parameters.csv")

data <- data %>%
  rowwise() %>%   
  mutate(species = parameters$species[id])

ggplot(data, aes(psi, pad, color = species)) +
  geom_point()
