library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)
source("../scripts/lib/pneumatron_air_discharge.R", local = TRUE)
source("../scripts/lib/psi_extrapolation.R", local = TRUE)
source("helper.R", local = TRUE)

file_name <- "2022_08_01"

server <- function(input, output) {
  #import data when press the button
  data_ad <- reactive({
    input$btn_refreash_data
    get_pneumatron_ad(paste0("../data/raw_pneumatron/", file_name, ".csv"))
  })

  print("teste")
  output$plot <- renderPlot({
    ggplot(data_ad(),
           aes(datetime, pad)) +
      geom_point() +
      scale_x_datetime(date_labels = "%b %d") +
      theme_bw()
    })
 }