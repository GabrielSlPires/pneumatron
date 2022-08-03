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

  #create plots (running experiments) for each diferent device in ui
  output$pneumatron_plots <- renderUI({

    plot_output_list <- lapply(unique(data_ad()$id), function(i) {
      boxname <- paste0("Pneumatron ID:", i)
      plotname <- paste0("pneumatron_plot_p", i)
      column(width = 4,
             box(title = boxname,
                 width = 12,
                 plotOutput(plotname)))
    })
    do.call(tagList, plot_output_list)
  })

  #render each plot (running experiments)
  for (i in 1:100) {
    local({
      my_i <- i
      plotname <- paste0("pneumatron_plot_p", my_i)
      output[[plotname]] <- renderPlot({
        ggplot(filter(data_ad(), id == my_i),
               aes(datetime, pad)) +
          geom_point() +
          labs(title = paste("Pneumatron id: ", my_i)) +
          scale_x_datetime(date_labels = "%b %d") +
          theme_bw()
      })
    })
  }

 }