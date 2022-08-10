library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)
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
      boxname <- paste("Pneumatron ID:", i)
      date_range_name <- paste0("date_range_running_p", i)
      plotname <- paste0("pneumatron_plot_p", i)
      date_min = min(data_ad()[data_ad()$id == i,]$datetime)
      date_max = max(data_ad()[data_ad()$id == i,]$datetime)
      column(width = 4,
             box(title = boxname,
                 collapsible = TRUE,
                 status = "primary",
                 width = 12,
                 dateRangeInput(date_range_name,
                                label = 'Date range:',
                                start = date_min,
                                end = date_max,
                                min = date_min,
                                max = date_max),
                 plotlyOutput(plotname)))
    })
    do.call(tagList, plot_output_list)
  })

  #render each plot (running experiments)
  for (i in 1:100) {
    local({
      my_i <- i
      plotname <- paste0("pneumatron_plot_p", my_i)
      output[[plotname]] <- renderPlotly({
        datetime_filter <- input[[paste0("date_range_running_p", my_i)]]
        p <- ggplot(data_ad() %>%
                 mutate(date = lubridate::date(datetime)) %>%
                 filter(
                  id == my_i,
                  date >= datetime_filter[1],
                  date <= datetime_filter[2]
                 ),
               aes(datetime, pad)) +
          geom_point() +
          scale_x_datetime(date_labels = "%b %d") +
          theme_bw()
          ggplotly(p)
      })
    })
  }

  output$filter_experiment_boxes <- renderUI({
    date_min = min(data_ad()$datetime)
    date_max = max(data_ad()$datetime)
    box(
      width = 12,
      column(
        width = 12,
        align = "center",
        sliderInput("filter_experiment_datetime",
                    label = "Time range",
                    min = date_min,
                    max = date_max,
                    value = c(date_min,
                              date_max),
                    timeFormat = "%F %n %H:%M",
                    step = 1,
                    width = "90%")
      )
    )
  })

 }