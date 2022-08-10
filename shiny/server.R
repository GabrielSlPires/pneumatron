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

  #render data_psi in page
  output$psi_file_table <- renderTable({data_psi()})

  #read data_psi file
  data_psi <- reactive({
    #read only if file is uploaded
    req(input$psi_file_input)
    tryCatch(
      {
        df <- data.table::fread(input$psi_file_input$datapath)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(df)
  })



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
      ),
      fileInput(
        "psi_file_input",
        "Select your file with Water Pressure values:",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")
      ),
      tableOutput('psi_file_table')
    )
  })

 }