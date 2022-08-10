library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)
source("../scripts/lib/pneumatron_air_discharge.R", local = TRUE)
source("../scripts/lib/psi_extrapolation.R", local = TRUE)
source("helper.R", local = TRUE)

file_name <- "2022_08_01"

server <- function(input, output, session) {
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

  output$psi_plot_filter_view <- renderPlotly({
      req(input$psi_file_input)
      p <- ggplot(data_psi(),
      aes(time, pot, group = 1)) +
        geom_line() +
        theme_bw()
      ggplotly(p)
  })

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

  data_ad_experiment_filter <- reactive({
    datetime_filter <- input$filter_experiment_datetime
    data <- data_ad() %>%
      filter(datetime >= datetime_filter[1],
             datetime <= datetime_filter[2],
             id == input$pneumatron_id) %>%
      mutate(pad = ((ad_ul - min(ad_ul))/(max(ad_ul) - min(ad_ul)))*100)
    if(!is.null(input$psi_file_input)){
      psi <- data_psi()
      psi$time <- dmy_hm(psi$time)
      data <- psi_extraplolation(data, psi)
    }
    return(data)
  })

  output$pneumatron_filtered_plot <- renderPlotly({
    p <- ggplot(data_ad_experiment_filter(), aes(datetime, ad_ul)) +
      geom_point() +
      theme_bw()
    ggplotly(p)
  })

  #Analysis plots
  output$pneumatron_plot_psi_pad <- renderPlot({
    p <- ggplot(data_ad_experiment_filter(), aes(psi, pad)) +
      geom_point() +
      theme_bw() +
      xlab(expression(paste(psi, " (MPa)"))) +
      ylab("Air Discharge (%)")
    p
  })
  output$pneumatron_plot_psi_ad_ul <- renderPlot({
    p <- ggplot(data_ad_experiment_filter(), aes(psi, ad_ul)) +
      geom_point() +
      theme_bw() +
      xlab(expression(paste(psi, " (MPa)"))) +
      ylab(expression(paste("Air Discharge (", mu, "l)")))
    p
  })
  output$pneumatron_plot_time_psi <- renderPlot({
    p <- ggplot(data_ad_experiment_filter(), aes(datetime, psi)) +
      geom_point() +
      theme_bw() +
      ylab(expression(paste(psi, " (MPa)")))
    p
  })
  output$pneumatron_plot_time_ad_ul <- renderPlot({
    p <- ggplot(data_ad_experiment_filter(), aes(datetime, ad_ul)) +
      geom_point() +
      theme_bw() +
      ylab(expression(paste("Air Discharge (", mu, "l)")))
    p
  })

  observeEvent(input$btn_save_data, {
    if(input$file_name_save == ""){
      session$sendCustomMessage(type = 'testmessage',
        message = 'You need to write a file name to save!')
    } else {
      #Save plots
      ggplot(data_ad_experiment_filter(), aes(psi, pad)) +
        geom_point() +
        theme_bw() +
        xlab(expression(paste(psi, " (MPa)"))) +
        ylab("Air Discharge (%)")
      ggsave(paste0("../fig/", input$file_name_save, "_psi_pad.png"))
      ggplot(data_ad_experiment_filter(), aes(psi, ad_ul)) +
        geom_point() +
        theme_bw() +
        xlab(expression(paste(psi, " (MPa)"))) +
        ylab(expression(paste("Air Discharge (", mu, "l)")))
      ggsave(paste0("../fig/", input$file_name_save, "_psi_ad_ul.png"))
      ggplot(data_ad_experiment_filter(), aes(datetime, psi)) +
        geom_point() +
        theme_bw() +
        ylab(expression(paste(psi, " (MPa)")))
      ggsave(paste0("../fig/", input$file_name_save, "_time_psi.png"))
      ggplot(data_ad_experiment_filter(), aes(datetime, ad_ul)) +
        geom_point() +
        theme_bw() +
        ylab(expression(paste("Air Discharge (", mu, "l)")))
      ggsave(paste0("../fig/", input$file_name_save, "_time_ad_ul.png"))

      #Save Table
      write.csv(data_ad_experiment_filter(),
                paste0("../result/", input$file_name_save, ".csv"),
                row.names = FALSE)

      #Save analysis log
      datetime_filter <- input$filter_experiment_datetime
      fileConn <- file(paste0("../result/analysi_log_", input$file_name_save, ".txt"))
      writeLines(c(paste("pneumatron id:", input$pneumatron_id),
                   paste("initial datetime:", datetime_filter[1]),
                   paste("final datetime:", datetime_filter[2]),
                   paste("water pressure file:", input$psi_file_input$name)), fileConn)
      close(fileConn) 
      session$sendCustomMessage(type = 'testmessage',
        message = 'Your experiment data is saved! Please, check "fig" and "result" folders')
    }
  })

  output$analysis_plots <- renderUI({
    fluidRow(
      column(
        width = 12,
        if(!is.null(input$psi_file_input)){
          box(
            width = 12,
            fluidRow(
              #column(
              #  width = 4,
              #  textInput(
              #    "title_analysis_plots",
              #    "Graphic Title:"
              #  )
              #),
              column(
                width = 4,
                textInput(
                  "file_name_save",
                  "File Name (Save):"
                )
              ),
              column(
                width = 4,
                actionButton("btn_save_data", "Save"),
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput("pneumatron_plot_psi_pad")
              ),
              column(
                width = 6,
                plotOutput("pneumatron_plot_psi_ad_ul")
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput("pneumatron_plot_time_psi")
              ),
              column(
                width = 6,
                plotOutput("pneumatron_plot_time_ad_ul")
              )
            )
          )
          } else {
            fluidRow(
              column(
                width = 12,
                align = "center",
                HTML("Water Pressure file is required, please attached it in <b>Analysis</b> -> <b>Filter Experiment</b> View!")
              )
            )
          }
      )
    )
  })

  output$filter_experiment_boxes <- renderUI({
    date_min = min(data_ad()$datetime)
    date_max = max(data_ad()$datetime)
    box(
      width = 12,
      fluidRow(
        column(
          width = 12,
          align = "center",
          sliderInput("filter_experiment_datetime",
                      label = "Time range",
                      min = date_min,
                      max = date_max,
                      value = c(date_min,
                                date_max),
                      timeFormat = "%F %H:%M",
                      step = 1,
                      width = "90%"
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "pneumatron_id",
            label = "Select Pneumatron ID",
            choices = unique(data_ad()$id)
          )
        ),
        column(
          width = 9,
          plotlyOutput("pneumatron_filtered_plot")
        )
      ),
      fluidRow(
        column(
          width = 3,
          HTML("The Water Pressure table needs to have two columns, <b>time</b> (dd.mm.yyyy hh:mm) and <b>pot</b> (MPa).<br>"),
          br(),
          fileInput(
            "psi_file_input",
            "Select your file with Water Pressure values:",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv")
          ),
          tableOutput('psi_file_table')
        ),
        column(
          width = 9,
          plotlyOutput("psi_plot_filter_view")
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Meassure Paramenters",
          status = "warning",
          collapsible = TRUE,
          collapsed = TRUE,
          column(
            width = 6,
            HTML("<b>Pneumatron Paramenters</b>"),
            p(),
            p("Define the time where you would have your initial (pi_s) and final (pf_s) pressures."),
            p("Time desired is usually 1.5 (initial pressure) and 15 (final pressure) seconds."),
            numericInput("pneumatron_initial_pressure",
                          label = "Initial Pressure",
                          value = 1.5),
            numericInput("pneumatron_final_pressure",
                          label = "Final Pressure",
                          value = 15),
            p("Define your tubing volume (in mL)"),
            numericInput("pneumatron_tubing",
                          label = "Tubing Volume",
                          value = 2.6)
          ),
          column(
            width = 6,
            HTML("<b>Enviroment Paramenters</b>"),
            p(),
            p("Define atmospheric pressure (in kPa)"),
            numericInput("env_atm_pressure",
                          label = "Atmospheric Pressure",
                          value = 101.3),
            numericInput("env_temp",
                          label = "Temperature (K)",
                          value = 293.15)
          )
        )
      )
    )
  })
 }