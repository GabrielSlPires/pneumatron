options(shiny.maxRequestSize = 300*1024^2)

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(shinyFiles))

source("helper.R", local = TRUE)

#file_name <- "2022_08_01"

server <- function(input, output, session) {

  #import pneumatron database ----------------------------------------------------
  output$open_data_ad <- renderUI(HTML("Waiting for Pneumatron Database upload."))

  shinyFileChoose(input, 'file_database', root=c(root='../data'), session=session)

  data_raw <- reactive({
    req(input$file_database)
    if (!is.null(input$file_database)) {
      file_selected <- parseFilePaths(root=c(root='../data'), input$file_database)
      file_path <- as.character(file_selected$datapath)
      }

    req(file_path)
    output$open_data_ad <- renderUI(HTML("Loading..."))

    data <- tryCatch({
      if (input$database_auto_update) invalidateLater(900000, session)
      data <- open_pneumatron_db(file_path)
      output$open_data_ad <- renderUI(HTML("Pneumatron Database is ready!"))
      return(data)

    }, error = function(e) {
      output$open_data_ad <- renderUI(HTML("Failed to open Pneumatron Database"))
      return(FALSE)
    })
    return(data)
  })
  data_ad <- reactive({
    req(data_raw())
    data_ad <- tryCatch({
      data <- pneumatron_air_discharge(data_raw())
      output$calculate_data_ad <- renderUI(HTML("Pneumatron Air Discharged is ready!"))
      return(data)
    }, error = function(e){
      output$air_discharge_error <- renderText(e)
      output$calculate_data_ad <- renderUI({
        HTML("Failed to Calculate Air Discharged!")
        verbatimTextOutput("air_discharge_error")
      })
      req(FALSE)
    })
  })

  #create plots (running experiments) for each different device in ui
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
        psi_point <- list()
        try({
          req(data_psi())
          psi <- dplyr::filter(data_psi(),
                               id == my_i)
          psi_point <- list(
            geom_vline(data = psi,
                       aes(xintercept = as.numeric(time)),
                       alpha = 0.5)
          )
        }, silent = TRUE)

        p <- ggplot(data_ad() %>%
                 filter(
                  id == my_i,
                  datetime >= datetime_filter[1],
                  datetime <= datetime_filter[2] + 1
                 ),
               aes(datetime, ad_ul)) +
          geom_point() +
          psi_point +
          scale_x_datetime(date_labels = "%b %d") +
          ylab("Air Discharge (ul)") +
          theme_bw()
        ggplotly(p)
      })
    })
  }

  output$psi_plot_filter_view <- renderPlotly({
      req(data_psi())
      p <- ggplot(dplyr::filter(data_psi(), id == input$pneumatron_id),
      aes(time, pot, group = 1)) +
        geom_line() +
        theme_bw()
      ggplotly(p)
  })

  output$psi_plot_databases_view <- renderPlotly({
      req(data_psi())
      p <- ggplot(na.omit(data_psi()),
      aes(time, pot, group = factor(id), color = factor(id))) +
        geom_line() +
        theme_bw() +
        scale_color_brewer(name = "Pneumatron",
                           palette = "Set1")
      ggplotly(p)
  })

  #render data_psi in page
  output$psi_file_table <- renderTable({
    req(data_psi())
    df <- dplyr::arrange(data_psi(), id, time)
    df$time <- as.character(df$time)
    return(df)
  })

  #read data_psi file
  output$open_data_psi <- renderText("Waiting for table upload.")
  data_psi <- reactive({
    #read only if file is uploaded
    req(input$psi_file_input)
    df <- tryCatch(
      {
        if(!validate_data_psi(input$psi_file_input$datapath)) stop()
        df <- data.table::fread(input$psi_file_input$datapath, fill=TRUE)
        df$time <- lubridate::dmy_hm(df$time)
        df <- dplyr::filter(df, !is.na(id))

        output$open_data_psi <- renderText("Table is ready!")
        return(df)
      },
      error = function(e) {
        output$open_data_psi <- renderText("Failed to open.")
        req(FALSE)
      }
    )
  })

  data_ad_experiment_filter <- reactive({
    datetime_filter <- input$filter_experiment_datetime
    data <- data_ad() %>%
      filter(datetime >= datetime_filter[1],
             datetime <= datetime_filter[2],
             id == input$pneumatron_id) %>%
      mutate(pad = ((ad_ul - min(ad_ul))/(max(ad_ul) - min(ad_ul)))*100)
    if(!is.null(input$psi_file_input)){
      psi <- dplyr::filter(data_psi(), id == input$pneumatron_id, !is.na(pot))
      data <- extrapolated_wp(data, psi)
    }
    return(data)
  })

  output$pneumatron_filtered_plot <- renderPlotly({
    p <- ggplot(data_ad_experiment_filter(), aes(datetime, ad_ul)) +
      geom_point() +
      theme_bw()
    ggplotly(p)
  })

  #Vulnerability curve parameters
  p50_table <- reactive(pneumatron_p50(dplyr::select(data_ad_experiment_filter(), pad, psi)))

  output$filter_view_p50_table <- renderTable({
    req(p50_table())

    df <- data.frame(variable = names(p50_table()),
                     estimated = p50_table(),
                     measured = c(NA,
                                  pneumatron_px_proximity(data_ad_experiment_filter(), 12),
                                  pneumatron_px_proximity(data_ad_experiment_filter(), 50),
                                  pneumatron_px_proximity(data_ad_experiment_filter(), 88)))
    return(df)
  })

  #Analysis plots
  output$pneumatron_plot_psi_pad <- renderPlot(plot_psi_pad())
  plot_psi_pad <- reactive({
    #apply non linear fit

    #dar opção de escrever posição da tabela
    table_x_min <- data_ad_experiment_filter() %>% 
      mutate(psi = psi*-1,
             x_axis_norm = (psi - min(psi))/(max(psi) - min(psi))*100) %>%
      slice(which.min(abs(x_axis_norm - input$p50_plot_x_axis_min))) %>%
      mutate(psi = psi*-1) %>%
      select(psi) %>% 
      as.numeric()

    table_x_max <- data_ad_experiment_filter() %>% 
      mutate(psi = psi*-1,
             x_axis_norm = (psi - min(psi))/(max(psi) - min(psi))*100) %>%
      slice(which.min(abs(x_axis_norm - input$p50_plot_x_axis_max))) %>%
      mutate(psi = psi*-1) %>%
      select(psi) %>% 
      as.numeric()

    p <- ggplot(data_ad_experiment_filter(),
                aes(psi, pad)) +
      geom_point() +
      stat_function(fun = function(x) 100/(1 + exp(p50_table()["a"]*(x - p50_table()["p50"]))),
                color = "royalblue", 
                size = 1) +
      geom_vline(xintercept = p50_table()["p50"]) + 
      theme_bw() +
      ggtitle(input$title_analysis_plots) +
      xlab(expression(paste(psi, " (MPa)"))) +
      ylab("Air Discharge (%)") + 
      annotation_custom(tableGrob(data.frame(estimated = round(p50_table(), 2)),
                              theme = ttheme_minimal()),
                    xmin = table_x_min,
                    xmax = table_x_max,
                    ymin = input$p50_plot_y_axis_min,
                    ymax = input$p50_plot_y_axis_max)

    p
  })

  output$pneumatron_filter_psi_ad_ul <- renderPlotly({
    p <- ggplot(data_ad_experiment_filter(), aes(psi, ad_ul)) +
      geom_point() +
      theme_bw()
    ggplotly(p)
  })
  output$pneumatron_plot_psi_ad_ul <- renderPlot(plot_psi_ad_ul())
  plot_psi_ad_ul <- reactive({
    p <- ggplot(data_ad_experiment_filter(), aes(psi, ad_ul)) +
      geom_point() +
      theme_bw() +
      ggtitle(input$title_analysis_plots) +
      xlab(expression(paste(psi, " (MPa)"))) +
      ylab(expression(paste("Air Discharge (", mu, "l)")))
    p
  })

  output$pneumatron_filter_time_psi <- renderPlotly({
    p <- ggplot(data_ad_experiment_filter(), aes(datetime, psi)) +
      geom_point() +
      theme_bw()
    ggplotly(p)
  })
  output$pneumatron_plot_time_psi <- renderPlot(plot_time_psi())
  plot_time_psi <- reactive({
    p <- ggplot(data_ad_experiment_filter(), aes(datetime, psi)) +
      geom_point() +
      theme_bw() +
      ggtitle(input$title_analysis_plots) +
      ylab(expression(paste(psi, " (MPa)")))
    p
  })
  output$pneumatron_plot_time_ad_ul <- renderPlot(plot_time_ad_ul())
  plot_time_ad_ul <- reactive({
    p <- ggplot(data_ad_experiment_filter(), aes(datetime, ad_ul)) +
      geom_point() +
      theme_bw() +
      ggtitle(input$title_analysis_plots) +
      ylab(expression(paste("Air Discharge (", mu, "l)")))
    p
  })

  output$experiment_data <- renderTable({
    df <- data.frame(
      Parameter = c(
        "Pneumatron ID:",
        "Plant ID:",
        "Initial Date:",
        "End Date:",
        "Database:",
        "Water Measures:"
        ),
      Value =c(
        input$pneumatron_id,
        NA,
        input$filter_experiment_datetime[1],
        input$filter_experiment_datetime[2],
        as.character(parseFilePaths(root=c(root='../data'), input$file_database)$datapath),
        input$psi_file_input$name
        )
    )
    print(df)
    return(df)
  })

  observeEvent(input$btn_save_data, {
    if(input$file_name_save == ""){
      session$sendCustomMessage(type = 'testmessage',
        message = 'You need to write a file name to save!')
    } else {
      #Save plots
      ggsave(paste0("../fig/", input$file_name_save, "_psi_pad.png"), plot_psi_pad())
      ggsave(paste0("../fig/", input$file_name_save, "_psi_ad_ul.png"), plot = plot_psi_ad_ul())
      ggsave(paste0("../fig/", input$file_name_save, "_time_psi.png"), plot_time_psi())
      ggsave(paste0("../fig/", input$file_name_save, "_time_ad_ul.png"), plot_time_ad_ul())

      #Save Table
      write.csv(data_ad_experiment_filter(),
                paste0("../result/", input$file_name_save, ".csv"),
                row.names = FALSE)

      #Save analysis log
      datetime_filter <- input$filter_experiment_datetime
      fileConn <- file(paste0("../result/analysis_log_", input$file_name_save, ".txt"))
      writeLines(c(paste("pneumatron id:", input$pneumatron_id),
                   paste("initial datetime:", datetime_filter[1]),
                   paste("final datetime:", datetime_filter[2]),
                   paste("water pressure file:", input$psi_file_input$name)), fileConn)
      close(fileConn) 
      session$sendCustomMessage(type = 'testmessage',
        message = 'Your experiment data is saved! Please, check fig and result folders')
    }
  })

  # Filter Experiments View -------------------------------------------------------
  observe({
    data_ad()
    updateSelectInput(session,
                      inputId = "pneumatron_id",
                      choices = unique(data_ad()$id)
                      )
  })

  observe({
    if (input$pneumatron_id != 0) {
      data <- data_ad() %>% 
        dplyr::filter(id == input$pneumatron_id) %>%
        dplyr::select(datetime) %>% 
        unique()
      date_min = min(data$datetime)
      date_max = max(data$datetime)
      updateSliderInput(session,
                        inputId = "filter_experiment_datetime",
                        min = date_min,
                        max = date_max,
                        value = c(date_min,
                                  date_max)
                        )
    }
  })
 }