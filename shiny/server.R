options(shiny.maxRequestSize = 300*1024^2)

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(shinyFiles))
suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(RColorBrewer))

options(shiny.usecairo = TRUE)

source("helper.R", local = TRUE)

experiment_path <- "../data/experiments.csv"

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

  output$pneumatron_ids_table <- DT::renderDT({
    req(data_raw())
    data <- data_raw() %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(measure = as.character(ceiling(n()/120)),
                       initial_measure = as.character(min(datetime)),
                       last_update = as.character(max(datetime)),
                       voltage = as.character(round(mean(volt), 2))) %>%
      dplyr::arrange(desc(last_update))
    
    DT::datatable(data,
                  rownames = FALSE
                  #options = list(
                  #  pageLength = 5,
                  #  lengthMenu = c(5, 10, 15, 20)
                  #  )
                  )
  })

  data_ad <- reactive({
    input$calculate_air_discharge
    req(data_raw())
    req(!input$calculate_air_discharge)
    data_ad <- tryCatch({
      data <- calculate_air_discharge(data_raw())
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

  running_exp_Server("running_experiments", data_ad, data_psi)

  output$psi_plot_filter_view <- renderPlotly({
    req(data_psi())
    datetime_filter <- input$filter_experiment_datetime

    p <- ggplot(dplyr::filter(data_psi(), id == input$pneumatron_id),
                aes(time, pot, group = 1)) +
      geom_rect(aes(x = NULL,
                    y = NULL,
                    xmin = datetime_filter[1] - as.difftime(1, unit = "days"),
                    xmax = datetime_filter[2]),
                fill = "grey70",
                ymin = -10,
                ymax = 1,
                colour = "white",
                size = 0.5,
                alpha = 0.2) +
      geom_point() +
      geom_line() +
      theme_bw()
    ggplotly(p)
  })

  output$psi_plot_databases_view <- renderPlotly({
      req(data_psi())
      #Dinamically change color pallete n()
      colourCount = length(unique(data_psi()$id))
      getPalette = colorRampPalette(brewer.pal(7, "Set1"))

      p <- ggplot(na.omit(data_psi()),
      aes(time, pot, group = factor(id), color = factor(id))) +
        geom_line() +
        geom_point(size = 2) +
        theme_bw() +
        scale_color_manual(name = "Pneumatron",
                           values = getPalette(colourCount))
      ggplotly(p)
  })

  #render data_psi in page
  output$psi_file_table <- DT::renderDT({
    req(data_psi())
    df <- dplyr::arrange(data_psi(), id, time)
    df$time <- as.character(df$time)
    return(DT::datatable(df))
  })

  #read data_psi file
  output$open_data_psi_status <- renderText("Waiting for table upload.")
  data_psi <- eventReactive(input$psi_file_input, {

    req(input$psi_file_input)
    df <- tryCatch(
      {
        if(!validate_data_psi(input$psi_file_input$datapath)) stop()
        df <- data.table::fread(input$psi_file_input$datapath, fill = TRUE)
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


  #------------- Measure Diagnostic
  observe({
    data_raw()

    date_max = max(data_raw()$datetime)
    date_min = min(data_raw()$datetime)

    updateDateRangeInput(session,
                         inputId = "diagnostics_range_date",
                         label = 'Date range:',
                         start = date_max,
                         end = date_max,
                         min = date_min,
                         max = date_max)
  })

  output$plot_measure_diagnostic <- renderPlot({
    req(data_raw())
    req(input$diagnostics_range_date)
    
    data <- tryCatch({
      mutate(data_raw(), unique_var = paste(measure, group))
    }, error = function(e) {
      mutate(data_raw(), unique_var = measure)
    })
    
    data %>% 
      filter(datetime >= lubridate::ymd(input$diagnostics_range_date[1]),
             datetime <= lubridate::ymd(input$diagnostics_range_date[2]) + 1,
             pressure < 85) %>% 
      group_by(id) %>% 
      ggplot(aes(log_line,
                 pressure,
                 color = datetime,
                 group = unique_var)) +
      geom_rect(aes(xmin = 3, xmax = 30, ymax = Inf, ymin = -Inf), color = "grey70") +
      geom_line() +
      geom_vline(aes(xintercept = 3)) +
      geom_vline(aes(xintercept = 30)) +
      facet_wrap(~id, scales = "free") +
      ylab("Pressure (kPa)") +
      xlab("log line") +
      ggtitle("Pressure difference inside each measurement by Pneumatron") +
      theme_bw() +
      scale_color_datetime(low = "blue", high = "red")
      
  })
  #-------------

  #------------- Filter Experiments View

  observeEvent(input$send_to_experiment_table, {
    req(experiments_table())
    req(data_ad_experiment_filter())

    this_exp <- data.frame(
      exp_id = max(experiments_table()$exp_id) + 1,
      pneumatron = input$pneumatron_id,
      start_datetime = input$filter_experiment_datetime[1],
      final_datetime = input$filter_experiment_datetime[2],
      finished = TRUE,
      database = gsub("../data/",
                      "",
                      as.character(parseFilePaths(root=c(root='../data'), input$file_database)$datapath)),
      water_potential = input$psi_file_input$name
    )

    data <- data.frame(experiments_table())

    data[setdiff(names(this_exp), names(data))] <- NA
    this_exp[setdiff(names(data), names(this_exp))] <- NA

    data = rbind(this_exp, data)
    experiments_table(data)
    updateTabItems(session, "tabs", "exp_managment_view")
  })

  data_ad_experiment_filter <- reactive({
    datetime_filter <- input$filter_experiment_datetime
    data <- data_ad() %>%
      filter(datetime >= datetime_filter[1],
             datetime <= datetime_filter[2],
             id == input$pneumatron_id) %>%
      mutate(pad = ((ad_ul - min(ad_ul))/(max(ad_ul) - min(ad_ul)))*100)
    if (!is.null(input$psi_file_input)) {
      psi <- dplyr::filter(data_psi(),
                           id == input$pneumatron_id,
                           time >= datetime_filter[1] - as.difftime(1, unit = "days"),
                           time <= datetime_filter[2],
                           !is.na(pot))
      try(data <- extrapolated_wp(data, psi))
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
  p50_table <- reactive({
    req(data_ad_experiment_filter())
    pneumatron_p50(dplyr::select(data_ad_experiment_filter(), pad, psi))
  })

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
    tryCatch({
      p <- ggplot(data_ad_experiment_filter(), aes(psi, ad_ul)) +
        geom_point() +
        theme_bw()
      ggplotly(p)
    },
    error = function(c) {
      stop("Water Potential file is missing, or points for more than one curve are filtered.")
    })
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
    tryCatch({
      p <- ggplot(data_ad_experiment_filter(), aes(datetime, psi)) +
        geom_point() +
        theme_bw()
      ggplotly(p)
    },
    error = function(c) {
      stop("Water Potential file is missing, or points for more than one curve are filtered.")
    })
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

  #---------- Manage Experiments
  #https://stackoverflow.com/questions/57581690/combining-editable-dt-with-add-row-functionality
  
  #disable to release
  #experiments_table <- reactiveVal(data.table::fread(experiment_path))

  observeEvent(input$table_manage_experiments_cell_edit, {
    info <- input$table_manage_experiments_cell_edit
    edit_row <-  info$row
    edit_col <-  info$col + 1
    edit_value <-  info$value

    data = data.frame(experiments_table())
    data[as.numeric(edit_row),
         as.numeric(edit_col)] <- edit_value
    experiments_table(data)
  })

  observeEvent(input$experiment_add, {
    new_line <- data.frame(exp_id = max(experiments_table()$exp_id) + 1,
                           pneumatron = NA,
                           start_datetime = NA,
                           final_datetime = NA,
                           finished = NA,
                           database = NA,
                           water_potential = NA)

    data <- data.frame(experiments_table())

    data[setdiff(names(new_line), names(data))] <- NA
    new_line[setdiff(names(data), names(new_line))] <- NA

    data <- rbind(data, new_line)

    experiments_table(data)
  })

  observeEvent(input$experiment_delete, {
    data = experiments_table()
    if (!is.null(input$table_manage_experiments_rows_selected)) {
      data <- data[-as.numeric(input$table_manage_experiments_rows_selected),]
    }
    experiments_table(data)
  })

  observeEvent(input$experiment_open, { #pegar somente o primeiro
    data = experiments_table()
    if (!is.null(input$table_manage_experiments_rows_selected)) {
      data <- data[-as.numeric(input$table_manage_experiments_rows_selected),]
    }

    updateTabItems(session, "tabs", "analysis_filter_view")

  })

  observeEvent(input$experiment_var_add, {

    req(!input$experiment_var_name %in% c(colnames(experiments_table()), ""))
    data <- data.frame(experiments_table())
    data[input$experiment_var_name] <- NA

    experiments_table(data)
  })

  observeEvent(input$experiment_save, {
    data.table::fwrite(experiments_table(), experiment_path)
  })

  output$table_manage_experiments <- DT::renderDT({
    DT::datatable(experiments_table(),
                  rownames = FALSE,
                  editable = TRUE)
  })

  #----------
 }