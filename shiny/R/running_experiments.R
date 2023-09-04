library(purrr)

# create one pneumatron box with date range and plotly
pneumatron_box_ui <- function(id, pneumatron, data_ad) {
  stopifnot(is.reactive(data_ad))
  ns <- NS(id)
  date_min = min(data_ad()[data_ad()$id == pneumatron,]$datetime)
  date_max = max(data_ad()[data_ad()$id == pneumatron,]$datetime)
  
  column(4,
         box(title = paste("Pneumatron ID:", pneumatron),
             collapsible = TRUE,
             status = "primary",
             width = 12,
             dateRangeInput(ns("running_date_range"),
                            label = 'Date range:',
                            start = date_min,
                            end = date_max,
                            min = date_min,
                            max = date_max),
             plotlyOutput(ns("running_plotly"))
         )
  )
}

# create server for one pneumatron box
pneumatron_box_server <- function(id, pneumatron, data_ad) {
  stopifnot(is.reactive(data_ad))
  
  moduleServer(id, function(input, output, session) {
    
    output$running_plotly <- renderPlotly({
      datetime_filter <- input$running_date_range

      p <- ggplot(data_ad() %>%
                    filter(
                      id == pneumatron,
                      datetime >= datetime_filter[1],
                      datetime <= datetime_filter[2] + 1
                    ),
                  aes(datetime, ad_ul)) +
        geom_point() +
        scale_x_datetime(date_labels = "%b %d") +
        ylab("Air Discharge (ul)") +
        theme_bw()
      ggplotly(p)
    })
  })
}

running_exp_UI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("pneumatron_box"))
}

# create server for all pneumatron boxes
running_exp_Server <- function(id, data_ad) {
  req(data_ad)
  stopifnot(is.reactive(data_ad))
  
  moduleServer(id, function(input, output, session) {
    pneumatrons <- reactive(sort(unique(data_ad()$id)))

    # gather all pneumatron box in one unique ui
    output$pneumatron_box <- renderUI({
      map(pneumatrons(), function(pneumatron) {
        ns <- NS(id)
        pneumatron_box_ui(ns(pneumatron), pneumatron, data_ad)
      })
    })
    
    # create server for each pneumatron box
    observeEvent(pneumatrons(), {
      map(pneumatrons(), function(pneumatron) {
        pneumatron_box_server(pneumatron, pneumatron, data_ad)
      })
    })
    
  })
}