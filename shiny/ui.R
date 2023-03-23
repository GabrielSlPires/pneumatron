suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(shinyFiles))

sapply(list.files("ui/",
                  pattern = "*.R",
                  full.names = TRUE),
       source)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Databases",
             tabName = "databases",
             icon = icon("database") #filter-list
    ),
    menuItem("Running Experiments",
             tabName = "running_view",
             icon = icon("filter") #filter-list
    ),
    menuItem("Measure Diagnostics",
             tabName = "measure_diagnostics",
             icon = icon("magnifying-glass")),
    menuItem("Analysis",
             menuSubItem("Filter Experiments",
                                 tabName = "analysis_filter_view"),
             menuSubItem("Save Experiment",
                                 tabName = "save_experiment_view"),
             menuSubItem("Experiments",
                                 tabName = "experiments_view"),
             menuSubItem("Plots",
                                 tabName = "analysis_plots_view"),
             tabName = "analysis_view",
             icon = icon("chart-bar")
    ),
    menuItem("About",
             icon = icon("th"),
             tabName = "about_view"
    )
  ),
  menuItem("Source code",
           icon = icon("file-code"),
           href = "https://github.com/GabrielSlPires/pneumatron")
)

body <- dashboardBody(
  tags$head(tags$script(src = "message-handler.js")),
    tabItems(
        databases_tab,
        tabItem(
          tabName = "measure_diagnostics",
          #plot last day only
          fluidRow(
            column(
              width = 4,
              dateInput(
                "diagnostics_initial_date",
                "Initial date for Measure Diagnostic"
              ),
            ),
            column(
              width = 8,
              p("To calculate air discharged for each measurements we use the difference between log line 3 to 30 (vertical lines)"),
              p("Each measure has 120 log line points, because Pneumatron reads pressure every 500ms.")
            )
          ),
          fluidRow(
            column(
              width = 12,
              shiny_busy(),
              plotOutput(
                "plot_measure_diagnostic",
                height = "75vh"
              )
            )
          )
        ),
        tabItem(
          tabName = "running_view",
          fluidRow(
            column(
              width = 12,
              align = "center",
              h3("Running Experiment"),
              HTML("<br>")
            )
          ),
          fluidRow(
              uiOutput("pneumatron_plots")
          )
        ), # end Running Views

        tabItem(
          tabName = "analysis_filter_view",
          fluidRow(
            box(
              width = 12,
              fluidRow(
                column(
                  width = 3,
                  selectInput(
                    inputId = "pneumatron_id",
                    label = "Select Pneumatron ID",
                    choices = 0
                  )
                ),
                column(
                  width = 9,
                  align = "center",
                  sliderInput("filter_experiment_datetime",
                              label = "Time range",
                              min = 0,
                              max = 0,
                              value = c(0,0),
                              timeFormat = "%F %H:%M",
                              step = 1,
                              width = "90%"
                  )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Selected Mesurements",
              width = 12,
              column(
                width = 6,
                h3("Water Pressure"),
                plotlyOutput("psi_plot_filter_view")
              ),
              column(
                width = 6,
                h3("Air Discharged"),
                plotlyOutput("pneumatron_filtered_plot")
              )
            )
          ),
          fluidRow(
            box(
              title = "Extrapolated Values",
              width = 12,
              column(
                width = 6,
                h3("Water Pressure"),
                plotlyOutput("pneumatron_filter_time_psi")
              ),
              column(
                width = 6,
                h3("Water Pressure x Air Discharged"),
                plotlyOutput("pneumatron_filter_psi_ad_ul")
              )
            )
          ),
          fluidRow(
            box(
              title = "Vulnerability Curve Paramenters",
              width = 6,
              column(
                width = 12,
                h3("Parameters Table"),
                tableOutput("filter_view_p50_table")
              )
            )
          )
        ),

        tabItem(
          tabName = "save_experiment_view",
          fluidRow(
            column(
              width = 12,
              h2 = "Save Experiment"
            )
          ),
          fluidRow(
            tableOutput("experiment_data")
          )
        ),


        tabItem(
          tabName = "analysis_plots_view",
          fluidRow(
            column(
              width = 12,
              align = "center",
              HTML("<b>Pneumatron Database</b> and <b>Water Pressure</b> file are required for this view, please attached them in <b>Databases</b> View!")
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                title = "Plot Customization",
                width = 6,
                column(
                  width = 6,
                  fluidRow(
                    column(
                      width = 12,
                      align = "center",
                      h4("P50 table position"),
                    ),
                    fluidRow(
                      column(
                        width = 6,
                        align = "center",
                        fluidRow(
                          numericInput("p50_plot_x_axis_min",
                                      label = ("X axis min (%)"), 
                                      width = "80%",
                                      min = 0,
                                      max = 100,
                                      value = 75,
                                      step = 1.0)
                        ),
                        fluidRow(
                          numericInput("p50_plot_x_axis_max",
                                      label = ("X axis max (%)"), 
                                      width = "80%",
                                      min = 0,
                                      max = 100,
                                      value = 100,
                                      step = 1.0)
                        )
                      ),
                      column(
                        width = 6,
                        fluidRow(
                          numericInput("p50_plot_y_axis_min",
                                      label = ("Y axis min (%)"), 
                                      width = "80%",
                                      min = 0,
                                      max = 100,
                                      value = 0,
                                      step = 1.0)
                        ),
                        fluidRow(
                          numericInput("p50_plot_y_axis_max",
                                      label = ("Y axis max (%)"), 
                                      width = "80%",
                                      min = 0,
                                      max = 100,
                                      value = 25,
                                      step = 1.0)
                        )
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  textInput(
                    "title_analysis_plots",
                    "Graphic Title:"
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                width = 12,
                fluidRow(
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
            )
          )
        ),
        tabItem(
          tabName = "about_view",
          fluidRow(
            p("Version: 0.3 - Release: 28/02/2023")
          )
        )
    )
)

dashboardPage(
  dashboardHeader(title = "Pneumatron"),
  sidebar,
  body
)