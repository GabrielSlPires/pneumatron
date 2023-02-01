suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(plotly))


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
    #menuItem("Experiments Managment",
    #         tabName = "exp_managment_view",
    #         icon = icon("folder") #filter-list
    #),
    menuItem("Analysis",
             menuSubItem("Filter Experiments",
                                 tabName = "analysis_filter_view"),
             menuSubItem("Plots",
                                 tabName = "analysis_plots_view"),
             tabName = "analysis_view",
             icon = icon("chart-bar")
    ),
    menuItem("About",
             icon = icon("th"),
             tabName = "about_vire"
    )
  ),
  menuItem("Source code",
           icon = icon("file-code"),
           href = "https://github.com/GabrielSlPires/pneumatron")
)

body <- dashboardBody(
  tags$head(tags$script(src = "message-handler.js")),
    tabItems(
        tabItem(
          tabName = "databases",
          fluidRow(
            box(
              title = "Pneumatron",
              status = "primary",
              width = 12,
              column(
                width = 12,
                fileInput(
                  "file_database",
                  "Select a file to change your database",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"),
                  width = "100%"
                )
              ),
              column(
                width = 6,
                actionButton("btn_refreash_data", "Refreash Data"),
              )
            )
          ),
          fluidRow(
            box(
              title = "Water Potential",
                          status = "primary",
              width = 12,
              column(
                width = 3,
                HTML("The Water Pressure table needs to have three columns, <b>id</b>, <b>time</b> (dd.mm.yyyy hh:mm) and <b>pot</b> (MPa).<br>"),
                br(),
                fileInput(
                  "psi_file_input",
                  "Select your file with Water Pressure values:",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv") #accept excel
                ),
                tableOutput('psi_file_table')
              ),
              column(
                width = 9,
                plotlyOutput("psi_plot_databases_view")
              )
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
        ), # end Databases

        tabItem(
          tabName = "running_view",
          fluidRow(
            h3("Running Experiment")
          ),
          fluidRow(
              uiOutput("pneumatron_plots")
          )
        ),
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
                plotOutput("pneumatron_plot_time_psi")
              ),
              column(
                width = 6,
                h3("Water Pressure <b>x</b> Air Discharged"),
                plotOutput("pneumatron_plot_psi_ad_ul")
              )
            )
          )
        ),
        tabItem(
          tabName = "analysis_plots_view",
          fluidRow(
            uiOutput("analysis_plots")
          )
        )
    )
)

dashboardPage(
  dashboardHeader(title = "Pneumatron"),
  sidebar,
  body
)