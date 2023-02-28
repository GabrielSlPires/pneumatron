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
          )
        ),
        tabItem(
          tabName = "analysis_plots_view",
          fluidRow(
            uiOutput("analysis_plots")
          )
        ),
        tabItem(
          tabName = "about_view",
          fluidRow(
            p("Version: 0.2.2")
          )
        )
    )
)

dashboardPage(
  dashboardHeader(title = "Pneumatron"),
  sidebar,
  body
)