suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))


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
            uiOutput("filter_experiment_boxes")
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