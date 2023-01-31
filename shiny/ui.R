suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Database",
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
              title = "Database",
              collapsible = TRUE,
              status = "primary",
              width = 12,
              column(
                width = 6,
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
        ),
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