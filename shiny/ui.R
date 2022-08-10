library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
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
    tabItems(
        tabItem(
            tabName = "running_view",
            fluidRow(
                actionButton("btn_refreash_data", "Refreash Data"),
                h3("Running Experiments"),
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
        )
    )
)

dashboardPage(
  dashboardHeader(title = "Pneumatron"),
  sidebar,
  body
)