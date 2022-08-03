library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Experiments Managment",
             tabName = "exp_managment_view",
             icon = icon("folder") #filter-list
    ),
    menuItem("Running Experiments",
             tabName = "running_view",
             icon = icon("filter") #filter-list
    ),
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

dashboardPage(
  dashboardHeader(title = "Pneumatron"),
  sidebar,
  dashboardBody()
)