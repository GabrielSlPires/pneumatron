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
    id = "tabs",
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
             #menuSubItem("Save Experiment",
             #                    tabName = "save_experiment_view"),
             #menuSubItem("Experiments",
             #                    tabName = "experiments_view"),
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
          fluidRow(
            column(
              width = 12,
              h3("Measurements Diagnostic")
            )
          ),
          fluidRow(
            column(
              width = 12,
              box( 
                title = "Instructions",
                width = 12,
                status = "primary",
                collapsible = TRUE,
                collapsed = TRUE,
                fluidRow(
                  style = "text-align: justify; text-justify: inter-word;",
                  column(
                    width = 6,
                    HTML("In this chart, it is possible to diagnose your pneumatron measurements. Measures are identified by colors ranging from <b>oldest (blue)</b> to <b>newest (red)</b>. Within a measurement, <b>pressure (Y-axis)</b> is logged every 500ms, so each measure has 120 <b>log lines (X-axis)</b>.<br>"),
                    HTML("A pneumatron measurement consists of applying a partial vacuum to the cut-open vessels and then tracking pressure difference to calculate air discharge. To do this, only the pressure difference from log lines <b>3 to 30 (grey area)</b> is used. Thus, <b>if a pressure drop happens in this region during a measure, this one will be automatically discarted</b>. If all measurements of a pneumatron were discarded, <b>this device will not appear in the 'Running Experiments' tab</b>."),
                  ),
                  column(
                    width = 6,
                    HTML("Pressure drops happen every time the vacuum pump is activated. Thereby, if you have <b>pressure drops</b> on the first day of new branch measurements, it could be a <b>leakage indicator</b>. In this case, try to use <b>stronger clamps, thinner tube adapters, and glue</b>. It is common to have pressure drops outside the grey region in the final days of your experiment.<br>"),
                    HTML("To increase visibility, the first pressure log is not shown, because it's close to atmospheric pressure.<br>"),
                    HTML("Please, select the range you want to inspect. It will be automatically displayed only the <b>newest availabe day</b>. Note that those plots could take a while to display.")
                  ),
                ),
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              dateRangeInput("diagnostics_range_date",
                                label = 'Date range:'),
            ),
          ),
          fluidRow(
            column(
              width = 12,
              shiny_busy(),
              plotOutput(
                "plot_measure_diagnostic",
                #height = "75vh"
              )
            )
          )
        ),
        tabItem(
          tabName = "exp_managment_view",
          h1("Work in progress - Do not use it!!!!!"),
          fluidRow(
            column(
              width = 12,
              box(
                title = "Table Settings",
                width = 12,
                column(
                  width = 2,
                  align = "center",
                  h4("Add New Experiment"),
                  actionButton("experiment_add", "Add")
                ),
                column(
                  width = 2,
                  align = "center",
                  h4("Delete Selected Experiment"),
                  actionButton("experiment_delete", "Delete")
                ),
                column(
                  width = 2,
                  align = "center",
                  h4("Open Selected Experiment"),
                  actionButton("experiment_open", "Open")
                ),
                column(
                  width = 3,
                  align = "center",
                  fluidRow(
                    h4("Add a new Variable (Column)"),
                  ),
                  fluidRow(
                    column(
                      width = 8,
                      textInput("experiment_var_name", "Column Name"),
                    ),
                    column(
                      width = 4,
                      actionButton("experiment_var_add", "Add")
                    ) #add select input to remove column
                  )
                ),
                column(
                  width = 2,
                  align = "center",
                  h4("Save Experiment Table"),
                  actionButton("experiment_save", "Save")
                ),
              )
            )
          ), #end first row
          fluidRow(
            column(
              width = 12,
              box(
                title = "Experiments",
                width = 12,
                #add some text explaning
                #do something to allow only right format in columns - need to do it before data not atribuited
                DT::DTOutput("table_manage_experiments"),
                style = "overflow-x: scroll;"
              )
            )
          ), #end second row
          fluidRow(
            column(
              width = 12,
              box(
                title = "Data not atribuited to any experiment",
                width = 12,
                #DT::DTOutput
              )
            )
          ) #end third row
        ),
        tabItem(
          tabName = "running_view",
          fluidRow(
            column(
              width = 12,
              h3("Running Experiment",
                 width = 3,
                 style = "text-align: center;"),
            ),
          ),
          fluidRow(
            column(
              width = 12,
              HTML("<br>"),
              shiny_busy(),
            )
          ),
          fluidRow(
            running_exp_UI("running_experiments")
          ),
          fluidRow(
            column(
              width = 12,
              HTML("If a pneumatron that you are using is not appering here, please check <b>Measure Diagnostics</b> tab."),
            )
          ),
        ), # end Running Views

        tabItem(
          tabName = "analysis_filter_view",
          fluidRow(
            box(
              width = 12,
              fluidRow(
                column(
                  width = 2,
                  selectInput(
                    inputId = "pneumatron_id",
                    label = "Select Pneumatron ID",
                    choices = 0
                  )
                ),
                column(
                  width = 10, #8,
                  align = "center",
                  sliderInput("filter_experiment_gas_discharge",
                              label = "Gas Discharge",
                              min = 0,
                              max = 0,
                              value = c(0,0),
                              step = 1,
                              width = "90%"
                  )
                #),
                #column(
                #  width = 2,
                #  align = "center",
                #  h4("Save Experiment"),
                #  actionButton("send_to_experiment_table", "Save")
                )
              ),
              fluidRow(
                column(
                  width = 12, #8,
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
              title = "Mesurements",
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
              fluidRow(
                column(
                  width = 12,
                  HTML("<b>estimated</b> = Water pressure value it's aquired fitting the Pammenter and Willigen equation.<br>"),
                  HTML("<b>measure</b> = Water pressure value it's aquired due to pneumatron hight resolution, whitout fitting a curve.")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h4("Parameters Table"),
                  tableOutput("filter_view_p50_table")
                )
              )
            )
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
                collapsible = TRUE,
                collapsed = TRUE,
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
                                      value = 10,
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