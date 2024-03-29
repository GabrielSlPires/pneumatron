shiny_busy <- function() { #https://community.rstudio.com/t/shiny-app-show-some-message-while-user-is-waiting-for-output/12822
  # use &nbsp; for some alignment, if needed
  HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
    '<span data-display-if="',
    '$(&#39;html&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;',
    '">',
    '<i class="fa fa-spinner fa-pulse fa-fw" style="color:orange"></i>',
    '</span>'
  ))
}

databases_tab <- tabItem(
  tabName = "databases",
  
  fluidRow(
    box(
      title = "Pneumatron File",
      status = "primary",
      width = 4,
      fluidRow(
        column(
          width = 12,
          align="center",
          shinyFilesButton('file_database',
                           label = 'File select',
                           title = 'Select a file to change your database',
                           multiple = FALSE)
        )
      ),
      fluidRow(
        column(
          width = 12,
          align="center",
          HTML("<br>"),
          uiOutput("open_data_ad")
        )
      ),
      fluidRow(
        column(
          width=12,
          h4("Configuration")
        )
      ),
      fluidRow(
        column(
          width = 12,
          checkboxInput("database_auto_update",
                        "Enable database auto-update",
                        value = FALSE),
          checkboxInput("calculate_air_discharge",
                        "Disable Air Discharged Calculation",
                        value = FALSE),
          h5("Air Discharged:"),
          shiny_busy(),
          uiOutput("calculate_data_ad")
        )
      ),
      fluidRow(
        column(
          width=12,
          h4("Water Potential File")
        )
      ),
      fluidRow(
        column(
          width = 12,
          HTML("The Water Pressure table needs to have three columns, <b>id</b>, <b>time</b> (dd.mm.yyyy hh:mm) and <b>pot</b> (MPa).<br>"),
          br(),
          fileInput(
            "psi_file_input",
            "Select your file with Water Pressure values:",
            accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")), #accept excel
          verbatimTextOutput("open_data_psi_status")
        )
      ),
    ),
    box(
      title = "Database Summary",
      status = "primary",
      width = 8,
      column(
        width = 12,
        DT::DTOutput("pneumatron_ids_table")
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
        DT::DTOutput('psi_file_table')
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
) # end Databases