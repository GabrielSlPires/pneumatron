databases_tab <- tabItem(
  tabName = "databases",
  
  fluidRow(
    box(
      title = "Pneumatron",
      status = "primary",
      width = 12,
      column(
        width = 12,
        fluidRow(
          fileInput(
            "file_database",
            "Select a file to change your database",
            accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"),
            width = "100%")
        ),
        fluidRow(
          verbatimTextOutput("open_data_ad")
        )
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
          ".csv")), #accept excel
        verbatimTextOutput("open_data_psi"),
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
) # end Databases