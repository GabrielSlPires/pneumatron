packages <- c(
    "data.table",
    "dplyr",
    "ggplot2",
    "gridExtra",
    "lubridate",
    "plotly",
    "reshape2",
    "rstudioapi",
    "serial",
    "shiny",
    "shinydashboard",
    "shinyTime"
)

install.packages(setdiff(packages, rownames(installed.packages())))

rm(packages)

dir.create("data", showWarnings = FALSE)
dir.create("data/raw_pneumatron", showWarnings = FALSE)
dir.create("data/raw_psi", showWarnings = FALSE)
dir.create("data/raw_table", showWarnings = FALSE)
dir.create("fig", showWarnings = FALSE)
dir.create("result", showWarnings = FALSE)