packages <- c(
    "data.table",
    "dplyr",
    "ggplot2",
    "gridExtra",
    "lubridate",
    "plotly",
    "rstudioapi",
    "serial",
    "shiny",
    "shinydashboard"
)

install.packages(setdiff(packages, rownames(installed.packages())))

rm(packages)