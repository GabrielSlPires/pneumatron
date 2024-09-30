exp_info <- function() {
  species_names <- data.table::fread("data/coffea/experiments-example.csv", blank.lines.skip = TRUE)
  
  other_info <- species_names[,.(id, species, diametro, id_planta)]
  
  other_info <- rbind(
    other_info,
    data.frame(
      id = 74:76,
      species = "C. arabica (Germany)"
    ),
    fill = TRUE
  )
  
  other_info <- rbind(
    other_info,
    data.frame(
      id = 77:81,
      species = "23-01-09"
    ),
    fill = TRUE
  )
  
  other_info <- rbind(
    other_info,
    data.frame(
      id = 82:88,
      species = "23-01-26"
    ),
    fill = TRUE
  )
  
  other_info <- rbind(
    other_info,
    data.frame(
      id = 89:95,
      species = "23-01-31"
    ),
    fill = TRUE
  )
  
  # other_info <- rbind(
  #   other_info,
  #   data.frame(
  #     id = 96:105,
  #     species = "C. arabica"
  #   ),
  #   fill = TRUE
  # )
  
  other_info$species[other_info$species == "Madasgacar"] <- "Madagascar"
  
  other_info
}

# perform spline to get derivate

gas_discharge_derivate <- function(df) {
  # df <- df[!is.na(datetime)]
  # df <- df[!is.na(gd_ul)]
  df <- df[order(datetime)]
  spl <- smooth.spline(x = df$datetime, y = df$gd_ul)
  pred <- predict(spl)
  pred.prime <- predict(spl, deriv = 1)
  df$smooth <- spl$y
  df$derivative <- pred.prime$y
  
  return(df)
}

vc_derivate <- function(df) {
  df <- df[!is.na(psi)]
  df <- df[!is.na(gd_ul)]
  df <- df[order(psi)]
  spl <- smooth.spline(x = df$psi, y = df$gd_ul)
  pred <- predict(spl)
  pred.prime <- predict(spl, deriv = 1)
  df$smooth <- spl$y
  df$derivative <- pred.prime$y
  
  return(df)
}


get_pammenter_max <- function(model) {
  x <- coef(model)[["p50"]]
  target <- 0
  while (target < 99 & x > -100) {
    x <- x - 1
    target <- 100/(1 + exp(coef(model)["a"]*(x - coef(model)["p50"])))
  }
  x
}
get_pammenter_min <- function(model) {
  x <- coef(model)[["p50"]]
  target <- 100
  while (target > 1 & x < 100) {
    x <- x + 1
    target <- 100/(1 + exp(coef(model)["a"]*(x - coef(model)["p50"])))
  }
  x
}

GeomSigmoidCI <- ggproto(
  "GeomPammenter", Geom,
  required_aes = c("p50", "p50min", "p50max",
                   "a", "amin", "amax"),
  
  default_aes = aes(
    colour = "black", fill = "grey20", linewidth = 0.5,
    linetype = 1, alpha = 0.3
  ),
  
  draw_key = draw_key_crossbar,
  
  draw_group = function(data, panel_params, coord, n) {
    pammenter_func <- function(x, a = 1, b = -5) 100/(1 + exp(a*(x - b)))
    data$x <- NULL
    data$y <- NULL
    parameters <- unique(data)
    
    parameters_len <- length(parameters$p50)
    
    if (parameters_len != length(unique(parameters$group))) parameters$group = seq_len(parameters_len)
    
    rng <- panel_params$x$get_limits()
    data <- data.frame()
    xs <- data.frame(x = seq(rng[1], rng[2], length.out = n))
    
    for (row in seq_len(nrow(parameters))) {
      row_par <- parameters[row,]
      points <- data.frame(xs, row_par, row.names = NULL)
      points$y <- pammenter_func(points$x, row_par$a, row_par$p50)
      points$ymax <- pammenter_func(points$x, row_par$amin, row_par$p50min)
      points$ymin <- pammenter_func(points$x, row_par$amax, row_par$p50max)
      data <- rbind(data, points)
    }
    
    data_line <- data
    data_line$alpha <- 1
    
    data_ribbon <- data
    data_ribbon$colour <- NA
    
    
    # Draw the daytime and nighttime rectangles on the panel
    grid::gList(
      ggplot2::GeomRibbon$draw_panel(data_ribbon, panel_params, coord),
      ggplot2::GeomLine$draw_panel(data_line, panel_params, coord)
    )
  }
)

geom_sigmoid_ci <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", na.rm = FALSE, show.legend = NA, 
                            inherit.aes = FALSE, n = 50, ...) {
  layer(
    geom = GeomSigmoidCI, mapping = mapping, data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}


extrapolated_wp <- function(pneumatron, #pneumatron data file
                            data_psi, #pressure pump data
                            pneumatron_time_column = "datetime", #pneumatron time column name
                            data_psi_time_column = "time", #data_psi time column name
                            data_psi_pot_column = "pot" #data_psi pressure column name
){
  library(dplyr)
  
  #fix column name for pneumatron table
  pneumatron <- pneumatron %>%
    rename(psi_time_estimate = all_of(pneumatron_time_column)) %>% 
    arrange(psi_time_estimate)
  
  #fix column name for data_psi table
  data_psi <- data_psi %>%
    rename(psi_time_estimate = all_of(data_psi_time_column),
           pot = all_of(data_psi_pot_column))
  
  psi_predict <- c()
  
  model <- lm(pot ~ psi_time_estimate, data = data_psi[c(1, 2),])
  a <- pneumatron %>% 
    filter(psi_time_estimate < data_psi$psi_time_estimate[1]) %>% 
    select(psi_time_estimate)
  #predict psi in function of time
  psi_predict <- c(psi_predict, predict(model, a))
  
  #for each line of data_psi, predict psi value with linear model
  for (i in seq(2, nrow(data_psi))) {
    #generate model of psi in function of time
    model <- lm(pot ~ psi_time_estimate, data = data_psi[c(i - 1, i),])
    a <- pneumatron %>% 
      #filter within pressure pump measured time
      filter(psi_time_estimate >= data_psi$psi_time_estimate[i - 1],
             psi_time_estimate < data_psi$psi_time_estimate[i]) %>% 
      select(psi_time_estimate)
    #predict psi in function of time
    psi_predict <- c(psi_predict, predict(model, a))
  }
  
  #same prection, but for points after the last pressure pump measurement
  a <- pneumatron %>% 
    filter(psi_time_estimate >= data_psi$psi_time_estimate[i]) %>% 
    select(psi_time_estimate)
  psi_predict <- c(psi_predict, predict(model, a))
  #add psi column 
  pneumatron$psi <- as.numeric(psi_predict)
  #return column names to original form
  pneumatron <- pneumatron %>%
    rename(!!pneumatron_time_column := psi_time_estimate)
  
  return(pneumatron)
}

open_data_psi <- function(file_path) {
  df <- data.table::fread(file_path, fill = TRUE)
  df$time <- lubridate::dmy_hm(df$time)
  df <- dplyr::filter(df, !is.na(id))
  return(df)
}
