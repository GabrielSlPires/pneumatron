# criar dentro de pneumatron a pasta "all_curves" e suas subpastas
dir.create("all_curves", showWarnings = FALSE)
dir.create("all_curves/curves", showWarnings = FALSE)
dir.create("all_curves/parameters", showWarnings = FALSE)
dir.create("all_curves/plots", showWarnings = FALSE)
dir.create("all_curves/plots/ad_ul", showWarnings = FALSE)
dir.create("all_curves/plots/pad", showWarnings = FALSE)

# logical para filtar ou não jump inicial
library(data.table)
library(dplyr)
library(ggplot2)
library(purrr)
# library(furrr) # para fazer walk em paralelo
library(lubridate)

source("shiny/R/calculate_air_discharge.R")
source("shiny/helper.R")

# antes de iniciar o script, avaliar se precisa deletar "all_curves/parameters/parameters.csv"

# arquivo com inicio e fim de todas as curvas
exp_data <- fread("all_curves/experiments-example.csv")
# arquivo com "filtro" dos valoes de maximo e minimo de ad_ul
ad_limits <- data.table::fread("all_curves/fix_ad_limits.csv")
# future_walk
walk(exp_data$id, \(x) {
  try({
    message(paste("start", x, "---------------------------------------------------"))
    # abrir a primira linhas dos experimentos
    my_line <- exp_data[id == x,]
    # abrir arquivo do pneumatron e fazer o filtro de data
    #---
    pneumatron <- open_pneumatron_db(my_line$dado_pneumatron)
    pneumatron <- pneumatron %>% 
      filter(id == my_line$id_pneumatron,
             between(datetime,
                     ymd_hm(my_line$inicio_exp),
                     ymd_hm(my_line$final_exp)))
    #---
    # realizar filtros de jump inicial e plato final
    #---
    try({
      if (is.na(my_line$cut_begin)) stop("Sem jump inicial")
      pneumatron <- filter(pneumatron, datetime >= ymd_hms(my_line$cut_begin))
    }, silent = TRUE)
    try({
      if (is.na(my_line$cut_end)) stop("Sem cut final")
      pneumatron <- filter(pneumatron, datetime >= ymd_hms(my_line$cut_end))
    }, silent = TRUE)
    #---
    # abrir arquivo de potencial e fazer filtro de data
    #---
    water <- open_data_psi(my_line$dados_potencial)
    water <- water %>% 
      filter(id == my_line$id_pneumatron,
             between(time,
                     ymd_hm(my_line$inicio_exp),
                     ymd_hm(my_line$final_exp)))
    #---
    # calcular ad
    p_ad <- calculate_air_discharge(pneumatron)
    # realizar filtros de AD
    #---
    try({
      limit <- ad_limits[id == x,]$min_ad_ul
      if (is.na(limit)) stop("Sem limite minimo")
      p_ad <- filter(p_ad, ad_ul >= limit)
    }, silent = TRUE)
    try({
      limit <- ad_limits[id == x,]$max_ad_ul
      if (is.na(limit)) stop("Sem limite maximo")
      p_ad <- filter(p_ad, ad_ul <= limit)
    }, silent = TRUE)
    #---
    # calular PAD, extrapolar potencial e salvar dados de uma curva
    #---
    p_ad <- mutate(p_ad, pad = ((ad_ul - min(ad_ul, na.rm = TRUE))/(max(ad_ul, na.rm = TRUE) - min(ad_ul, na.rm = TRUE)))*100)
    p_ad <- extrapolated_wp(p_ad, water)
    p_ad <- rename(p_ad, id_pneumatron = id)
    p_ad <- data.frame(p_ad, id = x)
    # aqui temos um arquivo para cada curva
    fwrite((p_ad), paste0("all_curves/curves/curve_", x, ".csv"))
    message("ad calculado - salvo")
    #---
    # plotar dados
    #---
    p <- ggplot(p_ad,
                aes(x = psi)) +
      theme_bw() +
      xlab(expression(paste(psi, " (MPa)")))
    try({
      p_save <- p +
        geom_point(aes(y = ad_ul)) +
        ylab(expression(paste("Air Discharge (", mu, "l)")))
      ggsave(paste0("all_curves/plots/ad_ul/curve_", x,".png"), plot = p_save)
      message("plot ad_ul vs wp - salvo")
    }, silent = TRUE)
    #---
    # criar arquivo de resumo - sem parametros das curvas
    curve_summary <- data.frame(id = x,
                                a = NA,
                                p12 = NA,
                                p50 = NA,
                                p88 = NA,
                                species = my_line$species) # se quiser colocar outra variavel e aqui
    # esse arquivo com os parametros e uma variavel categorica (species) pode ser
    # usado para calcular medias
    
    # calcular parametros
    try({
      curve <- pneumatron_p50(select(p_ad, pad, psi))
      
      #salvar plot com parametros
      try({
        p_save <- p +
          geom_point(aes(y = pad)) +
          ylab("Air Discharge (%)") + 
          stat_function(fun = function(x) 100/(1 + exp(curve["a"]*(x - curve["p50"]))),
                        color = "royalblue",
                        linewidth = 1)
        ggsave(paste0("all_curves/plots/pad/curve_", x,".png"), plot = p_save)
        message("plot pad vs wp - salvo")
      })
      
      # adicionar parametros das curvas
      #---
      curve_summary$a <- curve["a"]
      curve_summary$p12 <- curve["p12"]
      curve_summary$p50 <- curve["p50"]
      curve_summary$p88 <- curve["p88"]
      #---
    })
    
    # salvar arquivo de resumos
    fwrite(curve_summary, paste0("all_curves/parameters/parameters.csv"), append = TRUE)
    # limpar memoria
    #---
    rm(my_line)
    rm(curve_summary)
    rm(curve)
    #---
  })
}, .progress = TRUE)


# da para vc abrir com um loop em files.path (eu acho) todos os arquivos "curve_x.csv"
# depois vc da um merge com o arquivo de parameters
# ai voce pode "colorir/agrupar" os pontos da curva pela sua variavel categorica (species)