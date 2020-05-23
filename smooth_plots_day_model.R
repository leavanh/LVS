
### Plots das Day_Model als Funktion
### Nimmt als Argument ein day_model_function(data)-Objekt

plots_day_model <- function(
  day_model
) {
  
  day_Viz <- day_model$Viz
  
  plot_list <- list()
  
  ## Smooth-Plots für nichtparametrische Kovariablen
  
  pdf(file = NULL)
  
  for (i in 1:5) {
    
    # Nützliche Werte zur Vereinfachung in eigenen DataFrame
    
    data <- data.frame(
      x = plot(day_model$model, trans = plogis, se = 1.96,
               seWithMean = TRUE)[[i+1]]$x,
      fit = plot(day_model$model, trans = plogis, se = 1.96,
                 seWithMean = TRUE)[[i+1]]$fit,
      se = plot(day_model$model, trans = plogis, se = 1.96,
                seWithMean = TRUE)[[i+1]]$se, 
      intercept = rep(coef(day_model$model)[1], 100)
    )
    
    # tatsächliche x-Werte der Daten müssen in eigenen DataFrame für den Rug
    
    raw <- data.frame(
      raw = plot(day_model$model, trans = plogis, se = 1.96,
                 seWithMean = TRUE)[[i+1]]$raw
    )
    
    # nachgebauter Plot
    
    plot_list[[i]] <- 
      ggplot(data = data, aes(x = x)) +
      # Fitline
      geom_line(aes(y = plogis(fit + intercept))) +
      # Konfidenzintervall
      geom_ribbon(aes(ymin = plogis(fit + intercept - se),
                      ymax = plogis(fit + intercept + se)),
                  color = "grey", alpha = 0.2) +
      # Rug
      geom_rug(data = raw, aes(x = raw), alpha = 0.2) +
      scale_y_continuous(limits = c(0,1))
    
    
  }
  
  # Echte x-Werte für den 2D-Plot
  
  raw <- data.frame(
    time = plot(day_model$model, select = 1, trans = plogis,
                shift = coef(day_model$model)[1], se = 1.96,
                seWithMean = TRUE)[[1]]$raw$x,
    date = plot(day_model$model, select = 1, trans = plogis,
                shift = coef(day_model$model)[1], se = 1.96,
                seWithMean = TRUE)[[1]]$raw$y
  )
  
  
  # 2-Dimensionale Smooth-Funktion; Date and Time
  
  # mgcViz-Plot zum Nachbauen
  
  mgcviz_plot <- 
    plot(sm(day_Viz, select = 1), trans = plogis) +
    l_fitRaster() + l_rug()
  
  dev.off()
  
  # Nachbauen aus mgcViz-Plot-Daten
  
  day_model_date_time <- 
  ggplot(mgcviz_plot$data$fit, aes(x, y)) +
    geom_tile(aes(fill = plogis(z + coef(day_model$model)[1]))) +
    geom_rug(data = raw, aes(x = time, y = date)) +
    scale_y_continuous(breaks = c(17897,17928,17956,17987), 
                       labels = c("01. Jan","01. Feb",
                                  "01. Mär","01. Apr")) +
    scale_x_continuous(breaks=c(-2209060800,-2209050000,-2209039200,-2209028400, 
                                -2209017600, -2209006800,
                                -2208996000, -2208985200, -2208974460), 
                       labels=c("04:00","07:00","10:00","13:00", "16:00", "19:00",
                                "22:00", "01:00", "03:59")) +
    ggtitle("Anteil für Uhrzeit und Datum") + 
    labs(y="Datum", x="Uhrzeit", fill = NULL) +
    scale_fill_gradient2(midpoint = 0.5, low = "blue", mid = "white",
                         high = "green", limits = c(0, 1))

    
  
  # Lawinengefahr

  avalanche_grid <- plot_list[[2]] +
    labs(title = "Lawinenwarnstufe",
         x = "", y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1,  2,  3,  4))
  
  avalanche_smooth <- plot_list[[2]] +
    labs(title = "Smooth-Funktion für Lawinenwarnstufe",
         x = "Lawinenwarnstufe",
         y = "s(Lawinenwarnstufe)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1,  2,  3,  4))
  
  
  
  # Wochentag
  
  day_grid <- plot_list[[1]] +
    labs(title = "Wochentag",
         x = "", y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),
                     labels=c("1" = "Mo", "2" = "Di",
                              "3" = "Mi", "4" = "Do",
                              "5" = "Fr", "6" = "Sa",
                              "7" = "So"))
  
  day_smooth <- plot_list[[1]] +
    labs(title = "Smooth-Funktion für Wochentag",
         x = "Wochentag",
         y = "s(Wochentag)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),
                     labels=c("1" = "Montag", "2" = "Dienstag",
                              "3" = "Mittwoch", "4" = "Donnerstag",
                              "5" = "Freitag", "6" = "Samstag",
                              "7" = "Sonntag"))
  
  
  # Temperatur

  temperature_grid <- plot_list[[3]] +
    labs(title = "Temperatur",
         x = "",
         y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6, 8))

  temperature_smooth <- plot_list[[3]] +
    labs(title = "Smooth-Funktion für Temperatur",
         x = "Temperatur in Grad Celsius",
         y = "s(Temperatur)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6, 8))
  
  
  # Anteil Sonneneinstrahlung
  
  solar_radiation_grid <- plot_list[[4]] +
    labs(title = "Sonneneinstrahlung",
         x = "",
         y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))
  
  solar_radiation_smooth <- plot_list[[4]] +
    labs(title = "Smooth-Funktion für Anteil \n der 
                  Sonneneinstrahlung am Maximum",
         x = "Anteil Sonneneinstrahlung",
         y = "s(Anteil Sonneneinstrahlung)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))
  
  # Neuschnee
  
  snowhight_grid <- plot_list[[5]] +
    labs(title = "Neuschnee",
         x = "",
         y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(-12, -4, 4, 12, 20, 28, 36))
  
  snowhight_smooth <- plot_list[[5]] +
    labs(title = "Smooth-Funktion für Neuschnee",
         x = "Neuschnee in cm",
         y = "s(Neuschnee)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(-12, -8, -4, 0, 4, 8, 12, 16, 20,
                                  24, 28, 32, 36, 40))
  
  
  # Grid für Day Model Plots
  
  day_model_grid <-
    list(day_grid,
         avalanche_grid,
         solar_radiation_grid,
         temperature_grid, 
         snowhight_grid)
  
  # Liste zur Ausgabe
  
  plots_day_model_list <- list(
    date_time = day_model_date_time,
    day = day_smooth,
    avalanche = avalanche_smooth,
    solar_radiation = solar_radiation_smooth,
    temperature = temperature_smooth,
    snowhight = snowhight_smooth,
    grid = day_model_grid
  )
  
  return(plots_day_model_list)
  
}
