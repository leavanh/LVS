
### Plottet das Zeitmodell (als Funktion)
### Nimmt als Argument ein time_model_function(data)-Objekt

plots_time_model <- function(
  time_model
) {
  
  day_Viz <- time_model$Viz
  
  plot_list <- list()
  
  ## Smooth-Plots für nichtparametrische Kovariablen
  
  pdf(file = NULL)
  
  for (i in 1:5) {
    
    # Nützliche Werte zur Vereinfachung in eigenen DataFrame
    
    data <- data.frame(
      x = plot(time_model$model, trans = plogis, se = 1.96,
               seWithMean = TRUE)[[i+1]]$x,
      fit = plot(time_model$model, trans = plogis, se = 1.96,
                 seWithMean = TRUE)[[i+1]]$fit,
      se = plot(time_model$model, trans = plogis, se = 1.96,
                seWithMean = TRUE)[[i+1]]$se, 
      intercept = rep(coef(time_model$model)[1], 100)
    )
    
    # tatsächliche x-Werte der Daten müssen in eigenen DataFrame für den Rug
    
    raw <- data.frame(
      raw = time_model$model$model[[i+4]]
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
      scale_y_continuous(limits = c(0,0.5))
    
    
  }
  
  # Echte x-Werte für den 2D-Plot
  
  raw <- data.frame(
    time = time_model$model$model[[3]],
    date = time_model$model$model[[4]]
  )
  
  
  # 2-Dimensionale Smooth-Funktion; Date and Time
  
  # mgcViz-Plot zum Nachbauen
  
  mgcviz_plot <- 
    plot(sm(day_Viz, select = 1), trans = plogis) +
    l_fitRaster() + l_rug()
  
  dev.off()
  
  # Nachbauen aus mgcViz-Plot-Daten
  
  time_model_date_time <- 
  ggplot(mgcviz_plot$data$fit, aes(x, y)) +
    geom_tile(aes(fill = plogis(z + coef(time_model$model)[1]))) +
    geom_rug(data = raw, aes(x = time, y = date)) +
    scale_y_continuous(breaks = c(17897,17928,17956,17987), 
                       labels = c("01.Jan","01.Feb",
                                  "01.Mär","01.Apr")) +
    scale_x_continuous(breaks = c(-2209060800,-2209050000,-2209039200,-2209028400, 
                                   -2209017600, -2209006800,
                                   -2208996000, -2208985200, -2208974460), 
                       labels = c("04:00","07:00","10:00","13:00", "16:00", "19:00",
                                "22:00", "01:00", "03:59")) +
    ggtitle("Anteil für Uhrzeit und Datum") + 
    labs(y="Datum", x="Uhrzeit", fill = NULL) +
    scale_fill_gradient2(midpoint = 0.5, low = "#FF6666", mid = "white",
                         high = "#33FF66", limits = c(0, 1)) +
    theme(legend.position = "right")
  # Linien die den Sonnenauf und -untergang anzeigen:
    # + geom_line(data=date_data, aes(as.numeric(sunrise), int_date)) +
    # geom_line(data=date_data, aes(as.numeric(sunset), int_date))

    
  
  # Lawinengefahr

  avalanche_grid <- plot_list[[2]] +
    labs(title = "Lawinenwarnstufe",
         x = "", y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1,  2,  3,  4))
  
  avalanche_smooth <- plot_list[[2]] +
    labs(title = "glatte Funktion für Lawinenwarnstufe",
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
    labs(title = "glatte Funktion für Wochentag",
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
    labs(title = "glatte Funktion für Temperatur",
         x = "Temperatur in Grad Celsius",
         y = "s(Temperatur)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6, 8))
  
  
  # Bewölkung
  
  cloud_cover_grid <- plot_list[[4]] +
    labs(title = "Bewölkung",
         x = "",
         y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100))
  
  cloud_cover_smooth <- plot_list[[4]] +
    labs(title = "glatte Funktion für Bewölkung",
         x = "Bewölkung",
         y = "s(Bewölkung)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100))
  
  # Schneedifferenz
  
  snow_diff_grid <- plot_list[[5]] +
    labs(title = "Schneedifferenz",
         x = "",
         y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40))
  
  snow_diff_smooth <- plot_list[[5]] +
    labs(title = "glatte Funktion für Schneedifferenz",
         x = "Schneedifferenz in cm",
         y = "s(Schneedifferenz)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40))
  
  # Grid für Time Model Plots
  
  time_model_grid <-
    list(day_grid,
         avalanche_grid,
         cloud_cover_grid,
         temperature_grid, 
         snow_diff_grid)
  
  # Liste zur Ausgabe
  
  plots_time_model_list <- list(
    day = day_smooth,
    avalanche = avalanche_smooth,
    cloud_cover = cloud_cover_smooth,
    temperature = temperature_smooth,
    snow_diff = snow_diff_smooth,
    date_time = time_model_date_time,
    grid = time_model_grid
  )
  
  return(plots_time_model_list)
  
}
