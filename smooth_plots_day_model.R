
### Plots das Day_Model als Funktion
### Nimmt als Argument ein day_model_function(data)-Objekt

plots_day_model <- function(
  day_model
) {
  
  day_Viz <- day_model$Viz
  
  
  ## Smooth-Plots für nichtparametrische Kovariablen
  ## aufgeteilt in Darstellung für den Grid und einzeln
  
  
  #2-Dimensionale Smooth-Funktion; Date and Time
  
  day_model_date_time <- 
    plot(sm(day_Viz, select = 1), trans = plogis)  + labs(y="Datum", x="Uhrzeit") + 
    l_fitRaster() + l_rug() +
    scale_y_continuous(breaks = c(17897,17928,17956,17987), 
                       labels = c("01. Jan","01. Feb",
                                  "01. Mär","01. Apr")) +
    scale_x_continuous(breaks=c(-2209060800,-2209050000,-2209039200,-2209028400, 
                                -2209017600, -2209006800,
                                -2208996000, -2208985200, -2208974460), 
                       labels=c("04:00","07:00","10:00","13:00", "16:00", "19:00",
                                "22:00", "01:00", "03:59")) +
    ggtitle("Smoothfunktion für Uhrzeit und Datum")
  
  
  # Lawinengefahr
  
  day_model_avalanche <- 
    plot(sm(day_Viz, select = 3), trans = plogis) +
    l_ciPoly() + # Konfidenzband
    l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
    l_fitLine(color = "black", size = 1.2) + # Fitline
    l_rug(mapping = aes(x=x, y=y), alpha = 0.2,
          length = unit(0.02, "npc")) # Verdichtung an Achsen
  
  avalanche_grid <- day_model_avalanche +
    labs(title = "Lawinenwarnstufe",
         x = "", y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1,  2,  3,  4))
  
  avalanche_smooth <- day_model_avalanche +
    labs(title = "Smooth-Funktion für Lawinenwarnstufe",
         x = "Lawinenwarnstufe",
         y = "s(Lawinenwarnstufe)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1,  2,  3,  4))
  
  
  
  # Wochentag
  
  day_model_day <- 
    plot(sm(day_Viz, select = 2), trans = plogis) +
    l_ciPoly() + # Konfidenzband
    l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
    l_fitLine(color = "black", size = 1.2) + # Fitline
    l_rug(mapping = aes(x=x, y=y), alpha = 0.2) # Verdichtung an Achsen
  
  day_grid <- day_model_day +
    labs(title = "Wochentag",
         x = "", y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),
                     labels=c("1" = "Mo", "2" = "Di",
                              "3" = "Mi", "4" = "Do",
                              "5" = "Fr", "6" = "Sa",
                              "7" = "So"))
  
  day_smooth <- day_model_day +
    labs(title = "Smooth-Funktion für Wochentag",
         x = "Wochentag",
         y = "s(Wochentag)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),
                     labels=c("1" = "Montag", "2" = "Dienstag",
                              "3" = "Mittwoch", "4" = "Donnerstag",
                              "5" = "Freitag", "6" = "Samstag",
                              "7" = "Sonntag"))
  
  
  # Residuen für Temperatur (im Day Model auskommentiert)
  
  # day_model_temperature <- 
  #   plot(sm(day_Viz, select = 4), trans = plogis) +
  #   l_ciPoly() + # Konfidenzband
  #   l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #   l_fitLine(color = "black", size = 1.2) + # Fitline
  #   l_rug(mapping = aes(x=x, y=y), alpha = 0.2) # Verdichtung an Achsen
  # 
  # temperature_grid <- day_model_temperature +
  #   labs(title = "Temperatur",
  #        x = "",
  #        y = "") +
  #   theme(plot.title = element_text(hjust = 0.5)) +
  #   scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6, 8))
  # 
  # temperature_smooth <- day_model_temperature +
  #   labs(title = "Smooth-Funktion für Temperatur",
  #        x = "Temperatur in Grad Celsius",
  #        y = "s(Temperatur)") +
  #   theme(plot.title = element_text(hjust = 0.5)) +
  #   scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6, 8))
  
  
  # Anteil Sonneneinstrahlung
  
  day_model_solar_radiation <- 
    plot(sm(day_Viz, select = 4), trans = plogis) +
    l_ciPoly() + # Konfidenzband
    l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
    l_fitLine(color = "black", size = 1.2) + # Fitline
    l_rug(mapping = aes(x=x, y=y), alpha = 0.2) # Verdichtung an Achsen
  
  solar_radiation_grid <- day_model_solar_radiation +
    labs(title = "Sonneneinstrahlung",
         x = "",
         y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))
  
  solar_radiation_smooth <- day_model_solar_radiation +
    labs(title = "Smooth-Funktion für Anteil \n der 
                  Sonneneinstrahlung am Maximum",
         x = "Anteil Sonneneinstrahlung",
         y = "s(Anteil Sonneneinstrahlung)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))
  
  # Neuschnee
  
  day_model_snowhight <- 
    plot(sm(day_Viz, select = 5), trans = plogis) +
    l_ciPoly() + # Konfidenzband
    l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
    l_fitLine(color = "black", size = 1.2) + # Fitline
    l_rug(mapping = aes(x=x, y=y), alpha = 0.2) # Verdichtung an Achsen
  
  snowhight_grid <- day_model_snowhight +
    labs(title = "Neuschnee",
         x = "",
         y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(-12, -4, 4, 12, 20, 28, 36))
  
  snowhight_smooth <- day_model_snowhight +
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
         # temperature_grid, 
         snowhight_grid)
  
  # Liste zur Ausgabe
  
  plots_day_model_list <- list(
    date_time = day_model_date_time,
    day = day_smooth,
    avalanche = avalanche_smooth,
    solar_radiation = solar_radiation_smooth,
    # temperature = temperature_smooth,
    snowhight = snowhight_smooth,
    grid = day_model_grid
  )
  
  return(plots_day_model_list)
  
}
