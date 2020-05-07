
### Plots das Date_Model als funktion
### Nimmt als Argument ein date_model_function(date_data)-Objekt

plots_date_model <- function(
  date_model
) {

date_Viz <- date_model$Viz

theme_set(theme_minimal())

## Plots für den Grid

# Datum

date_model_date <- 
  plot(sm(date_Viz, select = 1), trans = plogis) +
  #l_points(shape = 19, size = 1, alpha = 0.4) +   # Residualpunkte
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8,
        length = unit(0.02, "npc")) # Verdichtung an Achsen

  

date_grid <- date_model_date +
                labs(title = "Datum",
                     x = "", y = "") +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                                   labels = c("01. Jan","01. Feb",
                                              "01. Mär","01. Apr"))

date_smooth <- date_model_date +
                  labs(title = "Smooth-Funktion für Datum",
                       x = "Datum",
                       y = "s(Datum)") +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                                     labels = c("01. Jan","01. Feb",
                                                "01. Mär","01. Apr"))
  


# Lawinengefahr

date_model_avalanche <- 
  plot(sm(date_Viz, select = 2), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8,
        length = unit(0.02, "npc")) # Verdichtung an Achsen

avalanche_grid <- date_model_avalanche +
                    labs(title = "Lawinenwarnstufe",
                         x = "", y = "") +
                    theme(plot.title = element_text(hjust = 0.5)) +
                    scale_x_continuous(breaks = c(1,  2,  3,  4))

avalanche_smooth <- date_model_avalanche +
                      labs(title = "Smooth-Funktion für Lawinenwarnstufe",
                           x = "Lawinenwarnstufe",
                           y = "s(Lawinenwarnstufe)") +
                      theme(plot.title = element_text(hjust = 0.5)) +
                      scale_x_continuous(breaks = c(1,  2,  3,  4))
  


# Wochentag

date_model_day <- 
  plot(sm(date_Viz, select = 3), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4, color = "blue") + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) # Verdichtung an Achsen

day_grid <- date_model_day +
              labs(title = "Wochentag",
                   x = "", y = "") +
              theme(plot.title = element_text(hjust = 0.5)) +
              scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),
                               labels=c("1" = "Mo", "2" = "Di",
                                        "3" = "Mi", "4" = "Do",
                                        "5" = "Fr", "6" = "Sa",
                                        "7" = "So"))

day_smooth <- date_model_day +
                labs(title = "Smooth-Funktion für Wochentag",
                     x = "Wochentag",
                     y = "s(Wochentag)") +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),
                                 labels=c("1" = "Montag", "2" = "Dienstag",
                                          "3" = "Mittwoch", "4" = "Donnerstag",
                                          "5" = "Freitag", "6" = "Samstag",
                                          "7" = "Sonntag"))


# Residuen für Temperatur

date_model_temperature <- 
  plot(sm(date_Viz, select = 4), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) # Verdichtung an Achsen

temperature_grid <- date_model_temperature +
                      labs(title = "Temperatur",
                           x = "",
                           y = "") +
                      theme(plot.title = element_text(hjust = 0.5)) +
                      scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6))

temperature_smooth <- date_model_temperature +
                labs(title = "Smooth-Funktion für Residuen \n der Temperatur",
                     x = "Temperatur in Grad Celsius",
                     y = "s(Temperatur_Residuen)") +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6))


# Residuen Sonneneinstrahlung

date_model_solar_radiation <- 
  plot(sm(date_Viz, select = 5), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4, color = "blue") + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) # Verdichtung an Achsen

solar_radiation_grid <- date_model_solar_radiation +
                        labs(title = "Sonneneinstrahlung",
                             x = "",
                             y = "") +
                        theme(plot.title = element_text(hjust = 0.5)) +
                        scale_x_continuous(breaks = c(-4, -3, -2, -1, 0, 1, 2))

solar_radiation_smooth <- date_model_solar_radiation +
  labs(title = "Smooth-Funktion für Residuen \n der Sonneneinstrahlung",
       x = "Sonneneinstrahlung in W/m²",
       y = "s(Sonneneinstrahlung)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-4, -3, -2, -1, 0, 1, 2))

# Residuen Schneehöhe

date_model_snowhight <- 
  plot(sm(date_Viz, select = 6), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) # Verdichtung an Achsen

snowhight_grid <- date_model_snowhight +
                    labs(title = "Schneehöhe",
                         x = "",
                         y = "") +
                    theme(plot.title = element_text(hjust = 0.5)) +
                    scale_x_continuous(breaks = c(-18, -15, -12, -9, -6, -3, 
                                                  0, 3, 6, 9, 12, 15, 18))

snowhight_smooth <- date_model_snowhight +
                labs(title = "Smooth-Funktion für Residuen \n der Schneehöhe",
                     x = "Schneehöhe in cm",
                     y = "s(Schneehöhe_Residuen)") +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = c(-18, -15, -12, -9, -6, -3, 
                                              0, 3, 6, 9, 12, 15, 18))


# Grid für Date Model Plots

date_model_grid <-
  gridPrint(date_grid,
            day_grid,
            avalanche_grid,
            solar_radiation_grid,
            temperature_grid,
            snowhight_grid,
            nrow = 2,
            top = "Smooth-Funktionen im Date Model")

plots_date_model_list <- list(
  date = date_smooth,
  day = day_smooth,
  avalanche = avalanche_smooth,
  solar_radiation = solar_radiation_smooth,
  temperature = temperature_smooth,
  snowhight = snowhight_smooth
  )

return(plots_date_model_list)

}

