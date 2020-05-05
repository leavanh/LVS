### Für das Day_Model

day_Viz <- day_model$Viz

theme_set(theme_minimal())

# Plots für den Grid

# Lawinengefahr

day_model_avalanche <- 
  plot(sm(day_Viz, select = 3), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.2) + # Verdichtung an Achsen
  labs(title = "Lawinenwarnstufe",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5))


# Wochentag

day_model_day <- 
  plot(sm(day_Viz, select = 2), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.2) + # Verdichtung an Achsen
  labs(title = "Wochentag",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),
                   labels=c("1" = "Mo", "2" = "Di",
                            "3" = "Mi", "4" = "Do",
                            "5" = "Fr", "6" = "Sa",
                            "7" = "So"))


# Residuen für Temperatur

day_model_temperature <- 
  plot(sm(day_Viz, select = 4), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.2) + # Verdichtung an Achsen
  labs(title = "Temperatur",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6))


# Residuen Sonneneinstrahlung

day_model_solar_radiation <- 
  plot(sm(day_Viz, select = 5), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.2) + # Verdichtung an Achsen
  labs(title = "Sonneneinstrahlung",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5))

# Residuen Schneehöhe

day_model_snowhight <- 
  plot(sm(day_Viz, select = 6), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.2) + # Verdichtung an Achsen
  labs(title = "Schneehoehe",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-18, -15, -12, -9, -6, -3, 0, 
                                3, 6, 9, 12, 15, 18))

# Grid für Day Model Plots

day_model_grid <- 
  gridPrint(day_model_day,
            day_model_avalanche,
            day_model_solar_radiation,
            day_model_temperature,
            day_model_snowhight,
            nrow = 2,
            top = "Smooth-Funktionen im Day Model")






#2-Dimensionale Smooth-Funktion; Date and Time

day_model_date_time <- 
  plot(sm(day_Viz, select = 1), trans = plogis)  + labs(y="Datum", x="Uhrzeit") + 
  l_fitRaster() + l_rug() +
  scale_y_continuous(breaks=c(17910,17940,17970,18000), 
                     labels=c("14-01-2019","13-02-2019",
                              "15-03-2019","14-04-2019")) +
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
  #l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.2) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für Lawinenwarnstufe",
       x = "Lawinenwarnstufe",
       y = "s(Lawinenwarnstufe)") +
  theme(plot.title = element_text(hjust = 0.5))


# Wochentag

day_model_day <- 
  plot(sm(day_Viz, select = 2), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.2) + # Verdichtung an Achsen
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

day_model_temperature <- 
  plot(sm(day_Viz, select = 4), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.2) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für Residuen \n der Temperatur",
       x = "Temperatur in Grad Celsius",
       y = "s(Temperatur_Residuen)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6))


# Residuen Sonneneinstrahlung

day_model_solar_radiation <- 
  plot(sm(day_Viz, select = 5), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.2) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für Residuen der \n Sonneneinstrahlung",
       x = "Sonneneinstrahlung in W/m²",
       y = "s(Sonneneinstrahlung_Residuen)") +
  theme(plot.title = element_text(hjust = 0.5))

# Residuen Schneehöhe

day_model_snowhight <- 
  plot(sm(day_Viz, select = 6), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.2) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für Residuen \n der Schneehoehe",
       x = "Schneehöhe in cm",
       y = "s(Schneehöhe_Residuen)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-18, -15, -12, -9, -6, -3, 0, 
                                3, 6, 9, 12, 15, 18))