

## Plots mit mgcViz

### Für das Date_Model

# Datum

date_model_date <- 
  plot(sm(date_Viz, select = 1), trans = plogis) +
  #l_points(shape = 19, size = 1, alpha = 0.4) +   # Residualpunkte
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8,
        length = unit(0.02, "npc")) + # Verdichtung an Achsen
  labs(title = "Datum",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(17910,17940,17970,18000), 
                     labels = c("14-01","13-02",
                                "15-03","14-04"))


# Lawinengefahr

date_model_avalanche <- 
  plot(sm(date_Viz, select = 2), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8,
        length = unit(0.02, "npc")) + # Verdichtung an Achsen
  labs(title = "Lawinenwarnstufe",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(1,  2,  3,  4))

# Wochentag

date_model_day <- 
  plot(sm(date_Viz, select = 3), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4, color = "blue") + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) + # Verdichtung an Achsen
  labs(title = "Wochentag",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),
                   labels=c("1" = "Mo", "2" = "Di",
                            "3" = "Mi", "4" = "Do",
                            "5" = "Fr", "6" = "Sa",
                            "7" = "So"))

# Residuen für Temperatur

date_model_temperature <- 
  plot(sm(date_Viz, select = 4), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) + # Verdichtung an Achsen
  labs(title = "Temperatur",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6))



# Residuen Sonneneinstrahlung

date_model_solar_radiation <- 
  plot(sm(date_Viz, select = 5), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4, color = "blue") + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) + # Verdichtung an Achsen
  labs(title = "Sonneneinstrahlung",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-4, -3, -2, -1, 0, 1, 2))

# Residuen Schneehöhe

date_model_snowhight <- 
  plot(sm(date_Viz, select = 6), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  #l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) + # Verdichtung an Achsen
  labs(title = "Schneehöhe",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-18, -15, -12, -9, -6, -3, 
                                0, 3, 6, 9, 12, 15, 18))


# Grid für Date Model Plots

date_model_grid <- 
  gridPrint(date_model_date,
            date_model_day,
            date_model_avalanche,
            date_model_solar_radiation,
            date_model_temperature,
            date_model_snowhight,
            nrow = 2,
            top = "Smooth-Funktionen im Date Model")


