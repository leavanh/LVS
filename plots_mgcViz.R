
## Plots mit mgcViz



### für das Date_Model

date_Viz <- getViz(date_model)

# Datum

plot(sm(date_Viz, select = 1), trans = plogis) +
  l_points(shape = 19, size = 1, alpha = 0.4) +   # Residualpunkte
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  l_fitLine(color = "red") + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8,
        length = unit(0.02, "npc")) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für Datum",
       x = "Datum",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c(17897, 17928, 17956, 17987), 
                   labels = c("17897" = "1. Januar",
                              "17928" = "1. Februar",
                              "17956" = "1. März",
                              "17987" = "1. April"))


# Lawinengefahr

plot(sm(date_Viz, select = 2), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "red") + # Fitline
  l_rug(mapping = aes(x=x), alpha = 0.8) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für Lawinengefahr",
       x = "Stufe",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4))

# Wochentag

plot(sm(date_Viz, select = 3), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  l_points(shape = 19, size = 1, alpha = 0.4, color = "blue") + # Residualpunkte
  l_fitLine(color = "red") + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für Wochentag",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),
                   labels=c("1" = "Montag", "2" = "Dienstag",
                            "3" = "Mittwoch", "4" = "Donnerstag",
                            "5" = "Freitag", "6" = "Samstag",
                            "7" = "Sonntag"))

# Residuen für Temperatur

plot(sm(date_Viz, select = 4), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "red") + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für den Abstand der Temperatur
                vom Mittelwert",
       x = "Abweichung vom Mittelwert in Grad Celsius",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c(-6, -4, -2, 0, 2, 4, 6))


# Residuen Sonneneinstrahlung

plot(sm(date_Viz, select = 5), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  l_points(shape = 19, size = 1, alpha = 0.4, color = "blue") + # Residualpunkte
  l_fitLine(color = "red") + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für den Abstand der Sonneneinstrahlung
                vom Mittelwert",
       x = "Abweichung vom Mittelwert in W/m²",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5))

# Residuen Schneehöhe

plot(sm(date_Viz, select = 6), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "red") + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für den Abstand der Schneehöhe
                vom Mittelwert",
       x = "Abweichung vom Mittelwert in Grad Celsius",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = seq(-16, 14, 2))



?seq
  


plot(sm(date_Viz, select = 1), trans = plogis,
     pages = 1, residuals = TRUE, pch = 19, cex = .3, scale = 0, 
     shade = TRUE, seWithMean = TRUE, shift = coef(date_model)[1],) +
  l_ciPoly(mul = 5) + # Konfidenzband
  l_ciLine(mul = 5, colour = "grey", linetype = 1) + # Konfidenzivränder
  l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "red") + # Fitline
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) + # Verdichtung an Achsen
  xlab("Neuschnee") +
  ylab("Smooth")






### für das Day_Model
                     
day_Viz <- getViz(day_model)

# Lawinengefahr

day_plot_avl <- plot(sm(day_Viz, select = 3), trans = plogis) +
                  l_ciPoly() + # Konfidenzband
                  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
                  l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
                  l_fitLine(color = "red") + # Fitline
                  l_rug(mapping = aes(x=x), alpha = 0.6) + # Verdichtung an Achsen
                  labs(title = "Smooth-Funktion für Lawinengefahr (im day_model)",
                       x = "Stufe",
                       y = "") +
                  theme(plot.title = element_text(hjust = 0.5))
                  

# Wochentag

day_plot_intday <- plot(sm(day_Viz, select = 2), trans = plogis) +
                    l_ciPoly() + # Konfidenzband
                    l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
                    l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
                    l_fitLine(color = "red") + # Fitline
                    l_rug(mapping = aes(x=x), alpha = 0.6) + # Verdichtung an Achsen
                    labs(title = "Smooth-Funktion für Wochentag (im day_model)",
                         x = "",
                         y = "") +
                    theme(plot.title = element_text(hjust = 0.5)) +
                    scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),
                                     labels=c("1" = "Montag", "2" = "Dienstag",
                                              "3" = "Mittwoch", "4" = "Donnerstag",
                                              "5" = "Freitag", "6" = "Samstag",
                                              "7" = "Sonntag"))

# Residuen für Temperatur

plot(sm(day_Viz, select = 4), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
  l_fitLine(color = "red") + # Fitline
  l_rug(mapping = aes(x=x), alpha = 0.6) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für den Abstand der Temperatur
                vom Mittelwert (im day_model)",
       x = "Abweichung vom Mittelwert in Grad Celsius",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c(-6, -4, -2, 0, 2, 4, 6))


# Residuen Sonneneinstrahlung

plot(sm(day_Viz, select = 5), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  l_points(shape = 19, size = 1, alpha = 0.2) + # Residualpunkte
  l_fitLine(color = "red") + # Fitline
  l_rug(mapping = aes(x=x), alpha = 0.6) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für den Abstand der Sonneneinstrahlung
                vom Mittelwert (im day_model)",
       x = "Abweichung vom Mittelwert in W/m²",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5))

# Residuen Schneehöhe

plot(sm(day_Viz, select = 6), trans = plogis) +
  l_ciPoly() + # Konfidenzband
  l_ciLine(colour = "grey", linetype = 1) + # Konfidenzivränder
  l_points(shape = 19, size = 1, alpha = 0.4) + # Residualpunkte
  l_fitLine(color = "red") + # Fitline
  l_rug(mapping = aes(x=x), alpha = 0.8) + # Verdichtung an Achsen
  labs(title = "Smooth-Funktion für den Abstand der Schneehöhe
                vom Mittelwert (im day_model)",
       x = "Abweichung vom Mittelwert in Grad Celsius",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = seq(-16, 14, 2))













plot(sm(date_Viz, select = 1), trans = plogis,
     pages = 1, residuals = TRUE, pch = 19, cex = .3, scale = 0, 
     shade = TRUE, seWithMean = TRUE, shift = coef(date_model)[1],)
