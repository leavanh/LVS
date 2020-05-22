
### Plots das Date_Model als Funktion
### Nimmt als Argument ein date_model_function(date_data)-Objekt

plots_date_model <- function(
  date_model
) {

date_Viz <- date_model$Viz

## Smooth-Plots für nichtparametrische Kovariablen
## aufgeteilt in Darstellung für den Grid und als Einzelplot

# Datum

date_model_date <- 
  plot(sm(date_Viz, select = 1), trans = plogis) +
  l_ciPoly(level = 0.95) + # Konfidenzintervallband
  l_ciLine(level = 0.95, colour = "grey", linetype = 1) + # KI-Ränder
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x), alpha = 0.8,
        length = unit(0.02, "npc")) + # Verdichtung an Achsen
  scale_y_continuous(limits = c(0,1))


## Struktur der Werte

str(plot(date_model$model, select = 1, trans = plogis,
     shift = coef(date_model$model)[1],
     seWithMean = TRUE, se = 1.96)[[1]])

# Nützliche Werte zur Vereinfachung in eigenen DataFrame

wochentag <- data.frame(
  x = plot(date_model$model, select = 1, trans = plogis,
           shift = coef(date_model$model)[1], se = 1.96,
           seWithMean = TRUE)[[1]]$x,
  fit = plot(date_model$model, select = 1, trans = plogis,
             shift = coef(date_model$model)[1], se = 1.96,
             seWithMean = TRUE)[[1]]$fit,
  se = plot(date_model$model, select = 1, trans = plogis,
            shift = coef(date_model$model)[1], se = 1.96,
            seWithMean = TRUE)[[1]]$se, 
  intercept = coef(date_model$model)[1]
)

# tatsächliche x-Werte der Daten müssen in eigenen DataFrame für den Rug später

raw <- data.frame(
  raw = plot(date_model$model, select = 1, trans = plogis,
             shift = coef(date_model$model)[1], se = 1.96,
             seWithMean = TRUE)[[1]]$raw
)

# nachgebauter Plot

ggplot(data = wochentag, aes(x = x)) +
  # Fitline
  geom_line(aes(y = plogis(fit + intercept))) +
  # Konfidenzintervall
  geom_ribbon(aes(ymin = plogis(fit + intercept - se),
                  ymax = plogis(fit + intercept + se)),
              color = "grey", alpha = 0.2) +
  # Rug
  geom_rug(data = raw, 
           aes(x = raw))

  

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
  l_ciPoly(level = 0.95) + # Konfidenzintervallband
  l_ciLine(level = 0.95, colour = "grey", linetype = 1) + # Konfidenzivränder
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x), alpha = 0.8,
        length = unit(0.02, "npc")) + # Verdichtung an Achsen
  scale_y_continuous(limits = c(0,1))

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
  l_ciPoly(level = 0.95) + # Konfidenzintervallband
  l_ciLine(level = 0.95, colour = "grey", linetype = 1) + # Konfidenzivränder
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x), alpha = 0.8) + # Verdichtung an Achsen
  scale_y_continuous(limits = c(0,1))

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


# Temperatur

date_model_temperature <- 
  plot(sm(date_Viz, select = 4), trans = plogis) +
  l_ciPoly(level = 0.95) + # Konfidenzintervallband
  l_ciLine(level = 0.95, colour = "grey", linetype = 1) + # Konfidenzivränder
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x), alpha = 0.8) + # Verdichtung an Achsen
  scale_y_continuous(limits = c(0,1))

temperature_grid <- date_model_temperature +
                      labs(title = "Temperatur",
                           x = "",
                           y = "") +
                      theme(plot.title = element_text(hjust = 0.5)) +
                      scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6, 8))

temperature_smooth <- date_model_temperature +
                labs(title = "Smooth-Funktion für Temperatur",
                     x = "Temperatur in Grad Celsius",
                     y = "s(Temperatur)") +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6, 8))


# Sonneneinstrahlung-Anteil

date_model_solar_radiation <- 
  plot(sm(date_Viz, select = 5), trans = plogis) +
  l_ciPoly(level = 0.95) + # Konfidenzintervallband
  l_ciLine(level = 0.95, colour = "grey", linetype = 1) + # Konfidenzivränder
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x), alpha = 0.8) + # Verdichtung an Achsen
  scale_y_continuous(limits = c(0,1))

solar_radiation_grid <- date_model_solar_radiation +
                        labs(title = "Sonneneinstrahlung",
                             x = "",
                             y = "") +
                        theme(plot.title = element_text(hjust = 0.5)) +
                        scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))

solar_radiation_smooth <- date_model_solar_radiation +
  labs(title = "Smooth-Funktion für Anteil \n der Sonneneinstrahlung am Maximum",
       x = "Anteil an maximaler Sonneneinstrahlung",
       y = "s(Anteil Sonneneinstrahlung)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))

# Differenz Schneehöhe

date_model_snowhight <- 
  plot(sm(date_Viz, select = 6), trans = plogis) +
  l_ciPoly(level = 0.95) + # Konfidenzintervallband
  l_ciLine(level = 0.95, colour = "grey", linetype = 1) + # Konfidenzivränder
  l_fitLine(color = "black", size = 1.2) + # Fitline
  l_rug(mapping = aes(x=x), alpha = 0.8) + # Verdichtung an Achsen
  scale_y_continuous(limits = c(0,1))

snowhight_grid <- date_model_snowhight +
                    labs(title = "Neuschnee",
                         x = "",
                         y = "") +
                    theme(plot.title = element_text(hjust = 0.5)) +
                    scale_x_continuous(breaks = c(-12, -4, 4, 12, 20, 28, 36))

snowhight_smooth <- date_model_snowhight +
                labs(title = "Smooth-Funktion für Neuschnee",
                     x = "Neuschnee in cm",
                     y = "s(Neuschnee)") +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = c(-12, -8, -4, 0, 4, 8, 12, 16, 20,
                                              24, 28, 32, 36, 40))


# Grid für Date Model Plots

date_model_grid <-
  list(date_grid,
            day_grid,
            avalanche_grid,
            solar_radiation_grid,
            temperature_grid,
            snowhight_grid)



# Liste zur Ausgabe

plots_date_model_list <- list(
  date = date_smooth,
  day = day_smooth,
  avalanche = avalanche_smooth,
  solar_radiation = solar_radiation_smooth,
  temperature = temperature_smooth,
  snowhight = snowhight_smooth,
  grid = date_model_grid
  )

return(plots_date_model_list)

}


