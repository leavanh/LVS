
### Plottet das Tagesmodell (als Funktion)
### Nimmt als Argument ein day_model_function(date_data)-Objekt

plots_day_model <- function(
  day_model
) {

plot_list <- list()

## Smooth-Plots für nichtparametrische Kovariablen

pdf(file = NULL)

for (i in 1:6) {
  
  # Nützliche Werte zur Vereinfachung in eigenen DataFrame
  
  data <- data.frame(
    x = plot(day_model$model, trans = plogis,
             shift = coef(day_model$model)[1], se = 1.96,
             seWithMean = TRUE)[[i]]$x,
    fit = plot(day_model$model, trans = plogis,
               shift = coef(day_model$model)[1], se = 1.96,
               seWithMean = TRUE)[[i]]$fit,
    se = plot(day_model$model, trans = plogis,
              shift = coef(day_model$model)[1], se = 1.96,
              seWithMean = TRUE)[[i]]$se, 
    intercept = rep(coef(day_model$model)[1], 100)
  )
  
  # tatsächliche x-Werte der Daten müssen in eigenen DataFrame für den Rug später
  
  raw <- data.frame(
    raw = day_model$model$model[[i+2]]
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
        geom_rug(data = raw, aes(x = raw)) +
        scale_y_continuous(limits = c(0,0.5))
  
  
}

dev.off()


## aufgeteilt in Darstellung für den Grid und als Einzelplot


# Datum

date_grid <- plot_list[[1]] +
                labs(title = "Datum",
                     x = "", y = "") +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                                   labels = c("01. Jan","01. Feb",
                                              "01. Mär","01. Apr")) +
  annotate("rect", xmin = 17903, xmax = 17911, ymin = 0, ymax = 0.5, 
           fill = "white", alpha = .8) # graut Bereich aus

date_smooth <- plot_list[[1]] +
                  labs(title = "glatte Funktion für Datum",
                       x = "Datum",
                       y = "s(Datum)") +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                                     labels = c("01.Jan","01.Feb",
                                                "01.Mär","01.Apr")) +
  annotate("rect", xmin = 17903, xmax = 17911, ymin = 0, ymax = 0.5, 
           fill = "white", alpha = .8)
  


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

day_grid <- plot_list[[3]] +
              labs(title = "Wochentag",
                   x = "", y = "") +
              theme(plot.title = element_text(hjust = 0.5)) +
              scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),
                               labels=c("1" = "Mo", "2" = "Di",
                                        "3" = "Mi", "4" = "Do",
                                        "5" = "Fr", "6" = "Sa",
                                        "7" = "So"))

day_smooth <- plot_list[[3]] +
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

temperature_grid <- plot_list[[4]] +
                      labs(title = "Temperatur",
                           x = "",
                           y = "") +
                      theme(plot.title = element_text(hjust = 0.5)) +
                      scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6, 8))

temperature_smooth <- plot_list[[4]] +
                labs(title = "glatte Funktion für Temperatur",
                     x = "Temperatur in Grad Celsius",
                     y = "s(Temperatur)") +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6, 8))


# Bewölkung

cloud_cover_grid <- plot_list[[5]] +
                        labs(title = "Bewölkung",
                             x = "",
                             y = "") +
                        theme(plot.title = element_text(hjust = 0.5)) +
                        scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100))

cloud_cover_smooth <- plot_list[[5]] +
  labs(title = "glatte Funktion für die Bewölkung",
       x = "Bewölkung",
       y = "s(Bewölkung)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100))

# Schneedifferenz

snow_diff_grid <- plot_list[[6]] +
                    labs(title = "Schneedifferenz",
                         x = "",
                         y = "") +
                    theme(plot.title = element_text(hjust = 0.5)) +
                    scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40))

snow_diff_smooth <- plot_list[[6]] +
                labs(title = "glatte Funktion für Schneedifferenz",
                     x = "Schneedifferenz in cm",
                     y = "s(Schneedifferenz)") +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40))


# Grid für Tagesmodell Plots

day_model_grid <-
  list(day_grid,
       avalanche_grid,
       cloud_cover_grid,
       temperature_grid,
       snow_diff_grid,
       date_grid)



# Liste zur Ausgabe

plots_day_model_list <- list(
  day = day_smooth,
  avalanche = avalanche_smooth,
  cloud_cover = cloud_cover_smooth,
  temperature = temperature_smooth,
  snow_diff = snow_diff_smooth,
  date = date_smooth,
  grid = day_model_grid
  )

return(plots_day_model_list)

}


