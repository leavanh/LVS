
### Diese Funktion plottet die einzelnen Kurven der verschiedenen Szenarien
### für jede Kovariable zusammen auf einen Graphen - für das Zeitmodell
### Die Funktion nimmt als Argument eine Liste von (Listen von) Plots je 
### Szenario und gibt wiederum eine Liste von Plots aus, die für jede Kovariable
### die verschiedenen Kurven auf einem Bild zeigt


time_model_comparison_plots_function <- function(
  plots_scenarios_time_model
)
{

plots_scenarios_time_model_comparison <- list()

# Werte für den Rug speichern

time_model_raw <- data.frame(
  time = time_model$model$model$num_time,
  day = time_model$model$model$int_day,
  avalanche = time_model$model$model$avalanche_report,
  cloud_cover = time_model$model$model$cloud_cover,
  temperature = time_model$model$model$temperature,
  snow_diff = time_model$model$model$snow_diff,
  date = time_model$model$model$int_date
)

# für jede Kovariable gemeinsame Plots erstellen

# startet bei 2, da erste Variable "date_time" keinen Smooth-Plot hat
# endet bei length(..)-1, da letztes Objekt in der Liste "grid" ist

for (j in 1:(length(plots_scenarios_time_model[[1]])-2)) {
  
  plots_scenarios_time_model_comparison[[j]] <- 
    ggplot() +
    # Konfidenzintervall für originales Szenario
    geom_ribbon(plots_scenarios_time_model[[1]][[j]]$data,
                mapping = aes(x = x,
                              ymin = plogis(fit + intercept - se),
                              ymax = plogis(fit + intercept + se)),
                colour = "grey", alpha = 0.2) +
    geom_line(plots_scenarios_time_model[[2]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "Generelle Unterschätzung von 25%"),
              size = 1.0) +
    geom_line(plots_scenarios_time_model[[3]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "Unterschätzung nach Gruppengröße"),
              size = 1.0) +
    geom_line(plots_scenarios_time_model[[4]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept),
                            color = "Nächtliche Überschätzung"),
              size = 1.0) +
    geom_line(plots_scenarios_time_model[[5]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept),
                            color = "Unterschätzung bei niedrigen Temperaturen"),
              size = 1.0) +
    geom_line(plots_scenarios_time_model[[1]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "Original"),
              size = 1.0) +
    scale_y_continuous(limits = c(0,0.5)) +
    labs(color = "Szenario") +
    scale_color_manual(breaks=c("Original",
                                "Generelle Unterschätzung von 25%",
                                "Unterschätzung nach Gruppengröße",
                                "Nächtliche Überschätzung",
                                "Unterschätzung bei niedrigen Temperaturen"),
                       values = c("Generelle Unterschätzung von 25%" = "#2b8cbe", 
                                  "Nächtliche Überschätzung" = "#009E73", 
                                  "Original" = "#000000", 
                                  "Unterschätzung bei niedrigen Temperaturen" = 
                                    "#CC79A7",
                                  "Unterschätzung nach Gruppengröße" = 
                                    "#E69F00"))
  
}



# Plots richtig beschriften

# Schrift und Legende für alle Plots einstellen

theme <- theme(plot.title = element_text(hjust = 0.5), 
               text = element_text(size = 10),
               legend.position = "bottom",
               legend.title = element_text(size = 12),
               legend.text = element_text(size = 10))

guides <- guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                                      title.position = "left",
                                      title.hjust = 0.5))

plots_scenarios_time_model_comparison[[1]] <- 
  plots_scenarios_time_model_comparison[[1]] +
  geom_rug(data = time_model_raw, aes(x = day)) +
  labs(title = "Wochentag",
       x = "", y = "") +
  scale_x_continuous(breaks = 1:7,
                     labels=c("1" = "Mo", "2" = "Di",
                              "3" = "Mi", "4" = "Do",
                              "5" = "Fr", "6" = "Sa",
                              "7" = "So")) +
  theme + guides

plots_scenarios_time_model_comparison[[2]] <- 
  plots_scenarios_time_model_comparison[[2]] +
  geom_rug(data = time_model_raw, aes(x = avalanche)) +
  labs(title = "Lawinenwarnstufe",
       x = "", y = "")  +
  theme + guides

plots_scenarios_time_model_comparison[[3]] <- 
  plots_scenarios_time_model_comparison[[3]] +
  geom_rug(data = time_model_raw, aes(x = cloud_cover)) +
  labs(title = "Bewölkung",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme + guides

plots_scenarios_time_model_comparison[[4]] <-
  plots_scenarios_time_model_comparison[[4]] +
  geom_rug(data = time_model_raw, aes(x = temperature)) +
  labs(title = "Temperatur",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6)) +
  theme + guides

plots_scenarios_time_model_comparison[[5]] <- 
  plots_scenarios_time_model_comparison[[5]] +
  geom_rug(data = time_model_raw, aes(x = snow_diff)) +
  labs(title = "Schneedifferenz",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40)) +
  theme + guides

# Plots in der Liste den richtigen Namen geben

names(plots_scenarios_time_model_comparison) <- 
  c("day", "avalanche", "cloud_cover", "temperature", "snow_diff")


## Grid erstellen

plots_scenarios_time_model_comparison_grid <- 
  plots_scenarios_time_model_comparison

# gemeinsame Legende speichern

legend_scenarios_time_model <- 
  get_legend(plots_scenarios_time_model_comparison[[1]])

# Legende der einzelnen Plots im Grid löschen

for (j in 1:(length(plots_scenarios_time_model[[1]])-2)) {
  plots_scenarios_time_model_comparison_grid[[j]] <- 
    plots_scenarios_time_model_comparison_grid[[j]] + 
    theme(legend.position = "none")
}

# Überblick über alle Plots erstellen

plots_scenarios_time_model_comparison_grid <- 
  arrangeGrob(plots_scenarios_time_model_comparison_grid[[1]],
              plots_scenarios_time_model_comparison_grid[[2]],
              plots_scenarios_time_model_comparison_grid[[3]],
              plots_scenarios_time_model_comparison_grid[[4]],
              plots_scenarios_time_model_comparison_grid[[5]],
              bottom = legend_scenarios_time_model,
              ncol = 3)

# Liste zur Ausgabe erstellen

plots_time_model_comparison_list <- list(
  day = plots_scenarios_time_model_comparison[[1]],
  avalanche = plots_scenarios_time_model_comparison[[2]],
  cloud_cover = plots_scenarios_time_model_comparison[[3]],
  temperature = plots_scenarios_time_model_comparison[[4]],
  snow_diff = plots_scenarios_time_model_comparison[[5]],
  date_time_original = plots_scenarios_time_model[[1]]$date_time,
  date_time_general = plots_scenarios_time_model[[2]]$date_time,
  date_time_group = plots_scenarios_time_model[[3]]$date_time,
  date_time_night = plots_scenarios_time_model[[4]]$date_time,
  date_time_temp = plots_scenarios_time_model[[5]]$date_time,
  grid = plots_scenarios_time_model_comparison_grid
)

return(plots_time_model_comparison_list)

}
