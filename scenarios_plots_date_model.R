

### In dieser Datei werden die Plots für die Szenarien erstellt und verglichen
### Für das Date Model


scenarios <- list(date_data_noNA,
                  data_general_function(0.25)$date_data,
                  date_data_group,
                  date_data_night,
                  date_data_temp)

scenarios_date_model <- list()
plots_scenarios_date_model <- list()
plots_scenarios_date_model_comparison <- list()




# für jedes der 5 Szenarien Smooth-Plots-erstellen

for (i in 1:length(scenarios)) {
  
  scenarios_date_model[[i]] <- scenarios[[i]] %>% date_model_function()
  
  plots_scenarios_date_model[[i]] <- scenarios_date_model[[i]]  %>%
                                          plots_date_model()
  
}

# Werte für den Rug speichern

date_model_raw <- data.frame(
  day = scenarios_date_model[[1]]$model$model$int_day,
  avalanche = scenarios_date_model[[1]]$model$model$avalanche_report,
  cloud_cover = scenarios_date_model[[1]]$model$model$cloud_cover_daily,
  temperature = scenarios_date_model[[1]]$model$model$temperature,
  snow_diff = scenarios_date_model[[1]]$model$model$snow_diff,
  date = scenarios_date_model[[1]]$model$model$int_date
)

# für jede Kovariable gemeinsame Plots erstellen
# endet bei length(..)-1, da letztes Objekt in der Liste "grid" ist

for (j in 1:(length(plots_scenarios_date_model[[1]])-1)) {
  
  raw <- data.frame(
    raw = scenarios_date_model$model$model[[j+2]]
  )
  
  plots_scenarios_date_model_comparison[[j]] <- 
    ggplot() +
    # Konfidenzintervall für originales Szenario
    geom_ribbon(plots_scenarios_date_model[[1]][[j]]$data,
                mapping = aes(x = x,
                              ymin = plogis(fit + intercept - se),
                              ymax = plogis(fit + intercept + se)),
                colour = "grey", alpha = 0.2) +
    geom_line(plots_scenarios_date_model[[2]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "Generelle Unterschätzung von 25%"),
              size = 1.0) +
    geom_line(plots_scenarios_date_model[[3]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "Unterschätzung nach Gruppengröße"),
              size = 1.0) +
    geom_line(plots_scenarios_date_model[[4]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept),
                            color = "Nächtliche Überschätzung"),
              size = 1.0) +
    geom_line(plots_scenarios_date_model[[5]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept),
                            color = "Unterschätzung bei niedrigen Temperaturen"),
              size = 1.0) +
    geom_line(plots_scenarios_date_model[[1]][[j]]$data, 
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
                                  "Unterschätzung nach Gruppengröße" = "#E69F00"))
  
  # Reihenfolge der Farben: Generell, Nächtl, Original, Unt_temp, Unt_group
  
}

# Plots richtig beschriften

# Schrift und Legende für alle Plots einstellen

theme <- theme(plot.title = element_text(hjust = 0.5), 
                 text = element_text(size = 10),
                 legend.position = "bottom",
                 legend.title = element_text(size = 12),
                 legend.text = element_text(size = 10))

guides <- guides(color = guide_legend(ncol = 3, byrow = TRUE, 
                                      title.position = "left",
                                      title.hjust = 0.5))

plots_scenarios_date_model_comparison[[6]] <- 
  plots_scenarios_date_model_comparison[[6]] +
  geom_rug(data = date_model_raw, aes(x = date)) +
  labs(title = "Datum",
       x = "", y = "") +
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01.Jan","01.Feb",
                                "01.Mar","01.Apr")) + 
  theme + guides

plots_scenarios_date_model_comparison[[1]] <- 
  plots_scenarios_date_model_comparison[[1]] +
  geom_rug(data = date_model_raw, aes(x = day)) +
  labs(title = "Wochentag",
       x = "", y = "") +
  scale_x_continuous(breaks = 1:7,
                   labels=c("1" = "Mo", "2" = "Di",
                            "3" = "Mi", "4" = "Do",
                            "5" = "Fr", "6" = "Sa",
                            "7" = "So")) +
  theme + guides

plots_scenarios_date_model_comparison[[2]] <- 
  plots_scenarios_date_model_comparison[[2]] +
  geom_rug(data = date_model_raw, aes(x = avalanche)) +
  labs(title = "Lawinenwarnstufe",
       x = "", y = "") +
  theme + guides

plots_scenarios_date_model_comparison[[3]] <- 
  plots_scenarios_date_model_comparison[[3]] +
  geom_rug(data = date_model_raw, aes(x = cloud_cover)) +
  labs(title = "Bewölkung",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme + guides

plots_scenarios_date_model_comparison[[4]] <- 
  plots_scenarios_date_model_comparison[[4]] +
  geom_rug(data = date_model_raw, aes(x = temperature)) +
  labs(title = "Temperatur",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-5, 0, 5, 10)) +
  theme + guides

plots_scenarios_date_model_comparison[[5]] <- 
  plots_scenarios_date_model_comparison[[5]] +
  geom_rug(data = date_model_raw, aes(x = snow_diff)) +
  labs(title = "Schneedifferenz",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40)) +
  theme + guides

# Plots in der Liste den richtigen Namen geben

names(plots_scenarios_date_model_comparison) <- 
  c("day", "avalanche", "cloud_cover", "temperature", "snow_diff", "date")


## Grid erstellen

plots_scenarios_date_model_comparison_grid <- 
  plots_scenarios_date_model_comparison

# gemeinsame Legende speichern

legend_scenarios_date_model <- 
  get_legend(plots_scenarios_date_model_comparison[[6]])

# Legende der einzelnen Plot löschen

for (j in 1:6) {
  plots_scenarios_date_model_comparison_grid[[j]] <- 
    plots_scenarios_date_model_comparison_grid[[j]] + 
    theme(legend.position = "none")
}


plots_scenarios_date_model_comparison_grid <- 
  arrangeGrob(plots_scenarios_date_model_comparison_grid[[1]],
              plots_scenarios_date_model_comparison_grid[[2]],
              plots_scenarios_date_model_comparison_grid[[3]],
              plots_scenarios_date_model_comparison_grid[[4]],
              plots_scenarios_date_model_comparison_grid[[5]],
              plots_scenarios_date_model_comparison_grid[[6]],
              ncol = 3,
              bottom = legend_scenarios_date_model)


