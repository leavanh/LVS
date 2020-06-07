

### In dieser Datei werden die Plots für die Szenarien erstellt und verglichen
### Für das Day Model


scenarios <- list(min_data_noNA,
                  data_general_function(0.25)$data,
                  min_data_group,
                  min_data_night,
                  min_data_temp)

scenarios_day_model <- list()
plots_scenarios_day_model <- list()
plots_scenarios_day_model_comparison <- list()


# für jedes der 5 Szenarien Smooth-Plots erstellen

for (i in 1:length(scenarios)) {
  
  scenarios_day_model[[i]] <- scenarios[[i]] %>% day_model_function()
  
  plots_scenarios_day_model[[i]] <- scenarios_day_model[[i]]  %>%
    plots_day_model()
  
}

# Werte für den Rug speichern

day_model_raw <- data.frame(
  time = scenarios_day_model[[1]]$model$model$num_time,
  day = scenarios_day_model[[1]]$model$model$int_day,
  avalanche = scenarios_day_model[[1]]$model$model$avalanche_report,
  cloud_cover = scenarios_day_model[[1]]$model$model$cloud_cover,
  temperature = scenarios_day_model[[1]]$model$model$temperature,
  snow_diff = scenarios_day_model[[1]]$model$model$snow_diff,
  date = scenarios_day_model[[1]]$model$model$int_date
)

# für jede Kovariable gemeinsame Plots erstellen

# startet bei 2, da erste Variable date_time keinen Smooth-Plot hat
# endet bei length(..)-1, da letztes Objekt in der Liste "grid" ist

for (j in 2:(length(plots_scenarios_day_model[[1]])-1)) {
  
  plots_scenarios_day_model_comparison[[j-1]] <- 
    ggplot() +
    # Konfidenzintervall für originales Szenario
    geom_ribbon(plots_scenarios_day_model[[1]][[j-1]]$data,
                mapping = aes(x = x,
                              ymin = plogis(fit + intercept - se),
                              ymax = plogis(fit + intercept + se)),
                colour = "grey", alpha = 0.2) +
    geom_line(plots_scenarios_day_model[[2]][[j-1]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "Generelle Unterschätzung von 25%"),
              size = 1.0) +
    geom_line(plots_scenarios_day_model[[3]][[j-1]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "Unterschätzung nach Gruppengröße"),
              size = 1.0) +
    geom_line(plots_scenarios_day_model[[4]][[j-1]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept),
                            color = "Nächtliche Überschätzung"),
              size = 1.0) +
    geom_line(plots_scenarios_day_model[[5]][[j-1]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept),
                            color = "Unterschätzung bei niedrigen Temperaturen"),
              size = 1.0) +
    geom_line(plots_scenarios_day_model[[1]][[j-1]]$data, 
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
                       values = c("#756bb1", "#CC79A7", "#000000", "#009E73",
                                  "#E69F00"))
  
}



# Plots richtig beschriften

plots_scenarios_day_model_comparison[[1]] <- 
  plots_scenarios_day_model_comparison[[1]] +
  geom_rug(data = day_model_raw, aes(x = day)) +
  labs(title = "Wochentag",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = 1:7,
                     labels=c("1" = "Mo", "2" = "Di",
                              "3" = "Mi", "4" = "Do",
                              "5" = "Fr", "6" = "Sa",
                              "7" = "So"))

plots_scenarios_day_model_comparison[[2]] <- 
  plots_scenarios_day_model_comparison[[2]] +
  geom_rug(data = day_model_raw, aes(x = avalanche)) +
  labs(title = "Lawinenwarnstufe",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

plots_scenarios_day_model_comparison[[3]] <- 
  plots_scenarios_day_model_comparison[[3]] +
  geom_rug(data = day_model_raw, aes(x = cloud_cover)) +
  labs(title = "Bewölkung",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100))

plots_scenarios_day_model_comparison[[4]] <-
  plots_scenarios_day_model_comparison[[4]] +
  geom_rug(data = day_model_raw, aes(x = temperature)) +
  labs(title = "Temperatur",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6))

plots_scenarios_day_model_comparison[[5]] <- 
  plots_scenarios_day_model_comparison[[5]] +
  geom_rug(data = day_model_raw, aes(x = snow_diff)) +
  labs(title = "Schneedifferenz",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40))

# Plots in der Liste den richtigen Namen geben

names(plots_scenarios_day_model_comparison) <- 
  c("day", "avalanche", "cloud_cover", "temperature", "snow_diff")


## Grid erstellen

plots_scenarios_day_model_comparison_grid <- 
  plots_scenarios_day_model_comparison

# gemeinsame Legende speichern

legend_scenarios_day_model <- 
  get_legend(plots_scenarios_day_model_comparison[[1]])

# Legende der einzelnen Plots löschen

for (j in 1:(length(plots_scenarios_day_model[[1]])-2)) {
  plots_scenarios_day_model_comparison_grid[[j]] <- 
    plots_scenarios_day_model_comparison_grid[[j]] + 
    theme(legend.position = "none")
}

plots_scenarios_day_model_comparison_grid <- 
  arrangeGrob(plots_scenarios_day_model_comparison_grid[[1]],
              plots_scenarios_day_model_comparison_grid[[2]],
              plots_scenarios_day_model_comparison_grid[[3]],
              plots_scenarios_day_model_comparison_grid[[4]],
              plots_scenarios_day_model_comparison_grid[[5]],
              legend_scenarios_day_model,
              ncol = 3)
