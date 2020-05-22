

### In dieser Datei werden die Plots für die Szenarien erstellt und verglichen
### Für das Day Model


scenarios <- list(data,
                  data_general_function(0.25)$data,
                  min_data_group,
                  min_data_night,
                  min_data_temp)

plots_scenarios_day_model <- list()
plots_scenarios_day_model_comparison <- list()


# für jedes der 5 Szenarien Smooth-Plots erstellen

for (i in 1:length(scenarios)) {
  
  plots_scenarios_day_model[[i]] <- scenarios[[i]] %>% 
                                      day_model_function()  %>%
                                        plots_day_model()
  
}

# für jede Kovariable gemeinsame Plots erstellen

# startet bei 2, da erste Variable date_time keinen Smooth-Plot hat
# endet bei length(..)-1, da letztes Objekt in der Liste "grid" ist

for (j in 2:(length(plots_scenarios_day_model[[1]])-1)) {
  
  plots_scenarios_day_model_comparison[[j-1]] <- 
    ggplot() +
    geom_line(plots_scenarios_day_model[[2]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Generelle Unterschätzung von 20%"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[3]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Unterschätzung nach Gruppengröße"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[4]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty,
                            color = "Nächtliche Überschätzung"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[5]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty,
                            color = "Unterschätzung bei niedrigen Temperaturen"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[1]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Original"),
              size = 1.05) +
    # Konfidenzintervall für originales Szenario
    geom_ribbon(plots_scenarios_day_model[[1]][[j]]$data$fit,
                mapping = aes(x = x,
                              ymin = plogis(y - 1.96*se),
                              ymax = plogis(y + 1.96*se)),
                colour = "grey", alpha = 0.2) +
    scale_y_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(color = "Szenario") +
    scale_color_manual(breaks=c("Original",
                                "Generelle Unterschätzung von 20%",
                                "Unterschätzung nach Gruppengröße",
                                "Nächtliche Überschätzung",
                                "Nächtliche Überschätzung",
                                "Unterschätzung bei niedrigen Temperaturen"),
                       values = c("#D55E00", "#CC79A7", "#000000", "#009E73",
                                  "#56B4E9" ))
  
}



# Plots richtig beschriften

plots_scenarios_day_model_comparison[[1]] <- 
  plots_scenarios_day_model_comparison[[1]] +
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
  labs(title = "Lawinenwarnstufe",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

plots_scenarios_day_model_comparison[[3]] <- 
  plots_scenarios_day_model_comparison[[3]] +
  labs(title = "Sonneneinstrahlung",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))

plots_scenarios_day_model_comparison[[4]] <-
  plots_scenarios_day_model_comparison[[4]] +
  labs(title = "Temperatur",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6))

plots_scenarios_day_model_comparison[[5]] <- 
  plots_scenarios_day_model_comparison[[5]] +
  labs(title = "Neuschnee",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40))

# Plots in der Liste den richtigen Namen geben

names(plots_scenarios_day_model_comparison) <- 
  c("day", "avalanche", "solar_radiation", "temperature", "snowhight")


## Grid erstellen

plots_scenarios_day_model_comparison_grid <- 
  plots_scenarios_day_model_comparison

# gemeinsame Legende speichern

legend_scenarios_day_model <- 
  get_legend(plots_scenarios_day_model_comparison[[2]])

# Legende der einzelnen Plots löschen

for (j in 1:(length(plots_scenarios_day_model[[1]])-1)) {
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
              ncol = 2,
              bottom = legend_scenarios_day_model,
              top = "Smooth-Plots für jedes Szenario im Vergleich")
