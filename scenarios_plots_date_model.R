

### In dieser Datei werden die Plots für die Szenarien erstellt und verglichen
### Für das Date Model


scenarios <- list(date_data,
                  date_data_general,
                  date_data_group,
                  date_data_night,
                  date_data_temp)

plots_scenarios_date_model <- list()
plots_scenarios_date_model_comparison <- list()


# für jedes der 5 Szenarien Smooth-Plots-erstellen

for (i in 1:length(scenarios)) {
  
  plots_scenarios_date_model[[i]] <- scenarios[[i]] %>% 
                                        date_model_function()  %>%
                                          plots_date_model()
  
}

# für jede Kovariable gemeinsame Plots erstellen
# endet bei length(..)-1, da letztes Objekt in der Liste "grid" ist

for (j in 1:(length(plots_scenarios_date_model[[1]])-1)) {
  
  plots_scenarios_date_model_comparison[[j]] <- 
    ggplot() +
    geom_line(plots_scenarios_date_model[[2]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Generelle Unterschätzung von 20%"),
              size = 1.05) +
    geom_line(plots_scenarios_date_model[[3]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Unterschätzung nach Gruppengröße"),
              size = 1.05) +
    geom_line(plots_scenarios_date_model[[4]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty,
                            color = "Nächtliche Überschätzung"),
              size = 1.05) +
    geom_line(plots_scenarios_date_model[[5]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty,
                            color = "Unterschätzung bei niedrigen Temperaturen"),
              size = 1.05) +
    geom_line(plots_scenarios_date_model[[1]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Original"), 
              size = 1.5) +
    scale_y_continuous(limits = c(0,1)) +
    labs(color = "Szenario") +
    scale_color_brewer(breaks=c("Original",
                                  "Generelle Unterschätzung von 20%",
                                  "Unterschätzung nach Gruppengröße",
                                  "Nächtliche Überschätzung",
                                  "Nächtliche Überschätzung",
                                  "Unterschätzung bei niedrigen Temperaturen"),
                         palette = "Set1")

  
}



# Plots richtig beschriften

plots_scenarios_date_model_comparison[[1]] <- 
  plots_scenarios_date_model_comparison[[1]] +
  labs(title = "Smooth-Funktion für Datum",
       x = "Datum",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01. Jan","01. Feb",
                                "01. Mär","01. Apr"))

plots_scenarios_date_model_comparison[[3]] <- 
  plots_scenarios_date_model_comparison[[3]] +
  labs(title = "Lawinenwarnstufe",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

plots_scenarios_date_model_comparison[[2]] <- 
  plots_scenarios_date_model_comparison[[2]] +
  labs(title = "Wochentag",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = 1:7,
                   labels=c("1" = "Mo", "2" = "Di",
                            "3" = "Mi", "4" = "Do",
                            "5" = "Fr", "6" = "Sa",
                            "7" = "So"))

plots_scenarios_date_model_comparison[[4]] <- 
  plots_scenarios_date_model_comparison[[4]] +
  labs(title = "Sonneneinstrahlung",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))

plots_scenarios_date_model_comparison[[5]] <- 
  plots_scenarios_date_model_comparison[[5]] +
  labs(title = "Temperatur",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-5, 0, 5, 10))

plots_scenarios_date_model_comparison[[6]] <- 
  plots_scenarios_date_model_comparison[[6]] +
  labs(title = "Neuschnee",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40))

# Plots in der Liste den richtigen Namen geben

names(plots_scenarios_date_model_comparison) <- 
  c("date", "avalanche", "temperature", "day", "solar_radiation", "snowhight")


## Grid erstellen

plots_scenarios_date_model_comparison_grid <- 
  plots_scenarios_date_model_comparison

# gemeinsame Legende speichern

legend_scenarios_date_model <- 
  get_legend(plots_scenarios_date_model_comparison[[1]])

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
             legend_scenarios_date_model,
             ncol = 3,
             layout_matrix = cbind(c(1,2,7), c(3,4,7), c(5,6,7)),
             top = "Smooth-Plots für jedes Szenario im Vergleich"
             )
