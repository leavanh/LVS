

theme_set(theme_minimal())

## Datensätze für die Szenarien erstellen


scenarios <- list(data,
                  min_data_general,
                  min_data_group,
                  min_data_night,
                  min_data_temp)

plots_scenarios_day_model <- list()
plots_scenarios_day_model_comparison <- list()


# für jedes der 5 Szenarien Smooth-Plots-erstellen

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
    geom_line(plots_scenarios_day_model[[1]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Original")) +
    geom_line(plots_scenarios_day_model[[2]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Generelle Unterschätzung von 20%")) +
    geom_line(plots_scenarios_day_model[[3]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Unterschätzung nach Gruppengröße")) +
    geom_line(plots_scenarios_day_model[[4]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty,
                            color = "Nächtliche Überschätzung")) +
    geom_line(plots_scenarios_day_model[[5]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty,
                            color = "Unterschätzung bei niedrigen Temperaturen")
    ) +
    scale_y_continuous(limits = c(0,1)) +
    labs(color = "Szenario")
  
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

# Temperatur zur Zeit nicht im Day Model

# plots_scenarios_day_model_comparison[[4]] <- 
#   plots_scenarios_day_model_comparison[[4]] +
#   labs(title = "Temperatur",
#        x = "",
#        y = "") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6))

plots_scenarios_day_model_comparison[[3]] <- 
  plots_scenarios_day_model_comparison[[3]] +
  labs(title = "Sonneneinstrahlung",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))

plots_scenarios_day_model_comparison[[4]] <- 
  plots_scenarios_day_model_comparison[[4]] +
  labs(title = "Neuschnee",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40))

names(plots_scenarios_day_model_comparison) <- 
  c("day", "avalanche", "solar_radiation", "snowhight")


## Grid erstellen

plots_scenarios_day_model_comparison_grid <- 
  plots_scenarios_day_model_comparison

# gemeinsame Legende speichern

legend_scenarios_day_model <- 
  get_legend(plots_scenarios_day_model_comparison[[2]])

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
             legend_scenarios_day_model,
             ncol = 2,
             # layout_matrix = cbind(c(1,2,7), c(3,4,7), c(5,6,7)),
             top = "Smooth-Plots für jedes Szenario im Vergleich")
