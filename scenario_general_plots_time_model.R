

### In dieser Datei werden die Plots für den Vergleich des ersten Szenarios 
### (generelle Unterschätzung) je nach Anteil an hinzugefügten Messungen
### erstellt

time_model_general_comparison_function <- function(
  plots_scenario_general_time_model
) 
{


plots_scenario_general_time_model_comparison <- list()

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
# endet bei length(..)-1, da letztes Objekt in der Liste "grid" ist

for (j in 1:(length(plots_scenario_general_time_model[[1]])-2)) {
  
  plots_scenario_general_time_model_comparison[[j]] <- 
    ggplot() +
    # Konfidenzintervall für originales Szenario
    geom_ribbon(plots_scenario_general_time_model[[1]][[j]]$data,
                mapping = aes(x = x,
                              ymin = plogis(fit + intercept - se),
                              ymax = plogis(fit + intercept + se)),
                colour = "grey", alpha = 0.2) +
    geom_line(plots_scenario_general_time_model[[2]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "10%"),
              size = 1.0) +
    geom_line(plots_scenario_general_time_model[[3]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "20%"),
              size = 1.0) +
    geom_line(plots_scenario_general_time_model[[4]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "30%"),
              size = 1.0) +
    geom_line(plots_scenario_general_time_model[[5]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "40%"),
              size = 1.0) +
    geom_line(plots_scenario_general_time_model[[6]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "50%"),
              size = 1.0) +
    geom_line(plots_scenario_general_time_model[[1]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "Original"),
              size = 1.0) +
    scale_y_continuous(limits = c(0,1)) +
    labs(color = "Anteil") +
    scale_color_manual(breaks = c("Original", "10%", "20%", "30%", "40%", "50%"),
                       values = c("Original" = "#000000", 
                                  "10%" = "#12394e", 
                                  "20%" = "#20698e", 
                                  "30%" = "#2b8cbe", 
                                  "40%" = "#4da8d7", 
                                  "50%" = "#7dbfe2"))
  
}




# Plots richtig beschriften

theme <- theme(plot.title = element_text(hjust = 0.5),
               text = element_text(size = 10),
               legend.position = "bottom",
               legend.title = element_text(size = 12),
               legend.text = element_text(size = 10))

guides <- guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                                      title.position = "left",
                                      title.hjust = 0.5))

plots_scenario_general_time_model_comparison[[1]] <- 
  plots_scenario_general_time_model_comparison[[1]] +
  geom_rug(data = time_model_raw, aes(x = day)) +
  labs(title = "Wochentag",
       x = "", y = "") +
  scale_x_continuous(breaks = 1:7,
                     labels=c("1" = "Mo", "2" = "Di",
                              "3" = "Mi", "4" = "Do",
                              "5" = "Fr", "6" = "Sa",
                              "7" = "So")) +
  theme + guides

plots_scenario_general_time_model_comparison[[2]] <- 
  plots_scenario_general_time_model_comparison[[2]] +
  geom_rug(data = time_model_raw, aes(x = avalanche)) +
  labs(title = "Lawinenwarnstufe",
       x = "", y = "") +
  theme + guides

plots_scenario_general_time_model_comparison[[3]] <- 
  plots_scenario_general_time_model_comparison[[3]] +
  geom_rug(data = time_model_raw, aes(x = cloud_cover)) +
  labs(title = "Bewölkung",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme + guides

plots_scenario_general_time_model_comparison[[4]] <-
  plots_scenario_general_time_model_comparison[[4]] +
  geom_rug(data = time_model_raw, aes(x = temperature)) +
  labs(title = "Temperatur",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6)) +
  theme + guides

plots_scenario_general_time_model_comparison[[5]] <- 
  plots_scenario_general_time_model_comparison[[5]] +
  geom_rug(data = time_model_raw, aes(x = snow_diff)) +
  labs(title = "Schneedifferenz",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40)) +
  theme + guides

# Plots in der Liste den richtigen Namen geben

names(plots_scenario_general_time_model_comparison) <- 
  c("day", "avalanche", "cloud_cover", "temperature", "snow_diff")


## Grid erstellen

plots_scenario_general_time_model_comparison_grid <- 
  plots_scenario_general_time_model_comparison

# gemeinsame Legende speichern

legend_scenarios_time_model <- 
  get_legend(plots_scenario_general_time_model_comparison[[1]])

# Legende der einzelnen Plots löschen

for (j in 1:(length(plots_scenario_general_time_model[[1]])-2)) {
  plots_scenario_general_time_model_comparison_grid[[j]] <- 
    plots_scenario_general_time_model_comparison_grid[[j]] + 
    theme(legend.position = "none")
}

plots_scenario_general_time_model_comparison_grid <- 
  arrangeGrob(plots_scenario_general_time_model_comparison_grid[[1]],
              plots_scenario_general_time_model_comparison_grid[[2]],
              plots_scenario_general_time_model_comparison_grid[[3]],
              plots_scenario_general_time_model_comparison_grid[[4]],
              plots_scenario_general_time_model_comparison_grid[[5]],
              bottom = legend_scenarios_time_model,
              ncol = 3)

plots_time_model_comparison_list <- list(
  day = plots_scenario_general_time_model_comparison[[1]],
  avalanche = plots_scenario_general_time_model_comparison[[2]],
  cloud_cover = plots_scenario_general_time_model_comparison[[3]],
  temperature = plots_scenario_general_time_model_comparison[[4]],
  snow_diff = plots_scenario_general_time_model_comparison[[5]],
  date_time_original = plots_scenario_general_time_model[[1]]$date_time,
  date_time_10 = plots_scenario_general_time_model[[2]]$date_time,
  date_time_20 = plots_scenario_general_time_model[[3]]$date_time,
  date_time_30 = plots_scenario_general_time_model[[4]]$date_time,
  date_time_40 = plots_scenario_general_time_model[[5]]$date_time,
  date_time_50 = plots_scenario_general_time_model[[6]]$date_time,
  grid = plots_scenario_general_time_model_comparison_grid
)

return(plots_time_model_comparison_list)

}
