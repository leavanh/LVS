

### In dieser Datei werden die Plots für den Vergleich des ersten Szenarios 
### (generelle Unterschätzung) je nach Anteil an hinzugefügten Messungen
### erstellt


scenarios <- list(min_data_noNA,
                  data_general_function(0.05)$data,
                  data_general_function(0.1)$data,
                  data_general_function(0.15)$data,
                  data_general_function(0.2)$data,
                  data_general_function(0.25)$data,
                  data_general_function(0.3)$data,
                  data_general_function(0.35)$data,
                  data_general_function(0.4)$data,
                  data_general_function(0.45)$data,
                  data_general_function(0.5)$data)

scenarios_day_model <- list()
plots_scenarios_day_model <- list()
plots_scenarios_day_model_comparison <- list()


# für jedes der Szenarien Smooth-Plots-erstellen

for (i in 1:length(scenarios)) {
  
  scenarios_day_model[[i]] <- scenarios[[i]] %>% day_model_function()
  
  plots_scenarios_day_model[[i]] <- scenarios_day_model[[i]]  %>%
    plots_day_model()
  
}

# Farben für die Unterscheidung

colors <- cbind(
  pct = c("Original", "5%", "10%", "15%", "20%", "25%", "30%", "35%", 
          "40%", "45%", "50%"),
  color = c("#040007", "#0e001b", "#17002f", "#210042", "#2b0056", "#350069",
            "#3f007d", "#490091", "#5300a4", "#5d00b8", "#6700cb")
)


# für jede Kovariable gemeinsame Plots erstellen
# endet bei length(..)-1, da letztes Objekt in der Liste "grid" ist

for (j in 1:(length(plots_scenarios_day_model[[1]])-2)) {
  
  plots_scenarios_day_model_comparison[[j]] <- 
    ggplot() +
    # Konfidenzintervall für originales Szenario
    geom_ribbon(plots_scenarios_day_model[[1]][[j]]$data,
                mapping = aes(x = x,
                              ymin = plogis(fit + intercept - se),
                              ymax = plogis(fit + intercept + se)),
                colour = "grey", alpha = 0.2) +
    geom_line(plots_scenarios_day_model[[2]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "5%"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[3]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "10%"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[4]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "15%"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[5]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "20%"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[6]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "25%"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[7]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "30%"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[8]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "35%"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[9]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "40%"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[10]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "45%"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[11]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "50%"),
              size = 1.05) +
    geom_line(plots_scenarios_day_model[[1]][[j]]$data, 
              mapping = aes(x = x, y = plogis(fit + intercept), 
                            color = "Original"),
              size = 1.05) +
    scale_y_continuous(limits = c(0,1)) +
    labs(color = "Anteil") +
    scale_color_manual(breaks = colors[,1],
                       values = colors[order(colors[,1]),2])
  
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
  geom_rug(data = day_model_raw, aes(x = solar_radiation)) +
  labs(title = "Sonneneinstrahlung",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))

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
  c("day", "avalanche", "solar_radiation", "temperature", "snowhight")


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

grid3 <- 
  grid.arrange(plots_scenarios_day_model_comparison_grid)

ggsave("Plots/Szenario1.png", grid3, scale = 1.2)
