

library("gridExtra")

theme_set(theme_minimal())

## Datensätze für die Szenarien erstellen

# Funktion zur Speicherung einer ggplot-Legende laden

source("get_legend.R")

# Datensätze zu den Szenarios laden

source("data_general.R", encoding = "UTF-8")
source("data_group.R", encoding = "UTF-8")
source("data_night.R", encoding = "UTF-8")
source("data_temp.R", encoding = "UTF-8")


scenarios <- list(date_data,
                  date_data_general,
                  date_data_group,
                  date_data_night,
                  date_data_temp)

plots_scenarios_date_model <- list()
plots_scenarios <- list()


# für jedes der 5 Szenarien Smooth-Plots-erstellen

for (i in 1:length(scenarios)) {
  
  plots_scenarios_date_model[[i]] <- scenarios[[i]] %>% 
                                        date_model_function()  %>%
                                          plots_date_model()
  
}

# für jede Kovariable gemeinsame Plots erstellen

for (j in 1:6) {
  
  plots_scenarios[[j]] <- 
    ggplot() +
    geom_line(plots_scenarios_date_model[[1]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Original")) +
    geom_line(plots_scenarios_date_model[[2]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Generelle Unterschätzung von 20%")) +
    geom_line(plots_scenarios_date_model[[3]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty, 
                            color = "Unterschätzung nach Gruppengröße")) +
    geom_line(plots_scenarios_date_model[[4]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty,
                            color = "Nächtliche Überschätzung")) +
    geom_line(plots_scenarios_date_model[[5]][[j]]$data$fit, 
              mapping = aes(x = x,y = ty,
                            color = "Unterschätzung bei niedrigen Temperaturen")
              ) +
    scale_y_continuous(limits = c(0,1)) +
    labs(color = "Szenario")
  
}


## Grid erstellen

# gemeinsame Legende speichern

legend_scenarios_date_model <- get_legend(plots_scenarios[[1]])

# Legende der einzelnen Plot löschen

for (j in 1:6) {
  plots_scenarios[[j]] <- plots_scenarios[[j]] + theme(legend.position = "none")
}


# Plots richtig beschriften

plots_scenarios[[1]] <- plots_scenarios[[1]] +
  labs(title = "Smooth-Funktion für Datum",
       x = "Datum",
       y = "s(Datum)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01. Jan","01. Feb",
                                "01. Mär","01. Apr"))

plots_scenarios[[3]] <- plots_scenarios[[3]] +
  labs(title = "Lawinenwarnstufe",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

plots_scenarios[[2]] <- plots_scenarios[[2]] +
  labs(title = "Wochentag",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = 1:7,
                   labels=c("1" = "Mo", "2" = "Di",
                            "3" = "Mi", "4" = "Do",
                            "5" = "Fr", "6" = "Sa",
                            "7" = "So"))

plots_scenarios[[4]] <- plots_scenarios[[4]] +
  labs(title = "Temperatur",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6))

plots_scenarios[[5]] <- plots_scenarios[[5]] +
  labs(title = "Sonneneinstrahlung",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-4, -3, -2, -1, 0, 1, 2))

plots_scenarios[[6]] <- plots_scenarios[[6]] +
  labs(title = "Schneehöhe",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(-18, -15, -12, -9, -6, -3, 
                                0, 3, 6, 9, 12, 15, 18))


grid.arrange(plots_scenarios[[1]],
             plots_scenarios[[2]],
             plots_scenarios[[3]],
             plots_scenarios[[4]],
             plots_scenarios[[5]],
             plots_scenarios[[6]],
             legend_scenarios_date_model,
             ncol = 3,
             layout_matrix = cbind(c(1,2,7), c(3,4,7), c(5,6,7)),
             top = "Smooth-Plots für jedes Szenario im Vergleich"
             )
