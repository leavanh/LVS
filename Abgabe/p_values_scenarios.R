
## Hier wird für beide Modelle jeweils eine Tabelle erstellt, die p-Werte und
## (falls sinnvoll) Koeffizienten anzeigt, je nach Szenario

# Namen für die spätere Beschriftung

scenario_names <- c("original", "general", "group", "night", "temp")

# Für das Tagesmodell



# DataFrame erstellen mit 8 Zeilen, da 8 Kovariablen

p_values_scenarios_day_model <- data.frame(1:8)

# Für jedes Szenario Teiltabelle mit Koeffizienten, p-Werten und Signifikanzen 
# erstellen

i <- 0

for (n in 1:length(scenarios_day_model)) {
  
  i = i + 1
  
  p_names <- row.names(scenarios_day_model[[i]]$summary$p.table)
  
  s_names <- row.names(scenarios_day_model[[i]]$summary$s.table)
  
  coefficients <- c(format(c(plogis(
    scenarios_day_model[[i]]$summary$p.coeff[1]), plogis(
      scenarios_day_model[[i]]$summary$p.coeff[1] +
        scenarios_day_model[[i]]$summary$p.coeff[2])), digits = 3),
    rep("x", length(s_names)))
  
  p_values <- c(scenarios_day_model[[i]]$summary$p.pv,
                scenarios_day_model[[i]]$summary$s.pv)
  
  stars <- stars.pval(p_values)
  
  p_values <- format.pval(p_values, digits = 1)
  
  p_values_scenarios_day_model <- p_values_scenarios_day_model %>% 
    cbind(coefficients, p_values, stars)
  
  names(p_values_scenarios_day_model)[(2:4)+3*(i-1)] <- 
    c(paste0("coeff (", scenario_names[n], ")"), 
      paste0("p (", scenario_names[n], ")"), " ")
  
  if (i == 1) { 
    row.names(p_values_scenarios_day_model) <- c(p_names, s_names) 
  }
  
}

# erste Reihe löschen, da nur zur Erstellung des DataFrames gebraucht

p_values_scenarios_day_model[,1] <- NULL



## Für das Zeitmodell

# DataFrame erstellen mit 8 Zeilen, da 8 Kovariablen

p_values_scenarios_time_model <- data.frame(1:8)

# Für jedes Szenario Teiltabelle mit Koeffizienten, p-Werten und Signifikanzen 
# erstellen

i <- 0

for (n in 1:length(scenarios_time_model)) {
  
  i = i + 1
  
  p_names <- row.names(scenarios_time_model[[i]]$summary$p.table)
  
  s_names <- row.names(scenarios_time_model[[i]]$summary$s.table)
  
  coefficients <- c(format(c(plogis(
    scenarios_time_model[[i]]$summary$p.coeff[1]), plogis(
      scenarios_time_model[[i]]$summary$p.coeff[1] +
        scenarios_time_model[[i]]$summary$p.coeff[2])), digits = 3),
    rep("x", length(s_names)))
  
  p_values <- c(scenarios_time_model[[i]]$summary$p.pv,
                scenarios_time_model[[i]]$summary$s.pv)
  
  stars <- stars.pval(p_values)
  
  p_values <- format.pval(p_values, digits = 1)
  
  p_values_scenarios_time_model <- p_values_scenarios_time_model %>% 
    cbind(coefficients, p_values, stars)
  
  names(p_values_scenarios_time_model)[(2:4)+3*(i-1)] <- 
    c(paste0("coeff (", scenario_names[n], ")"), 
      paste0("p (", scenario_names[n], ")"), " ")
  
  if (i == 1) { 
    row.names(p_values_scenarios_time_model) <- c(p_names, s_names) 
  }
  
}

# erste Reihe löschen, da nur zur Erstellung des DataFrames gebraucht

p_values_scenarios_time_model[,1] <- NULL
