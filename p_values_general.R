
## Hier wird für beide Modelle jeweils eine Tabelle erstellt, die p-Werte und
## (falls sinnvoll) Koeffizienten anzeigt, je nach Anteil hinzugefügter 
## Beobachtungen zum Datensatz


# Für das Tagesmodell

# DataFrame erstellen mit 8 Zeilen, da 8 Kovariablen

p_values_general_day_model <- data.frame(1:8)

# Für jeden Anteil an neuen Beobachtungen Teiltabelle mit Koeffizienten, 
# p-Werten und Signifikanzen erstellen

i <- 0

for (n in c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) {
  
  i = i + 1
  
  p_names <- row.names(general_day_model[[i]]$summary$p.table)
  
  s_names <- row.names(general_day_model[[i]]$summary$s.table)
  
  coefficients <- c(format(c(plogis(
    general_day_model[[i]]$summary$p.coeff[1]), plogis(
      general_day_model[[i]]$summary$p.coeff[1] +
        general_day_model[[i]]$summary$p.coeff[2])), digits = 3),
    rep("x", length(s_names)))
  
  p_values <- c(general_day_model[[i]]$summary$p.pv,
                general_day_model[[i]]$summary$s.pv)
  
  stars <- stars.pval(p_values)
  
  p_values <- format.pval(p_values, digits = 1)
  
  p_values_general_day_model <- p_values_general_day_model %>% 
    cbind(coefficients, p_values, stars)
  
  names(p_values_general_day_model)[(2:4)+3*(i-1)] <- 
    c(paste0("coeff (", n, ")"), paste0("p (", n, ")"), " ")
  
  if (i == 1) { 
    row.names(p_values_general_day_model) <- c(p_names, s_names) 
  }
  
}

# erste Reihe löschen, da nur zur Erstellung des DataFrames gebraucht

p_values_general_day_model[,1] <- NULL


## Für das Zeitmodell

p_values_general_time_model <- data.frame(1:8)

# Für jeden Anteil an neuen Beobachtungen Teiltabelle mit Koeffizienten, 
# p-Werten und Signifikanzen erstellen

i <- 0

for (n in c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) {
  
  i = i + 1
  
  p_names <- row.names(general_time_model[[i]]$summary$p.table)
  
  s_names <- row.names(general_time_model[[i]]$summary$s.table)
  
  coefficients <- c(format(c(plogis(
    general_time_model[[i]]$summary$p.coeff[1]), plogis(
      general_time_model[[i]]$summary$p.coeff[1] +
        general_time_model[[i]]$summary$p.coeff[2])), digits = 3),
    rep("x", length(s_names)))
  
  p_values <- c(general_time_model[[i]]$summary$p.pv,
                general_time_model[[i]]$summary$s.pv)
  
  stars <- stars.pval(p_values)
  
  p_values <- format.pval(p_values, digits = 1)
  
  p_values_general_time_model <- p_values_general_time_model %>% 
    cbind(coefficients, p_values, stars)
  
  names(p_values_general_time_model)[(2:4)+3*(i-1)] <- 
    c(paste0("coeff (", n, ")"), paste0("p (", n, ")"), " ")
  
  if (i == 1) { 
    row.names(p_values_general_time_model) <- c(p_names, s_names) 
  }
  
}


p_values_general_time_model[,1] <- NULL
