
## Tabelle mit den Koeffizienten und p-Werten des jeweiligen Modells für
## unterschiedlichen Anteil an neu hinzugefügten Beobachtungen 

# Tabelle mit Termnamen aus dem Modell


# Für das Date Model


p_values_general <- data.frame(1:8)

# Für jeden Anteil an neuen Beobachtungen Teiltabelle mit Koeffizienten, 
# p-Werten und Signifikanzen erstellen

i <- 0

for (n in c(0.1, 0.2, 0.25, 0.3, 0.4)) {
  
  # Modell berechnen
  
  i = i + 1

  date_data_model_n <- data_general_function(n)$date_data
  
  p_names <- row.names(date_model_function(date_data_model_n)$summary$p.table)
                       
  s_names <- row.names(date_model_function(date_data_model_n)$summary$s.table)

  coefficients <- c(format(plogis(
    date_model_function(date_data_model_n)$summary$p.coeff), digits = 3),
                    rep("x", length(s_names)))
  
  p_values <- c(date_model_function(date_data_model_n)$summary$p.pv,
                       date_model_function(date_data_model_n)$summary$s.pv)
  
  stars <- stars.pval(p_values)
  
  p_values <- format.pval(p_values)
  
  p_values_general <- p_values_general %>% 
                        cbind(coefficients, p_values, stars)

  names(p_values_general)[(2:4)+3*(i-1)] <- 
    c(paste0("coeff (", n, ")"), paste0("p (", n, ")"), " ")
  
  if (i == 1) { 
    row.names(p_values_general) <- c(p_names, s_names) 
    }
  

}


p_values_general[,1] <- NULL
p_values_general

class(p_values_general)

write.xlsx(x = p_values_general, file = "p_values_general.xlsx")

names(date_model_function(date_data_model_n)$summary$p.coeff)

# Für das Day Model

p_values_general <- 
  data.frame(variable = names(day_model_function(test$data)$summary$chi.sq))

for (i in c(0.1, 0.2, 0.25, 0.5)) {
  
  day_data_model_n <- data_general_function(i)$data
  
  p_values_general <- p_values_general %>% 
    cbind(c(day_model_function(day_data_model_n)$summary$p.pv,
            day_model_function(day_data_model_n)$summary$s.pv))
  
}




names(p_values_general) <- c("variable", c("0.1", "0.25"))
p_values_general




plots_date_model()








p_values_general<0.05

set.seed(42)
test <- data_general_function(0.25)

str(date_model_function(test$date_data))

date_model_function(test$date_data)$summary$s.table

str(date_model_function(test$date_data)$summary)

str(date_model_function(test$date_data)$model)
