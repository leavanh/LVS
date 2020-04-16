# Date_model.R bis gam.check() durchlaufen lassen
type <- "deviance"  ## "pearson" & "response" are other valid choices
resid <- residuals(date_model, type = type)
linpred <- napredict(date_model$na.action, date_model$linear.predictors)
observed.y <- napredict(date_model$na.action, date_model$y)


# Grafik extrahieren
plot(fitted(date_model), observed.y, xlab = "Fitted Values", 
     ylab = "Response", main = "Response vs. Fitted Values")

# Zeigt Ausreisser mit x und y value
sorting_order_x_array = sort.list(fitted(date_model))
ordered_x_array = fitted(date_model)[sort.list(fitted(date_model))]
ordered_y_array = observed.y[sort.list(fitted(date_model))]
deviation_threshold = 35   #D.h. wird als Ausreisser gesehen, wenn mehr als 35 % vom Nachbarpunkt entfernt
                            #deviation_threshold, kann beliebig abgeändert werden 
last_y_value = 0
elem_counter = 0
for(iter in 1:length(sorting_order_x_array))
{
  current_y_value = ordered_y_array[iter]
  curren_x_value = ordered_x_array[iter]
  current_index = match(curren_x_value, fitted(date_model))
  #cat("Last value = ", last_y_value, "current value = ", current_y_value ,"\n")
  current_index_new = sorting_order_x_array[iter]
  if (abs(current_y_value - last_y_value) > (abs(last_y_value) * deviation_threshold / 100))
  {
    if ((curren_x_value > 0.1) && curren_x_value < 0.2) #Das betrachtete Intervall auf der x-Achse
    {
    cat("Exceeded sample, y value = ", current_y_value, "; x value = ", curren_x_value, "index = ", current_index, "\n")
    cat("Proper index = ", current_index_new, "\n")
    }
    
  }
  last_y_value = current_y_value
}

  

#----------------------------------------------------------------------------------------------------

## Day_model, Response vs. Fitted Values (1 Möglichkeit)
# Day_model.R bis gam.check() durchlaufen lassen
type <- "deviance"  ## "pearson" & "response" are other valid choices
resid <- residuals(day_model, type = type)
linpred <- napredict(day_model$na.action, day_model$linear.predictors)
observed.y <- napredict(day_model$na.action, day_model$y)

# Grafik extrahieren
plot(fitted(day_model), observed.y, xlab = "Fitted Values", 
     ylab = "Response", main = "Response vs. Fitted Values")

# Zeigt Ausreisser mit x und y value 
sorting_order_x_array = sort.list(fitted(day_model))
ordered_x_array = fitted(day_model)[sort.list(fitted(day_model))]
ordered_y_array = observed.y[sort.list(fitted(day_model))]
deviation_threshold = 35   #D.h. wird als Ausreisser gesehen, wenn mehr als 35 % vom Nachbarpunkt entfernt
#deviation_threshold, kann beliebig abgeändert werden 
last_y_value = 0
elem_counter = 0
for(iter in 1:length(sorting_order_x_array))
{
  current_y_value = ordered_y_array[iter]
  curren_x_value = ordered_x_array[iter]
  current_index = match(curren_x_value, fitted(day_model))
  #cat("Last value = ", last_y_value, "current value = ", current_y_value ,"\n")
  current_index_new = sorting_order_x_array[iter]
  if (abs(current_y_value - last_y_value) > (abs(last_y_value) * deviation_threshold / 100))
  {
    if ((curren_x_value > 0.0) && curren_x_value < 0.1)
    {
    cat("Exceeded sample, y value = ", current_y_value, "; x value = ", curren_x_value, "index = ", current_index, "\n")
    cat("Proper index = ", current_index_new, "\n")
      }
    
  }
  last_y_value = current_y_value
}



## Unwichtig, hier nur Anzahl der Beobachtungen überprüft
which(ordered_y_array %in% 1) #zeigt den Index der GEORDNETEN Liste für y=1
ordered_x_array[which(ordered_y_array %in% 1)[28]] #zeigt den x-Wert, ich nehme das 3te Element aus geordneter Liste
sorting_order_x_array[28] #zeigt den tatsächlichen Index

### Day_model, Resids vs. linear pred.
sorting_order_x_array = sort.list(linpred)
ordered_x_array = linpred[sort.list(linpred)]
ordered_y_array = resid[sort.list((linpred))]
deviation_threshold = 35   #D.h. wird als Ausreisser gesehen, wenn mehr als 35 % vom Nachbarpunkt entfernt
#deviation_threshold, kann beliebig abgeändert werden 
last_y_value = 0
elem_counter = 0
for(iter in 1:length(sorting_order_x_array))
{
  current_y_value = ordered_y_array[iter]
  curren_x_value = ordered_x_array[iter]
  current_index = match(curren_x_value, (linpred))
  #cat("Last value = ", last_y_value, "current value = ", current_y_value ,"\n")
  current_index_new = sorting_order_x_array[iter]
  if (abs(current_y_value - last_y_value) > (abs(last_y_value) * deviation_threshold / 100))
  {
    if ((curren_x_value > -6) && curren_x_value < -5)  #Intervall auf x-Achse, welches betrachtet wird
    {
    cat("Exceeded sample, y value = ", current_y_value, "; x value = ", curren_x_value, "index = ", current_index, "\n")
    cat("Proper index = ", current_index_new, "\n")
    }
    
  }
  last_y_value = current_y_value
}


#---------------------------------------------------------------------------------------------------------
## Grafiken zu date_model.R geplottet
#Pakete
if (!require("mgcViz")) install.packages("mgcViz")
library("mgcViz")
if (!require("mgcv")) install.packages("mgcv")
library("mgcv")

#Effekt-spezifische Plots, Smooth-Effekte mit 95% KI
date_model <- getViz(date_model)

print(plot(date_model, allTerms = T), pages = 1)


b <- plot(date_model, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + theme_get() + labs(title = NULL)
b


# Plot b auf page=1 zusammengefügt
b$empty # Muss FALSE ergeben 

print(b, pages = 1)

