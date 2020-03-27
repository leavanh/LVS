## Übersicht

# Vorabberechnungen für die Übersicht der Art der Tage

# Absolute Anzahl der Beacons und Infrared, jeweils für Wochentage,
# Wochenendtage und Ferientage

weekday_sum <- c(sum(subset(date_data, day_weekday == TRUE)$lvs_true),
                 sum(subset(date_data, day_weekday == TRUE)$lvs_false))
weekend_sum <- c(sum(subset(date_data, day_weekend == TRUE)$lvs_true),
                 sum(subset(date_data, day_weekend == TRUE)$lvs_false))
holiday_sum <- c(sum(subset(date_data, holiday == TRUE)$lvs_true),
                 sum(subset(date_data, holiday == TRUE)$lvs_false))

# In einem data.frame zusammenführen und Reihen und Spalten benennen

day_type_count <- data.frame(weekday_sum, weekend_sum, holiday_sum)

rownames(day_type_count) <- c("LVS: True", "LVS: False")
colnames(day_type_count) <- c("weekday", "weekend", "holiday")

# Liste erstellen

# addmargins() fügt die Summenspalte bzw -Reihe hinzu

summary_list <- list(
  
  # Übersicht
  summary(date_data),
  
  # Typ und Position
  addmargins(table(data$type, data$position, useNA = "ifany")),
  # -> mehr Messungen bei S
  
  # Typ und Wochentag
  addmargins(table(data$type, data$day, useNA = "ifany")),
  # -> am wenigsten Messungen Montags, am meisten am Wochenende
  
  # Typ und Art des Tages (Summenspalte händisch hinzugefügt)
  rbind(day_type_count, Total = c(sum(day_type_count$weekday),
                                  sum(day_type_count$weekend),
                                  sum(day_type_count$holiday)))
)

## Plot

# alle Variablen gegeinander geplottet zur Übersicht
# keine Faktorvariablen und ohne Zeit

date_data_plot <- ggpairs(date_data[,c("date", "count_people",
                                       "ratio", "snowhight", 
                                       "temperature", "solar_radiation",
                                       "avalanche_report", "day_length")])
# -> day_length hängt vollkommen von date ab (Achtung beim Modell!)

## Datum

# Datum und absolute Häufigkeit der Messungen

# warning message verstehen

date_type <- ggplot(date_data, aes(date)) +
  geom_col(aes(y = count_people, fill = "red")) +
  geom_col(aes(y = lvs_true, fill = "blue")) +
  scale_y_continuous(limits = c(0, 1000)) +
  scale_fill_identity(name = "Messung",
                       breaks = c("red", "blue"),
                       labels = c("Person", "LVS-Gerät"),
                       guide = "legend") +
  labs(title = "Die Messungen in der Wintersaison 18/19",
       x = "Datum",
       y = "Absolute Häufigkeit")

# Datum und Ratio

date_ratio <- ggplot(date_data) +
  geom_line(aes(date, ratio)) +
  labs(title = "Anteil der Personen mit LVS-Gerät in der Wintersaison 18/19",
       x = "Datum",
       y = "Anteil LVS-Geräte")
# -> hohe Schwankung am Anfang, wie zu erklären?

# Datum und Schneehöhe

date_snowhight <- ggplot(date_data) +
  geom_line(aes(date, snowhight)) +
  xlab("Datum") +
  ylab("Schneehöhe (in cm)")

# Datum und Temperatur

date_temperature <- ggplot(date_data) +
  geom_line(aes(date, temperature)) +
  xlab("Datum") +
  ylab("Temperatur (in °C)")

# Datum und solar radiation

date_solar_radiation <- ggplot(date_data) +
  geom_line(aes(date, solar_radiation)) +
  xlab("Datum") +
  ylab("solar radiation")

## Ratio

# Ratio und Schneehöhe

snowhight_ratio <- ggplot(date_data) +
  geom_point(aes(snowhight, ratio), alpha = 0.5) +
  xlab("Schneehöhe (in cm)") +
  ylab("Anteil LVS-Geräte")

# Ratio und Temperatur

temperature_ratio <- ggplot(date_data) +
  geom_point(aes(temperature, ratio), alpha = 0.5) +
  xlab("Temperatur (in °C)") +
  ylab("Anteil LVS-Geräte")

# Ratio und solar radiation

solar_radiation_ratio <- ggplot(date_data) +
  geom_point(aes(solar_radiation, ratio), alpha = 0.5) +
  xlab("solar radiation") +
  ylab("Anteil LVS-Geräte")

# Ratio und Lawinenwarnstufe

avalanche_ratio <- ggplot(date_data) +
  geom_jitter(aes(avalanche_report, ratio), alpha = 0.5) + 
  labs(x = "Lawinenwarnstufe",
       y = "Anteil LVS-Geräte")

## Schneehöhe und radiation

snowhight_solar_radiation <- ggplot(date_data) +
  geom_point(aes(snowhight, solar_radiation), alpha = 0.5) +
  xlab("Schneehöhe (in cm)") +
  ylab("solar radiation")

## Uhrzeit

time_type <- ggplot() +
  geom_freqpoly(data = data,
                aes(time, colour = "red"), binwidth = 15) +
  geom_freqpoly(data = subset(data, type == "Beacon"),
                aes(time, colour = "blue"), binwidth = 15) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_identity(name = "Messung",
                       breaks = c("red", "blue"),
                       labels = c("Person", "Lvs-Gerät"),
                       guide = "legend") + 
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M") +
  labs(title = "Die Messungen nach Uhrzeit",
       x = "Uhrzeit",
       y = "Absolute Häufigkeit")
