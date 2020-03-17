## Übersicht

# Vorabberechnungen für die Übersicht der Art der Tage

# Absolute Anzahl der Beacons und Infrared, jeweils für Wochentage,
# Wochenendtage und Ferientage

weekday_sum <- c(sum(subset(lvs_date_data, day_weekday == TRUE)$lvs_true),
                 sum(subset(lvs_date_data, day_weekday == TRUE)$lvs_false))
weekend_sum <- c(sum(subset(lvs_date_data, day_weekend == TRUE)$lvs_true),
                 sum(subset(lvs_date_data, day_weekend == TRUE)$lvs_false))
holiday_sum <- c(sum(subset(lvs_date_data, holiday == TRUE)$lvs_true),
                 sum(subset(lvs_date_data, holiday == TRUE)$lvs_false))

# In einem data.frame zusammenführen und Reihen und Spalten benennen

day_type_count <- data.frame(weekday_sum, weekend_sum, holiday_sum)

rownames(day_type_count) <- c("LVS: True", "LVS: False")
colnames(day_type_count) <- c("weekday", "weekend", "holiday")

# Liste erstellen

# addmargins() fügt die Summenspalte bzw -Reihe hinzu

summary_list <- list(
  
  # Übersicht
  summary(lvs_data),
  
  # Typ und Position
  addmargins(table(lvs_data$lvs, lvs_data$position, useNA = "ifany")),
  # -> mehr Messungen bei S
  
  # Typ und Wochentag
  addmargins(table(lvs_data$lvs, lvs_data$day, useNA = "ifany")),
  # -> am wenigsten Messungen Montags, am meisten am Wochenende
  
  # Welche avalanche_report_comment gibt es?
  table(lvs_data$avalanche_report_comment, useNA = "ifany"),
  
  # Typ und Art des Tages (Summenspalte händisch hinzugefügt)
  rbind(day_type_count, Total = c(sum(day_type_count$weekday),
                                  sum(day_type_count$weekend),
                                  sum(day_type_count$holiday)))
)

## Plot

# alle Variablen gegeinander geplottet zur Übersicht
# keine Faktorvariablen und ohne Zeit

lvs_date_data_plot <- ggpairs(lvs_date_data[,c("date", "lvs_true", "lvs_false",
                                       "ratio", "snowhight", 
                                       "temperature", "solar_radiation",
                                       "day_length")])
# -> day_length hängt vollkommen von date ab (Achtung beim Modell!)

## Datum

# Datum und absolute Häufigkeit nach Typ

date_type <- ggplot(date_data, aes(date)) +
  geom_col(aes(y = count_infrared, fill = "red")) +
  geom_col(aes(y = count_beacon, fill = "blue")) +
  scale_y_continuous(limits = c(0, 1000)) +
  scale_fill_identity(name = "Messung",
                       breaks = c("red", "blue"),
                       labels = c("Infrarot", "Beacon"),
                       guide = "legend") + 
  labs(title = "Messungen vor dem Umcodieren",
       x = "Datum",
       y = "Absolute Häufigkeit")

# Datum und lvs-Gerät

date_lvs <- ggplot(lvs_date_data, aes(date)) +
  geom_col(aes(y = count_people, fill = "red")) +
  geom_col(aes(y = lvs_true, fill = "blue")) +
  scale_y_continuous(limits = c(0, 1000)) +
  scale_fill_identity(name = "Messung",
                       breaks = c("red", "blue"),
                       labels = c("Person", "Lvs-Gerät"),
                       guide = "legend") + 
  labs(title = "Messungen nach dem Umcodieren",
       x = "Datum",
       y = "Absolute Häufigkeit")

# Wie viel verändert sich durch das Umcodieren?

# neuen Datensatz zusammenführen

date_diff_data <- date_data %>%
                    right_join(lvs_date_data, by = "date") %>%
                    select(date, count_infrared, count_people) %>%
                    mutate(diff = count_people - count_infrared)

# Plotten

date_diff_plot <- ggplot(date_diff_data) +
  geom_col(aes(date, diff)) + 
  labs(title = "Unterschätzung der Personen vor dem Umcodieren",
       x = "Datum",
       y = "Absolute Häufigkeit")
  

# Datum und Ratio (vor dem Umcodieren)

date_ratio <- ggplot(date_data) +
  geom_line(aes(date, ratio)) +
  labs(title = "Anteil vor dem Umcodieren",
       x = "Datum",
       y = "Anteil")
# -> hohe Schwankung am Anfang, wie zu erklären?
# durch das Umcodieren teilweise behoben

# Datum und Ratio (nach dem Umcodieren)

lvs_date_ratio <- ggplot(lvs_date_data) +
  geom_line(aes(date, ratio)) +
  labs(title = "Anteil nach dem Umcodieren",
       x = "Datum",
       y = "Anteil")
# -> hohe Schwankung am Anfang, wie zu erklären?

# Datum und Schneehöhe

date_snowhight <- ggplot(lvs_date_data) +
  geom_line(aes(date, snowhight)) +
  xlab("Datum") +
  ylab("Schneehöhe (in cm)")

# Datum und Temperatur

date_temperature <- ggplot(lvs_date_data) +
  geom_line(aes(date, temperature)) +
  xlab("Datum") +
  ylab("Temperatur (in °C)")

# Datum und solar radiation

date_solar_radiation <- ggplot(lvs_date_data) +
  geom_line(aes(date, solar_radiation)) +
  xlab("Datum") +
  ylab("solar radiation")

## Ratio

# Ratio und Schneehöhe

snowhight_ratio <- ggplot(date_data) +
  geom_point(aes(snowhight, ratio), alpha = 0.5) +
  xlab("Schneehöhe (in cm)") +
  ylab("Ratio")

# Ratio und Temperatur

temperature_ratio <- ggplot(date_data) +
  geom_point(aes(temperature, ratio), alpha = 0.5) +
  xlab("Temperatur (in °C)") +
  ylab("Ratio")

# Ratio und solar radiation

solar_radiation_ratio <- ggplot(date_data) +
  geom_point(aes(solar_radiation, ratio), alpha = 0.5) +
  xlab("solar radiation") +
  ylab("Ratio")

## Schneehöhe und radiation

snowhight_solar_radiation <- ggplot(lvs_date_data) +
  geom_point(aes(snowhight, solar_radiation), alpha = 0.5) +
  xlab("Schneehöhe (in cm)") +
  ylab("solar radiation")

## Uhrzeit

# vor der Umcodierung

time_type <- ggplot() +
  geom_freqpoly(data = subset(data, type == "Infrared"),
                aes(time, colour = "red"), binwidth = 15) +
  geom_freqpoly(data = subset(data, type == "Beacon"),
                aes(time, colour = "blue"), binwidth = 15) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_identity(name = "Messung",
                       breaks = c("red", "blue"),
                       labels = c("Infrarot", "Beacon"),
                       guide = "legend") + 
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M") +
  labs(title = "Messungen vor dem Umcodieren",
       x = "Uhrzeit",
       y = "Absolute Häufigkeit")

# nach der Umcodierung

time_lvs <- ggplot() +
  geom_freqpoly(data = lvs_data,
                aes(time, colour = "red"), binwidth = 15) +
  geom_freqpoly(data = subset(lvs_data, lvs == TRUE),
                aes(time, colour = "blue"), binwidth = 15) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_identity(name = "Messung",
                       breaks = c("red", "blue"),
                       labels = c("Person", "Lvs-Gerät"),
                       guide = "legend") + 
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M") +
  labs(title = "Messungen nach dem Umcodieren",
       x = "Uhrzeit",
       y = "Absolute Häufigkeit")

## Lawinen

# Daten umformen

avalanche_data <- lvs_date_data %>%
  select(ratio, avalanche_report_down, 
         avalanche_report_top) %>%
  gather("position", "avalanche_risk",
         -c("ratio"))

# Plotten

# Unten und Oben

avalanche_position_plot <- ggplot(avalanche_data) +
  geom_boxplot(aes(avalanche_risk, ratio,
                   colour = position)) +
  scale_color_manual(values = c("green", "orange"), 
                     name = "Messung",
                     breaks = c("avalanche_report_down",
                                "avalanche_report_top"),
                     labels = c("Unten", "Oben")) + 
  labs(title = "Anteil der Mitnahme von LVS\nnach Lawinenwarnstufe",
       x = "Lawinenwarnstufe",
       y = "Anteil LVS-Geräte")

# Durchschnittswert

avalanche_mean_plot <- ggplot(lvs_date_data) +
  geom_boxplot(aes(avalanche_report, ratio)) + 
  labs(title = 
         "Anteil der Mitnahme von LVS\nnach Durchschnitts-Lawinenwarnstufe",
       x = "Lawinenwarnstufe",
       y = "Anteil LVS-Geräte")

