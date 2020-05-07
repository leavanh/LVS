## theme setzen (ändert nur die Ästhetik der Plots, nichts inhaltliches)

theme_set(theme_minimal())

# Liste erstellen

# addmargins() fügt die Summenspalte bzw -Reihe hinzu

summary_list <- list(
  
  # Übersicht
  summary(date_data),
  
  # Typ und Position
  addmargins(table(data$lvs, data$position)),
  # -> mehr Messungen bei S
  
  # Typ und Wochentag
  addmargins(table(data$lvs, data$day))
  # -> am wenigsten Messungen Montags, am meisten am Wochenende
)

## Plot

# alle Variablen gegeinander geplottet zur Übersicht
# keine Faktorvariablen und ohne Zeit
# NAs (Tage an denen Messungen nicht möglich waren) werden davor entfernt

date_data_plot <- date_data_noNA[,
                            c("date", "count_people",
                                       "ratio", "snowhight", "snow_diff", 
                                       "temperature", "solar_radiation",
                                       "avalanche_report", "sunrise")] %>%
                  ggpairs()
# -> day_length hängt vollkommen von date ab (Achtung beim Modell!)

## Datum

# Datum und absolute Häufigkeit der Messungen
# NAs (Tage an denen Messungen nicht möglich waren) werden davor entfernt

date_lvs <- date_data_noNA %>%
  ggplot(aes(date)) +
  geom_col(aes(y = count_people, fill = "red")) +
  geom_col(aes(y = lvs_true, fill = "blue")) +
  scale_fill_identity(name = "Messung",
                       breaks = c("red", "blue"),
                       labels = c("Person", "LVS-Gerät"),
                       guide = "legend") +
  labs(title = "Die Messungen in der Wintersaison 18/19",
       x = "Datum",
       y = "Absolute Häufigkeit")

# Datum und Ratio
# NAs (Tage an denen Messungen nicht möglich waren) werden davor entfernt

date_ratio <- ggplot(date_data) +
  geom_line(aes(date, ratio)) +
  labs(title = "Anteil der Personen mit LVS-Gerät in der Wintersaison 18/19",
       x = "Datum",
       y = "Anteil LVS-Geräte")

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
  ylab("Sonneneinstrahlung in W/m²")

# Datum und Position

date_position <- ggplot(data[!is.na(data$position),]) +
  geom_bar(aes(date, fill = position), position = "dodge") +
  scale_fill_manual(values = c("black", "orange"), 
                    name = "Position",
                    breaks = c("N", "S"),
                    labels = c("Nord", "Süd")) + 
  labs(title = "Die Messungen nach Position und Datum",
       x = "Datum",
       y = "Absolute Häufigkeit")

## Ratio
# NAs (Tage an denen Messungen nicht möglich waren) werden davor entfernt

# Ratio und Schneehöhe

snowhight_ratio <- date_data_noNA %>%
  ggplot() +
  geom_point(aes(snowhight, ratio), alpha = 0.5) +
  xlab("Schneehöhe (in cm)") +
  ylab("Anteil LVS-Geräte")

# Ratio und Temperatur

temperature_ratio <- date_data_noNA %>%
  ggplot() +
  geom_point(aes(temperature, ratio), alpha = 0.5) +
  xlab("Temperatur (in °C)") +
  ylab("Anteil LVS-Geräte")

# Ratio und solar radiation

solar_radiation_ratio <- date_data_noNA %>%
  ggplot() +
  geom_point(aes(solar_radiation, ratio), alpha = 0.5) +
  xlab("Sonneneinstrahlung in W/m²") +
  ylab("Anteil LVS-Geräte")

# Ratio und Lawinenwarnstufe

avalanche_ratio <- date_data_noNA %>%
  ggplot() +
  geom_jitter(aes(avalanche_report, ratio), alpha = 0.5) + 
  labs(x = "Lawinenwarnstufe",
       y = "Anteil LVS-Geräte")

## Schneehöhe und radiation

snowhight_solar_radiation <- ggplot(date_data) +
  geom_point(aes(snowhight, solar_radiation), alpha = 0.5) +
  xlab("Schneehöhe (in cm)") +
  ylab("solar radiation")

## Uhrzeit
# NAs (Tage an denen Messungen nicht möglich waren) werden entfernt

time_lvs <- ggplot() +
  geom_freqpoly(data = data[!is.na(data$time),],
                aes(time, colour = "red"), binwidth = 15) +
  geom_freqpoly(data = subset(data[!is.na(data$time),],
                              lvs == TRUE),
                aes(time, colour = "blue"), binwidth = 15) +
  scale_color_identity(name = "Messung",
                       breaks = c("red", "blue"),
                       labels = c("Person", "Lvs-Gerät"),
                       guide = "legend") + 
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M") +
  labs(title = "Die Messungen nach Uhrzeit",
       x = "Uhrzeit",
       y = "Absolute Häufigkeit")


## Solar Radiation Maximum

solar_radiation_max <- ggplot(date_data, aes(x = as.numeric(date))) +
  geom_line(aes(y = solar_radiation)) +
  geom_spline(aes(y = solar_radiation_max, 
                  colour = "geglättetes Maximum"),
              nknots = 30,
              spar = 0.1, size = 1.5) +
  labs(title = "Verlauf der Sonneneinstrahlung 18/19",
       x = "",
       y = "Sonneneinstrahlung in W/m²") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(.2, .9)) +
  scale_x_continuous(breaks = c(1546300800, 1548979200, 
                              1551398400, 1554163200), 
                   labels = c("1546300800" = "1. Januar",
                              "1548979200" = "1. Februar",
                              "1551398400" = "1. März",
                              "1554163200" = "1. April"))
