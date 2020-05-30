
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
  ggplot(aes(int_date)) +
  geom_col(aes(y = count_people, fill = "#F0E442")) +
  geom_col(aes(y = lvs_true, fill = "darkgreen")) +
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01.Jan",
                                "01.Feb",
                                "01.Mar",
                                "01.Apr")) +
  scale_fill_identity(name = " ",
                      breaks = c("#F0E442", "darkgreen"),
                      labels = c("Person ohne \n LVS-Gerät", "Person mit \n LVS-Gerät"),
                      guide = "legend") +
  labs(#title = "Checkpointerfassungen in der Wintersaison 18/19 nach Datum",
       x = "Datum",
       y = "Absolute Häufigkeit") +
  theme(text = element_text(size = 10), legend.position="top")
  #theme(text = element_text(size = 20))

# Datum und Ratio
# NAs (Tage an denen Messungen nicht möglich waren) werden davor entfernt

date_ratio <- ggplot(date_data) +
  geom_line(aes(int_date, ratio)) +
  labs(#title = "Anteil der Personen mit LVS-Gerät in der Wintersaison 18/19",
       x = "Datum",
       y = "Anteil LVS-Geräte") +
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01.Jan",
                                "01.Feb",
                                "01.Mar",
                                "01.Apr")) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(text = element_text(size = 15), legend.position="top")

# Datum und Schneehöhe

date_snowhight <- ggplot(date_data) +
  geom_line(aes(int_date, snowhight)) +
  xlab("Datum") +
  ylab("Schneehöhe (in cm)") +
  scale_y_continuous(limits = c(0, 220)) +
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01.Jan",
                                "01.Feb",
                                "01.Mar",
                                "01.Apr")) +
  theme(text = element_text(size = 10), legend.position="top")

# Datum und Temperatur

date_temperature <- ggplot(date_data) +
  geom_line(aes(int_date, temperature)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  xlab("Datum") +
  ylab("Temperatur (in °C)") +
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01.Jan",
                                "01.Feb",
                                "01.Mar",
                                "01.Apr")) +
  theme(text = element_text(size = 10), legend.position="top")

# Datum und solar radiation

date_solar_radiation <- ggplot(date_data) +
  geom_line(aes(int_date, solar_radiation)) +
  xlab("Datum") +
  ylab("Sonneneinstrahlung (in W/m²)") +
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01.Jan",
                                "01.Feb",
                                "01.Mar",
                                "01.Apr")) +
  theme(text = element_text(size = 10), legend.position="top")

# Datum und Position

date_position <- ggplot(data[!is.na(data$position),]) +
  geom_bar(aes(int_date, fill = position), position = "dodge") +
  scale_fill_manual(values = c("black", "orange"), 
                    name = "Position",
                    breaks = c("N", "S"),
                    labels = c("Nord", "Süd")) + 
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01.Jan",
                                "01.Feb",
                                "01.Mar",
                                "01.Apr"))+
  labs(#title = "Die Messungen nach Position und Datum",
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
  xlab("Sonneneinstrahlung (in W/m²)") +
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

data_time_lvs_plot <- data[!is.na(data$time),] %>%
  mutate(time = as.POSIXct(
    strftime(
      time, format = "%H:%M:%S"), 
    format = "%H:%M:%S"))
date(data_time_lvs_plot$time) <- as.POSIXct("1899-12-31", tz = "MET")

time_lvs <- ggplot() +
  geom_freqpoly(data = data_time_lvs_plot,
                aes(time, colour = "#F0E442"), binwidth = 15) +
  geom_freqpoly(data = subset(data_time_lvs_plot, lvs == TRUE),
                aes(time, colour = "darkgreen"), binwidth = 15) +
  scale_color_identity(name = " ",
                       breaks = c("#F0E442", "darkgreen"),
                       labels = c("Person ohne \n LVS-Gerät", 
                                  "Person mit \n LVS-Gerät"),
                       guide = "legend") +
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M") +
  labs(#title = "Die Messungen nach Uhrzeit",
       x = "Uhrzeit",
       y = "Absolute Häufigkeit") + 
  theme(legend.position="top") +
  #geom_vline(xintercept = as.POSIXct("1899-12-31 04:00:00", tz = "MET")) +
  theme(text = element_text(size = 15), legend.position="top")


## Solar Radiation Maximum

solar_radiation_max <- ggplot(date_data, aes(x = as.numeric(date))) +
  geom_line(aes(y = solar_radiation)) +
  geom_spline(aes(y = solar_radiation_max, 
                  colour = "geglättetes Maximum"),
              nknots = 30,
              spar = 0.1, size = 1.5) +
  labs(#title = "Verlauf der Sonneneinstrahlung 18/19",
       x = "Datum",
       y = "Sonneneinstrahlung in W/m²") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(.2, .9)) +
  scale_x_continuous(breaks = c(1546300800, 1548979200, 
                              1551398400, 1554163200), 
                   labels = c("1546300800" = "01.Jan",
                              "1548979200" = "01.Feb",
                              "1551398400" = "01.Mar",
                              "1554163200" = "01.Apr")) +
  theme(text = element_text(size = 15))
