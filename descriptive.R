# alle Variablen gegeinander geplottet zur Übersicht
# keine Faktorvariablen und ohne Zeit
# NAs (Tage an denen Messungen nicht möglich waren) werden davor entfernt

date_data_plot <- date_data_noNA[,
                            c("date", "count_people",
                                       "ratio", "snow_diff", 
                                       "temperature", "solar_radiation",
                                       "avalanche_report",
                              "cloud_cover_daily")] %>%
                  ggpairs()

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
  labs(title = "Checkpointerfassungen in der Wintersaison 18/19 nach Datum",
       x = "Datum",
       y = "Absolute Häufigkeit")

# Datum und Ratio
# NAs (Tage an denen Messungen nicht möglich waren) werden davor entfernt

date_ratio <- ggplot(date_data) +
  geom_line(aes(int_date, ratio), color  = "darkgreen") +
  labs(title = "Anteil der Personen mit LVS-Gerät in der Wintersaison 18/19",
       x = "Datum",
       y = "Anteil LVS-Geräte") +
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01.Jan",
                                "01.Feb",
                                "01.Mar",
                                "01.Apr")) +
  scale_y_continuous(limits = c(0, 1))

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
                                "01.Apr"))

# Datum und Schneedifferenz

date_snowdiff <- ggplot(date_data) +
  geom_line(aes(int_date, snow_diff)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  xlab("Datum") +
  ylab("Schneedifferenz zum Vortag(in cm)") +
  scale_y_continuous(limits = c(-15, 45)) +
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01.Jan",
                                "01.Feb",
                                "01.Mar",
                                "01.Apr"))

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
                                "01.Apr"))

# Datum und Bewölkung

date_cloud_cover <- ggplot(date_data) +
  geom_line(aes(int_date, cloud_cover_daily)) +
  xlab("Datum") +
  ylab("Bewölkung in %") +
  scale_x_continuous(breaks = c(17897,17928,17956,17987), 
                     labels = c("01.Jan",
                                "01.Feb",
                                "01.Mar",
                                "01.Apr"))

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

# Ratio und Bewölkung

cloud_cover_ratio <- date_data_noNA %>%
  ggplot() +
  geom_point(aes(cloud_cover_daily, ratio), alpha = 0.5) +
  xlab("Bewölkung in %") +
  ylab("Anteil LVS-Geräte")

# Ratio und Lawinenwarnstufe

avalanche_ratio <- date_data_noNA %>%
  ggplot() +
  geom_jitter(aes(avalanche_report, ratio), alpha = 0.5) + 
  labs(x = "Lawinenwarnstufe",
       y = "Anteil LVS-Geräte")

## andere Variablen

# Bewölkung

boxplot_cloud_cover <- date_data_noNA %>%
  ggplot() +
  geom_boxplot(aes(y = cloud_cover_daily)) +
  labs(x = "Bewölkung",
       y = "durchschnittliche Bewölkung pro Tag (in %)") +
  theme(axis.text.x = element_blank(), # Entfernt unnötige Zahlen auf der x-Achse
        axis.ticks.x = element_blank())

# Lawinenwarnstufe

boxplot_avalanche_report <- date_data_noNA %>%
  ggplot() +
  geom_boxplot(aes(y = avalanche_report)) +
  labs(x = "Lawinenwarnstufe",
       y = "Lawinenwarnstufe jedes Tages") +
  theme(axis.text.x = element_blank(), # Entfernt unnötige Zahlen auf der x-Achse
        axis.ticks.x = element_blank())

# Feiertag

holiday_plot <- data_noNA %>%
  ggplot() +
  geom_bar(aes(x = holiday, fill = lvs))  +
  scale_fill_manual(name = " ",
                      values = c("#F0E442", "darkgreen"),
                      labels = c("Person ohne \n LVS-Gerät", "Person mit \n LVS-Gerät"),
                      guide = "legend") +
  labs(x = "Ferientag",
       y = "Absolute Häufigkeit")

# Wochentag

day_plot <- data_noNA %>%
  ggplot() +
  geom_bar(aes(x = day, fill = lvs))  +
  scale_fill_manual(name = " ",
                    values = c("#F0E442", "darkgreen"),
                    labels = c("Person ohne \n LVS-Gerät", "Person mit \n LVS-Gerät"),
                    guide = "legend") +
  labs(x = "Wochentag",
       y = "Absolute Häufigkeit")


## Uhrzeit
# NAs (Tage an denen Messungen nicht möglich waren) werden entfernt

data_time_lvs_plot <- data[!is.na(data$time),] %>%
  mutate(time = as.POSIXct(
    strftime(
      time, format = "%H:%M:%S"), 
    format = "%H:%M:%S", tz = "GMT"))
date(data_time_lvs_plot$time) <- as.POSIXct("1899-12-31", tz = "GMT")

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
  labs(title = "Die Messungen nach Uhrzeit",
       x = "Uhrzeit",
       y = "Absolute Häufigkeit") +
  geom_vline(xintercept = as.POSIXct("1899-12-31 04:00:00", tz = "GMT"))

