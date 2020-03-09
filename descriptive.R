## Übersicht

# Vorabberechnungen für die Übersicht der Art der Tage

weekday_sum <- c(sum(subset(date_data, day_weekday == TRUE)$count_beacon),
                 sum(subset(date_data, day_weekday == TRUE)$count_infrared))
weekend_sum <- c(sum(subset(date_data, day_weekend == TRUE)$count_beacon),
                 sum(subset(date_data, day_weekend == TRUE)$count_infrared))
holiday_sum <- c(sum(subset(date_data, holiday == TRUE)$count_beacon),
                 sum(subset(date_data, holiday == TRUE)$count_infrared))

day_type_count <- data.frame(weekday_sum, weekend_sum, holiday_sum)

rownames(day_type_count) <- c("Beacon", "Infrared")
colnames(day_type_count) <- c("weekday", "weekend", "holiday")

# Liste erstellen

summary_list <- list(
                  summary(data),
                  addmargins(table(data$type, data$position, useNA = "ifany")),
                  # -> mehr Messungen bei S
                  addmargins(table(data$type, data$day, useNA = "ifany")),
                  # -> am wenigsten Messungen Montags, am meisten am Wochenende
                  table(data$avalanche_report_comment, useNA = "ifany"),
                  rbind(day_type_count, Total = c(sum(day_type_count$weekday),
                                                  sum(day_type_count$weekend),
                                                  sum(day_type_count$holiday)))
                )

## Plot

# keine Faktorvariablen und ohne Zeit

date_data_plot <- ggpairs(date_data[,c(1, 4:9, 17)])
# -> sunhours hängt vollkommen von date ab (Achtung beim Modell!)

## Datum

# Datum und absolute Häufigkeit

date_type <- ggplot(date_data, aes(date)) +
              geom_col(aes(y = count_infrared, colour = "red"), fill = "transparent") +
              geom_col(aes(y = count_beacon, colour = "blue"), fill = "transparent") +
              scale_color_identity(name = "Messung",
                                   breaks = c("red", "blue"),
                                   labels = c("Infrarot", "Beacon"),
                                   guide = "legend") + 
              xlab("Datum") +
              ylab("Absolute Häufigkeit")

# Datum und Ratio

date_ratio <- ggplot(date_data) +
                geom_line(aes(date, ratio)) +
                xlab("Datum") +
                ylab("Ratio")
# -> hohe Schwankung am Anfang, wie zu erklären?

# Datum und Schneehöhe

date_snowhight <- ggplot(date_data) +
                    geom_line(aes(date, snowhight)) +
                    xlab("Datum") +
                    ylab("Schneehöhe (in cm)")

# Datum und temperatur

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

snowhight_solar_radiation <- ggplot(date_data) +
  geom_point(aes(snowhight, solar_radiation), alpha = 0.5) +
  xlab("Schneehöhe (in cm)") +
  ylab("solar radiation")

## Uhrzeit

time_type <- ggplot() +
                geom_freqpoly(data = subset(data, type == "Infrared"),
                  aes(time, colour = "red"), binwidth = 15) +
                geom_freqpoly(data = subset(data, type == "Beacon"),
                  aes(time, colour = "blue"), binwidth = 15) +
                scale_color_identity(name = "Messung",
                                     breaks = c("red", "blue"),
                                     labels = c("Infrarot", "Beacon"),
                                     guide = "legend") + 
                scale_x_datetime(date_labels = "%H:%M") +
                xlab("Uhrzeit") +
                ylab("Absolute Häufigkeit")

## Lawinen

# Daten umformen

avalanche_data <- date_data %>%
                select(ratio, avalanche_report_down, avalanche_report_top) %>%
                gather("position", "avalanche_risk",
                       -c("ratio"))

# Plotten

avalanche_plot <- ggplot(avalanche_data) +
                    geom_boxplot(aes(avalanche_risk, ratio, colour = position)) +
                    scale_color_manual(values = c("green", "orange"), 
                                      name = "Position",
                                      breaks = c("avalanche_report_down",
                                                 "avalanche_report_top"),
                                      labels = c("Unten", "Oben")) + 
                    xlab("Lawinenwarnstufe") +
                    ylab("Ratio")