# Plot zum Vergleich Checkpoints vs manuelle Zählung


z_28_breaks <- mutate(z_28, breaks = cut(z_28$time, breaks = "5 min"))

z_28_grouped_5min <- z_28_breaks %>% group_by(breaks) %>% summarise(erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst))

ckpt_28_breaks <- mutate(ckpt_28, breaks = cut(ckpt_28$time, breaks = "5 min"))

ckpt_28_grouped_5min <- ckpt_28_breaks %>% group_by(breaks) %>% summarise(erfasst = frequency(type))




plot_ckpt_vs_z <- ggplot() +
  geom_freqpoly(data = ckpt_28,
                aes(x = time, colour = "red"), binwidth = 5) +
  geom_bar(data = z_28_grouped_5min,
           aes(x = as.POSIXct(breaks, tz = "UTC"), y = erfasst, colour = "blue")) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_identity(name = "Messung",
                       breaks = c("red", "blue"),
                       labels = c("Person", "Lvs-Gerät"),
                       guide = "legend") + 
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M") +
  labs(title = "Messungen nach dem Umcodieren",
       x = "Uhrzeit",
       y = "Absolute Häufigkeit")