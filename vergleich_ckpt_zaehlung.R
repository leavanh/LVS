# Plot zum Vergleich Checkpoints vs manuelle Zählung


z_28_breaks <- mutate(z_28, breaks = cut(z_28$time, breaks = "3 min"))

z_28_grouped_5min <- z_28_breaks %>% group_by(breaks) %>% summarise(erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst))

ckpt_28_breaks <- mutate(ckpt_28, breaks = cut(ckpt_28$time, breaks = "3 min"))

ckpt_28_grouped_5min <- ckpt_28_breaks %>% group_by(breaks) %>% summarise(erfasst = n())

z_28_grouped_5min <- z_28_grouped_5min %>% mutate(type = "Zählung")
ckpt_28_grouped_5min <- ckpt_28_grouped_5min %>% mutate(type = "Checkpoint")

zaehlung_vs_checkpoint_grouped_5min <- rbind(z_28_grouped_5min[, -3], ckpt_28_grouped_5min)


lims <- as.POSIXct(strptime(c("1899-12-31 10:00:00","1899-12-31 17:00:00"), format = "%Y-%m-%d %H:%M", tz = "UTC"))

zaehlung_vs_checkpoint_28 <- ggplot(data = zaehlung_vs_checkpoint_grouped_5min, 
                                     aes(x = as.POSIXct(breaks), y = erfasst, fill = type)) +
                                geom_bar(stat = "identity", position = "dodge") +
                                scale_color_identity(name = "Messungen",
                                                     breaks = c("red", "blue"),
                                                     labels = c("Checkpoint", "Manuell"),
                                                     guide = "legend") + 
                                scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M",
                                                 limits = lims) +
                                scale_y_continuous(limits = c(0,12)) +
                                #guide_legend(title = NULL) +
                                labs(title = "Vergleich Messungen am 28.02.",
                                     x = "Uhrzeit",
                                     y = "Absolute Häufigkeit")


