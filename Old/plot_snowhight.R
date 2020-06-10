plot_data <- data.frame(date = date_data$date, 
                   snowhight = date_data$snowhight, 
                   res = date_data$res_snowhight, 
                   smooth = snow_gam$fitted.values) %>%
  gather("type",  "value", 2:4)

ggplot(plot_data) +
  geom_line(aes(x = date, y = value, colour = type)) +
  xlab("Datum") +
  ylab("Schneehöhe (in cm)") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.title = element_blank()) +
  scale_colour_discrete(name = "Art",
                      labels = c("Residuen", "Smooth", "wahre Werte"))

ggplot(date_data) +
  geom_line(aes(date, snowhight)) +
  xlab("Datum") +
  ylab("Schneehöhe (in cm)") +
geom_line(aes(date, res_snowhight)) + geom_line(aes(date,snow_gam$fitted.values))

