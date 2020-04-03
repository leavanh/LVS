mydata <- cbind(date_data_noNA, fitted_values = model_6$gam$fitted.values) %>%
  gather("type", "percentage", c("ratio", "fitted_values"))


ggplot(mydata, aes(x = date)) +
  geom_line(aes(y = percentage, color = type), size = 0.8)
