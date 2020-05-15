date_model <- gam(
  cbind(lvs_true, lvs_false) ~ 
    s(temperature, bs = "ps", k = 10) +
    s(snow_diff, bs = "ps", k = 10)
    ,
  knots = list(c(-15.47554, -12.95623, -10.43691, -7.9176, -5.398286, -2.878971,
                 -0.3596571, 2.159657, 4.678971, 7.198286, 9.7176, 12.23691,
                 14.75623, 17.27554),
               c()),
  data = date_data,
  method = "REML",
  family = binomial(link = "logit"))
cat(date_model$smooth[[1]]$knots, sep= ", ")
table(date_data$int_date[is.na(date_data$ratio)])