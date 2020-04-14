# check binary models

plot(model_3$linear.predictors, residuals.gam(model_3, "deviance"))
# most of residuals should be within +- 3

plot(model_3$linear.predictors, abs(residuals.gam(model_3, "deviance")))
# non-zero slope -> bad