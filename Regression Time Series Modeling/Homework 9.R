install.packages("forecast")
library(forecast)
data <- read.csv("toothpaste.csv")
sales_ts <- ts(data$y)
plot(sales_ts, type = "o", main = "Weekly Sales of Toothpaste", xlab = "Week", ylab = "Sales")
acf(sales_ts, main = "ACF of Weekly Sales")
acf_values <- acf(sales_ts, plot = FALSE)
acf_values$acf[1:6]
ar_model <- arima(sales_ts, order = c(1, 0, 0))
beta0 <- ar_model$coef[1]
beta1 <- ar_model$coef[2]
sigma2 <- ar_model$sigma2
beta0
beta1
sigma2
mean_y <- beta0 / (1 - beta1)
var_y <- sigma2 / (1 - beta1^2)
mean_y
var_y
y3 <- sales_ts[3]
y2 <- sales_ts[2]
y_hat3 <- beta0 + beta1 * y2
residual_y3 <- y3 - y_hat3
residual_y3
residuals <- residuals(ar_model)
acf(residuals, main = "ACF of Residuals")
forecast_values <- forecast(ar_model, h = 3)
forecast_values
forecast_values$lower
forecast_values$upper
forecast_output <- data.frame(
  Forecast = forecast_values$mean,
  Lower_95 = forecast_values$lower[,2], 
  Upper_95 = forecast_values$upper[,2]  
)
print(forecast_output)
pacf(sales_ts, main = "Partial Autocorrelation of Weekly Sales")
pacf_values <- pacf(sales_ts, plot = FALSE)
pacf_values$acf[2:6]
ar2_model <- arima(sales_ts, order = c(2, 0, 0))
ar2_model$coef
ar2_model$sigma2
beta_0 <- ar2_model$coef[1]
beta_1 <- ar2_model$coef[2]
beta_2 <- ar2_model$coef[3]
sigma2 <- ar2_model$sigma2
E_yt <- beta_0 / (1 - beta_1 - beta_2)
Var_yt <- sigma2 / (1 - beta_1^2 - beta_2^2 - 2 * beta_1 * beta_2)
E_yt
Var_yt
y3_actual <- sales_ts[3]
y3_hat <- ar2_model$coef[1] + ar2_model$coef[2] * sales_ts[2] + ar2_model$coef[3] * sales_ts[1]
residual_y3 <- y3_actual - y3_hat
residual_y3
residuals_ar2 <- residuals(ar2_model)
acf(residuals_ar2, main = "ACF of Residuals from AR(2) Model")
aic_ar1 <- ar_model$aic
aic_ar2 <- ar2_model$aic
aic_ar1
aic_ar2
forecast_ar2 <- forecast(ar2_model, h = 3)
forecast_output <- data.frame(
  Forecast = forecast_ar2$mean,
  Lower_95 = forecast_ar2$lower[, 2],  # Lower bound of 95% CI
  Upper_95 = forecast_ar2$upper[, 2]   # Upper bound of 95% CI
)
print(forecast_output)
