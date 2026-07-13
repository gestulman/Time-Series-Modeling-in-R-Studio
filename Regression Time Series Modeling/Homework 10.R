install.packages("forecast")
library(forecast)
data <- read.csv("HW10.csv")
ts_sales <- ts(data)
plot(ts_sales, main="Time Series Plot of Weekly Sales", ylab="Sales", xlab="Week")
acf(ts_sales, main="ACF of Weekly Sales")
acf_values <- acf(ts_sales, plot=FALSE)$acf
print(acf_values[2:6])
ma1_model <- Arima(ts_sales, order=c(0,0,1))
summary(ma1_model)
mu_hat <- ma1_model$coef["intercept"]
theta1_hat <- ma1_model$coef["ma1"]
sigma2_hat <- ma1_model$sigma2
mu_hat
theta1_hat
sigma2_hat
acf(residuals(ma1_model), main="ACF of Residuals")
forecast_values <- forecast(ma1_model, h=3)
print(forecast_values)
ma2_model <- Arima(ts_sales, order=c(0, 0, 2))
summary(ma2_model)
mu_hat <- ma2_model$coef["intercept"]
theta1_hat <- ma2_model$coef["ma1"]
theta2_hat <- ma2_model$coef["ma2"]
sigma2_hat2 <- ma2_model$sigma2
mu_hat
theta1_hat
theta2_hat
sigma2_hat2
acf(residuals(ma2_model), main="ACF of Residuals (MA(2) Model)")
acf_values <- acf(residuals(ma2_model), plot=FALSE)$acf
aic1 <- ma1_model$aic
aic2 <- ma2_model$aic
aic1
aic2
forecast_values <- forecast(ma2_model, h=3)
print(forecast_values)
