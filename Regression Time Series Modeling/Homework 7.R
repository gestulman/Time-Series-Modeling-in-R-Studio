data <- read.csv("EuroExchange.csv")
exchange_rate <- data$exeuus
time <- 1:length(exchange_rate)
quad_model <- lm(exchange_rate ~ time + I(time^2))
summary(quad_model)
future_time <- c(700, 701, 702)
future_data <- data.frame(time = future_time, I_time2 = future_time^2)
forecast_values <- predict(quad_model, newdata = future_data)
print(forecast_values)
diff_euro <- diff(exchange_rate)
mean_diff <- mean(diff_euro, na.rm = TRUE)
sd_diff <- sd(diff_euro, na.rm = TRUE)
forecast_diff <- rep(0, 3)
euro_699 <- exchange_rate[length(exchange_rate)]
euro_forecast <- c(euro_699, euro_699 + forecast_diff[1], euro_699 + forecast_diff[1] + forecast_diff[2], euro_699 + forecast_diff[1] + forecast_diff[2] + forecast_diff[3])
variance_accumulated <- c(1, 2, 3) * sd_diff^2
prediction_intervals <- data.frame(
  Lower = euro_forecast[-1] - 1.96 * sqrt(variance_accumulated),
  Upper = euro_forecast[-1] + 1.96 * sqrt(variance_accumulated)
)
print(euro_forecast[2:4])
print(prediction_intervals)
