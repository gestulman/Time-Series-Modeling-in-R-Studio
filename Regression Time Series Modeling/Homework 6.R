i <- 1:10
X_i <- c(1,0,2,0,3,1,0,1,2,0)
Y_i <- c(16,9,17,12,22,13,8,15,19,11)
Poisson1 <- glm(Y_i ~ X_i, family = poisson)
summary(Poisson1)
coefficients <- coef(Poisson1)
response <- function(X){exp(coefficients[1] + coefficients[2] * X)}
residuals_deviance <- residuals(Poisson1)
plot(residuals_deviance, pch=20)
abline(h = 0, col = "red", lty = 2)
outliers <- abs(residuals_deviance) > 2  # Threshold for outliers (customizable)
points(i[outliers], residuals_deviance[outliers], col = "red", pch = 20)
lambda <- exp(coefficients[1] + coefficients[2] * 0)
lambda
probability_10_or_fewer <- ppois(10, 10.51654)
probability_10_or_fewer
