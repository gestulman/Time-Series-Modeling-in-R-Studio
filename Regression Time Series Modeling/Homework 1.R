#read Wisconsin Lottery file
Lot<-read.csv(file="WiscLottery.csv", header = TRUE)
#create variables representing population and sales in the thousands
Lot$POP_1000 <- Lot$POP/1000
Lot$SALES_1000 <- Lot$SALES/1000
#summary of population/sales in the thousands variables
summary(Lot$POP_1000)
summary(Lot$SALES_1000)
#plot population vs sales
plot(Lot$POP_1000, Lot$SALES_1000, xlab = "population", ylab = "sales", main = "Scatter Plot of Population and Sales", pch = 20)
#correlation between population and sales in terms of thousands
cor(Lot$POP_1000, Lot$SALES_1000)
#correlation between original population and sales variables
cor(Lot$POP, Lot$SALES)
#summary of medhome variable in dataframe Lot
summary(Lot$MEDHVL)
#summary of sales variable in dataframe Lot
summary(Lot$SALES)
#plot median house per zip code vs sales
plot(Lot$MEDHVL, Lot$SALES, xlab = "median house price", ylab = "sales", main = "Scatter Plot of Median House Price and Sales", pch = 20)
#correlation between median house per zip code and sales
cor(Lot$MEDHVL, Lot$SALES)
#make a model that calculates the regression coefficients for median house price on sales
model_1 <- lm(SALES ~ MEDHVL, data = Lot)
#list regression coefficients with 4 digits
round(coefficients(model_1), digits = 4)
#create a data frame where median house price is 50,000 dollars
Med50Data <- data.frame(MEDHVL = 50)
#create a prediction based on the regression model and median house price is 50,000 dollars
predict(model_1, Med50Data)
#summary of the regression model in an anova table
anova(model_1)
#size of the typical residual
sqrt(anova(model_1)$Mean[2])
#coefficient of determination
summary(model_1)$r.squared
#summary of regression model
summary(model_1)
#95% confidence interval
confint(model_1, level = 0.95)
#point prediction and 95% prediction interval when median house price is 50,000 dollars
predict(model_1, Med50Data, interval = "prediction", level = 0.95)
