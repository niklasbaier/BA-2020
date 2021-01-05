### BUSINESS ANALYTICS ###
### Exercise Sheet 3


## Exercise 3.1 - Simple Linear Regression
x <- c(0.5, 0.6, 1.0, 1.4, 1.8, 3.6, 5.7, 6.4, 13.0)
y <- c(5, 28, 68, 77, 48, 48, 98, 96, 99)

# e) Test the zero hypothesis H0: Beta1 <= 0 with significance level alpha = 0.05
z <- lm(y ~ x)
summary(z)

# the output of the linear regression model delivers a double-sided t-test at significance level alpha = 0.05, but we are asked to test the single-sided hypothesis H0:B Beta1 <= 0 with alternative hypothesis H1: Beta1 > 0
# however, if the double-sided t-test rejects the zero hypothesis H0: Beta1 = 0 with H1: Beta1 != 0, then the single-sided t-test will automatically also reject the zero hypothesis H0: Beta1 =< 0 with alternative hypothesis H1: Beta1 > 0
# to see this, we can take the double-sided p-value of 0.03247 and divide it by 2 to obtain the single-sided p-value 0.016
# this p-value is strictly smaller than alpha = 0.05 and therefore, we can reject the single-sided zero hypothesis H0: Beta1 =< 0

## Exercise 3.2 - Simple Linear Regression
t <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
t_adjusted <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
demand <- c(28.20, 37.65, 47.28, 59.76, 73.44, 86.19, 100.31, 112.58, 121.63)

# a) for the time series above, calculate the forecasted demand value for t = 10 using the simple linear regression
# regression model
model <- lm(demand ~ t_adjusted)
summary(model)

# predictions
pm <- predict(model)
pm
predict(model, data.frame(t_adjusted = c(9, 10)))

# b) calculate the RMSE and explain its meaning
# prediction errors
pe <- c(pm - demand)
pe

# mean squared error (MSE)
MSE <- sum((pe)^2) / length(pe)
MSE

# root mean squared error (RMSE)
RMSE <- sqrt(MSE)
RMSE

# c) for the time series above, calculate the forecasted demand value for t = 10 assuming a biannual seasonal component of the following form: 
#    starting from the first period t = 0, suppose after every second period a new year begins

# biannual component
# q <- c(1, 0, 1, 0, 1, 0, 1, 0, 1)
q <- c(rep(1, 9))

for (i in c(1: length(q))) {
  if(i %% 2 == 0) {
    q[i] = 0
  }
}

# fit the regression model
model_new <- lm(demand ~ t_adjusted + q)

# output predicted values for t = 9 and t = 10
predict(model_new, data.frame(t_adjusted <- c(9, 10), q <- c(0, 1)))

model_new
summary(model_new)


## Exercise 3.3 - Gauss-Markov assumptions
# given the data in Exercise_3.csv (Y - response variable), test the Gauss-Markov assumptions using R
# if any of the properties is violated, make the necessary corrections until you have a BLUE estimator

library(lmtest)
library(car)
library(tidyverse)

df = read_csv("C://Users//Niklas Baier//Dropbox//CAREER & EDUCATION//Academic Career//M.Sc. Management & Technology//05 - Semesters//2020-2021 WS//Lectures//[IN2028] Business Analytics//Exercises//Tutorial 03//Exercise 03//Exercise_3.csv")

x1 <- df$x1
x2 <- df$x2
y <- df$y

# a) test if there is a linear dependency between predictors
model <- lm(y ~ x1 + x2)

# test for multicollinearity between predictors using vif() 
vif(model)
# both, x1 and x2, have scores greater than 10, one can be remoced, i.e. x2

# b) check the linearity assumption and fit a model to the data
plot(x1, y)
# the plot shows that there is no linear relationship between y and x1, but rather quadratic

model1 <- lm(y ~ I(x1^2))

# c) check if the homoscedasticity assumption holds (using the Breusch-Pagan or White test)
bptest(model1)
# the resulting pvalue is bigger than 0.05, therefore the assumption of homoscedasticity cannot be rejected

# d) check for autocorrelation (using the Durbin-Watson statistic)
plot(x1, model1$residuals, type = 'b')
dwtest(model1)
# as DW = 3.49, there is a negative autocorrelation between residual terms

# to model seasonality, a new variable s1 is introduced
s1 <- rep(0, length(x1))
s1[seq(1, length(s1), 2)] = 1

model2 <- lm(y ~ I(x1^2) + s1)
plot(x1, model2$residuals, type = 'b')
dwtest(model2)
# now, DW = 2.08, there is only very little autocorrelation

# e) test the exogeneity assumption and briefly explain the results

# the Hausman-Test is used only for panel data, therefore, exogeneity can be tested by calculating the correlation between the predictor and the residuals instead
cor(x1, model2$residuals)
# the correlation is almost 0, therefore the exogeneity assumption holds


# now, the ordinary least squares (OLS) estimator is BLUE