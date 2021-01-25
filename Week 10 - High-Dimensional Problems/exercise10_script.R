### BUSINESS ANALYTICS ###
### Exercise Sheet 10


## Exercise 10.3

library(AER)
library(pls) # for pcr function

# a) Check the structure of the dataset. Filter the numerical attributes and discard the rest.
data("HousePrices")
str(HousePrices)

HousePrices <- HousePrices[, unlist(lapply(HousePrices, is.numeric))]
str(HousePrices)

# b) Build a model to predict the price of a house given the other independent variables using principal component regression with one component.
#    How much of the dependent variable is explained by the model?
pcr_auto <- pcr(price ~ ., data = HousePrices, scale = TRUE, ncomp = 1)
summary(pcr_auto)

# alternatively
pca <- princomp(HousePrices[-1], cor = TRUE)
prCom1 <- pca$scores[, 1]
pcr_manual <- lm(HousePrices$price ~ prCom1)
summary(pcr_manual)

# c) Build a model to predict the price of a house using simple OLS regressino for each independent variable separately.
#    Which OLS model explains best the price? What percentage of variation is explained in this case?
for(i in c(2:ncol(HousePrices))) {
  print(c("price", colnames(HousePrices)[i]))
  lin_m <- lm(price ~ ., data = HousePrices[, c("price", colnames(HousePrices)[i])])
  print(summary(lin_m)$r.squared)
}

# Pick model with largest R^2 (lotsize with R^2 of 0.287077)
lin_m <- lm(price ~ lotsize, data = HousePrices)

# d) Compare the models derived from b) and c).
#    Which one would you choose in this scenario? Give reasons.
summary(pcr_manual)$r.squared*100
summary(lin_m)$r.squared*100
# PCR with single component explains more variance than simple OLS with original variable

plot(HousePrices$price)
points(fitted(pcr_manual), col = 'red')
points(fitted(lin_m), col = 'blue')