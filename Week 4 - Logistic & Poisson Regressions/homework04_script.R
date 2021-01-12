### BUSINESS ANALYTICS ###
### Homework Sheet 4


## Homework 4.1
# install.packages("AER")
library(AER)
data("RecreationDemand")
?RecreationDemand
summary(RecreationDemand)

# a) Briefly describe the data set:
#    i.   Name the dependent variable and the independent variables
#    ii.  Which scales of measurement do the variables belong to (e.g. nominal, ordinal, interval or ratio)?
#    iii. Does the data set consist of cross-sectional, time-series or panel data?

attach(RecreationDemand)

summary(trips)
head(trips)
plot(table(trips))

summary(quality)
head(quality)
plot(table(quality))

summary(ski)
head(ski)
plot(table(ski))

summary(income)
head(income)
plot(table(income))

summary(userfee)
head(userfee)
plot(table(userfee))

summary(costC)
head(costC)
plot(table(costC))

summary(costS)
head(costS)
plot(table(costS))

summary(costH)
head(costH)
plot(table(costH))

#    i.   dependent variable: trips (boat trips in 1980)
#         independent variables: quality, ski, income, userfee, costC, costS, costH
#    ii.  ratio: trups, income, costC, costS, costH
#         nominal: ski, userfee
#         ordinal: quality, treated as numeric (see data set description)
#    iii. cross sectional: 2000 boat owners in 1980

# Estimate a Poisson Regression in which the number of boat trips is regressed against all explanatory variables:
# ln(mu(trips)) = Beta0 + Beta1*income + Beta2*costC + Beta3*costS + Beta4*costH + Beta5*ski + Beta6*userfee + Beta7*quality
rd_pois <- glm(trips ~ income + userfee + costC + costS + costH + ski + quality, data = RecreationDemand, family = poisson)
summary(rd_pois)

# b) The Poisson distribution has the property that variance equals mean (equidisperision). Thus, the Poisson Regression can only be applied if the mean of boat trips equals its variance.
#    Is the equidispersion assumption fulfilled in our data?
dispersiontest(rd_pois)
# we reject the zero-hypothesis of equidispersion ->  overdispersion

# c) As a consequence of overdispersion we decide to reestimate above model with a Negative Binomial Regression
# install.packages("MASS")
library(MASS)
rd_nb <- glm.nb(trips ~ income + userfee + costC + costS + costH + ski + quality, data = RecreationDemand)
summary(rd_nb)

# d) Which attributes are statistically significant regarding a significance level of 5%?

# intercept (*** = 0.001 = 0.1% significance level)
# income (not significant)
# userfee (. = 0.1 = 10% significance level)
# costC (*** = 0.001 = 0.1% significance level)
# costS (*** = 0.001 = 0.1% significance level)
# costH (*** = 0.001 = 0.1% significance level)
# ski (*** = 0.001 = 0.1% significance level)
# quality (*** = 0.001 = 0.1% significance level)

# e) Interpret the coefficients.

# intercept = -1.121936: for all other variables being zero (then quality = 0) the incidence rate is 0.326 (almost no boat trips) (exp(-1.122))
# Problem: this interpretation is not really allowed, because it can only be made outside our datarange 

#	income =  -0.026059: as income increases by one unit (1000 USD), incidence rate changes by a factor of 0.974 (decreases) (exp(-0.026))
# userfee = 0.669168: an introduction of userfee raises the incidence rate by a factor of 1.95 (almost doubles) (exp(0.669))
#	costC = 0.048009: increasing expenditure by one unit increases the incidence rate by a factor of 1.05 (remains more or less constant) (exp(0.05))
# costS = -0.092691: increasing expenditure by one unit decreases the incidence rate by a factor of 0.91 (exp(-0.09))
# costH = 0.038836: increasing expenditure by one unit increases the incidence rate by a factor of 1.04 (exp(0.04))
# ski = 0.61:  if the individual is engaged in water-skiing at the lake the incidence rate rises by a factor of 1.84 (almost doubles) (exp(0.61))
#	quality =  0.721999: an increase in the facility's quality by one unit raises the incidence rate by a factor of 2.05 (doubles) (exp(0.72))


## Homework 4.2
# b) Find the age for which the model would be indifferent between owning a car and not owning one.
#    An approximated solution using R's plotting functionality is sufficient.
age <- seq(0, 60, by = 1)
p <- (exp(-3.89 + (0.135*age))) / (1 + (exp(-3.89 + (0.135*age))))
plot(p, age)

# a more elaborate example using ggplot
library(tidyverse)
p_c <- 0.5
age_c <- (log(p_c / (1 - p_c)) + 3.89) / 0.135
ggplot(data = data.frame(age, p), aes(p, age)) + geom_point(size = 1, color = "blue") + geom_point(data = data.frame(age = age_c, p = p_c), color = "red", size = 2)