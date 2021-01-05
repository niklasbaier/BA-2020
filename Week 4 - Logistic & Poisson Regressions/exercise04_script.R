### BUSINESS ANALYTICS ###
### Exercise Sheet 4


## Exercise 4.1 - Generalized Linear Models

library(tidyverse)

# Load the training data from the file admit-train.csv into R
train = read_csv("admit-train.csv")

names(train) 	
# "admit" (acceptance for master course) = 1 means success of admission. 0 does failure.  
# "gre", "gpa" (points in exams): 
# GRE: Graduate Record Examinations e (200, 800) 10 point increments
# GPA: Grade Point Average e (2, 4) (4 is best GPA)
# "rank" (rank of bachelor university ) = e (1, ..., 4) 1 point increments (1 is best rank)

summary(train$gre)
summary(train$gpa)
summary(train$rank)
summary(as.factor(train$rank))

# a) briefly describe the data set:
#    i.  name the dependent variable and the independent variables
#    ii. which st do the variables belong to (e.g. nominal, ordinal, interval, or ratio)?

#    i.  binary dependent variable: admit
#        independent variables: gre, gpa, rank

#    ii. interval (numerical): gre, gpa
#        ordinal: rank
#        nominal: admit

# visualize relationships
plot(admit ~ gre, data = train, pch = "+")
plot(admit ~ gpa, data = train, pch = "+")
plot(admit ~ rank, data = train, pch = "+")

# visualise distributions
hist(train$gre, breaks = 25)
hist(train$gpa, breaks = 18)

# due to the fact that the dependent attribute "admit" is binaryy, we use a logistic regression model
mylogit <- glm(admit ~ gre + gpa + as.factor(rank), data = train, family = binomial(link = "logit"))
summary(mylogit)

# b) which attributes are statistically significant regarding a significance level of 5%?

# intercept (*** = 0.001 = 0.1% significance level)
# gre (* = 0.05 = 5% significance level)
# gpa (* = 0.05 = 5% significance level)
# rank_3 (*** = 0.001 = 0.1% significance level)
# rank_4 (*** = 0.001 = 0.1% significance level)

# c) interpret the coefficients

# intercept = -4.871106
#	b(gre) = 0.003: as the GRE score increases by one point, ODDS of being accepted rise by a factor of 1.003 (exp(0.003) = 1.003)
# 10*b(gre) = 0.03: as the GRE score increases by ten points, ODDS of being accepted rise by a factor of 1.03 (exp(0.03) = 1.03)
#	b(gpa) = 0.92: as the GPA score increases by one point, ODDS of being accepted rise by a factor of 2.5 (exp(0.92) = 2.51)
#	b(rank_3) = -1.52: If the rank is 3, ODDS of being accepted fall by a factor of 0.22 (exp(-1.52) = 0.22) compared to rank 1
#	b(rank_4) = -1,69: If the rank is 4, ODDS of being accepted fall by a factor of 0.18 (exp(-1.69) = 0.18) compared to rank 1

# d) test the significance of the attribute "rank" using a Wald-Test
# install.packages("aod")
library(aod)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
# similar to F-test in multiple linear regression analysis
# the attribute rank (consisting of its 3 dummy components (independent variables 4 to 6 in our model) is significant)

# e) in order to gain a better understanding of the model, have a look at the predicted probabilities of some observations
#    adjust only one parameter and keep the others constant, e.g. keep "gre" and "gpa" constant (using their mean/average) and vary "rank"

rank <- c(1, 2, 3, 4)
gre <- c(mean(train$gre))
gpa <- c(mean(train$gpa))

myinstances <- data.frame(gre, gpa, rank)
myinstances

# in order to find the predicted probability, add another variable named pAdmit to myInstances and fill it with values from the myLogit model
myinstances$pAdmit <- predict(mylogit, newdata = myinstances, type = "response")
myinstances
# the better ranked the bachelor university, the higher probability of being accepted
# these are the 'true' probabilities and were numerically computed  

# f) find the McFadden ratio and interpret the results
McFadden <- 1 - (mylogit$deviance / mylogit$null.deviance)
McFadden
# not very good, because of missing explanatory variables for example

# g) load the data record "admit-test.csv" and predict the probability
test <- read_csv("admit-test.csv")
preds <- predict(mylogit, newdata = test, type = 'response')

# confusion matrix
test <- test %>% mutate(pred = round(preds)) 
test %>% group_by(admit, pred) %>% summarise(count = n())

# alternatively
table(true = test$admit, prediction = round(preds))

# h) find the logit model's error rate
incorrectPredictionCount <- nrow(test %>% filter(admit != pred))
totalPredictions <- nrow(test)
errorRate <- incorrectPredictionCount / totalPredictions
errorRate