### BUSINESS ANALYTICS ###
### Homework Sheet 1


## Homework 1 - Game of Dice with R

# a) Have a look at the methods sample() and table().

# b) Generate a random sample of 1000 observations in a fair die experiment and store it in a variable called w1. 
#    Find the absolute and relative frequency of the random sample and construct a bar chart for the relative frequency of the numbers 1 to 6. 
#    Additionally, calculate the mean and variance of the numbers 1 to 6.
set.seed(10)
w1 <- sample(1:6, size = 1000, replace = T)

table(w1)
barplot(table(w1) / 1000, ylim = c(0, 0.25))
mean(w1)
var(w1)

# c) Generate a random sample of 1000 observations in a fair die experiment and store it in a variable called w2.
#    Is w1 exactly identical to w2?
w2 <- sample(1:6, size = 1000, replace = T)

# d) Interpret the data from the vector w12 <- w1 + w2.
#    What are the mean and variance of the face values? Construct a suitable graph to illustrate the relative frequency of the face values.
w12 <- w1 + w2
# it is the sum of the face values of two dice
mean(w12)
var(w12)
barplot(table(w12) / 1000, ylim = c(0, 0.25))


## Homework 2 - Working with Data

# Import the data from the file "E1-3-data.csv". It holds 3 columns of data, each contains 100 samples and each has a different distribution.
library(tidyverse)
data <- read_delim('E1-3-data.csv', " ")

# a) Which sample was taken from a normally distributed population?
hist(data$A)
hist(data$B)
hist(data$C)
# B was taken from a normally distributed population

# b) For the corresponding normal distribution referred above calculate the mean and standard deviation.
mean(data$B)
sd(data$B)

# c) Construct a histogram from the same normally distributed sample from above and overlay it with its density curve.
x <- seq(min(data$B), max(data$B), length = 1000)
hist(data$B, freq = F)
lines(x, dnorm(x, mean(data$B), sd(data$B)))

# d) The sampling process was simulated with a N(10, 80)-distribution.
#    Add the true density functino to the chart.
lines(x, dnorm(x, 10, 80), col = 'red')