### BUSINESS ANALYTICS ###
### Exercise Sheet 2


## Exercise 2.1 - Statistics

# The consumption per person is measureed in index values, where a high iindex value represents a high consumption.

# previous to tax increase, a
a <- c(27, 31, 23, 35, 26, 27, 26, 18, 22, 21)

# after tax increase, b
b <- c(40, 36, 43, 34, 25, 41, 32, 29, 21, 36)

# b) Determine if there is a significant difference in consumption prior to the tax increase and after, utilizing a hypothesis test (significance level alpha = 0.05). The difference is assumed to be normally distributed.
# paired t-test
t.test(a, b, alternative = "two.sided", paired = T)

# <=> test if difference is significantly different from zero
d <- a - b
t.test(d)
# H0 is rejected!


## Exercise 2.3 - Statistics

# During a recent study project, a friend of yours asked 8 men and 10 women how many hours per day they wear a mask during the ongoing COVID-19 pandemic.
female <- c(4, 2, 3, 5, 7, 2, 7, 3, 5, 2)
male <- c(2, 1, 5, 3, 1, 3, 2, 3)

# b) Test the hypothesis "On average, women wear their mask longer per day" with a significance level alpha = 0.05 and 16 degrees of freedom
t.test(female, male, alternative = "greater", paired = F)