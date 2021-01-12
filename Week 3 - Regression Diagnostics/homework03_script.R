### BUSINESS ANALYTICS ###
### Homework Sheet 3


## Homework 3.4
# install.packages("AER")
library(AER)
data("CPS1988")
?CPS1988
summary(CPS1988)

attach(CPS1988)  
# attach "journal" data set st. R knows all the relevant variables 

# a) Briefly describe the data set:
#    i.   Name the dependent variable and the independent variables
#    ii.  Which scales of measurement do the variables belong to (e.g. nominal, ordinal, interval or ratio)?
#    iii. Does the data set consist of cross-sectional, time-series or panel data?

summary(wage)
head(wage)
plot(table(wage))

summary(experience)
head(experience)
plot(table(experience))

summary(education)
head(education)
plot(table(education))

summary(ethnicity)
head(ethnicity)
plot(table(ethnicity))

#    i.   dependent variable: wage
#         independent variables: education, experience, ethnicity, smsa, region, parttime
#    ii.  ratio: wage, education, experience
#         nominal: ethnicity, smsa, region, parttime
#    iii. cross sectional: 28155 different men i 1988

# b) Plot the dependent variable against each independent variable and transform the variables if necessary.
#    Which transformations would you carry out and why?
plot(experience, wage, pch = "+")
# too many observations of wage close to the origin and only a few very far away
# => weigh the observations with the logarithmic function

plot(experience, log(wage), pch = "+")
# quadratic relationship observable
# => include square experience in the model

abline(lm(log(wage) ~ experience), pch = "+")


plot(education, log(wage), pch = "+")
abline(lm(log(wage) ~ education), pch = "+")

plot(ethnicity, log(wage), pch = "+")
abline(lm(log(wage) ~ ethnicity), pch = "+")

# c) Interpret the model mr_1: ln(wage) = Beta0 + Beta1*education + Beta2*ethnicity + Beta3*experience + Beta4*experience^2
#    i.   Which variables are statistically significant?
#    ii.  Is the entire model statistically significant?
#    iii. What is the explanatory power of the model and why?
#    iv.  Interpret each regression coefficient
mr_1 = lm(log(wage) ~ experience + I(experience^2) + education + ethnicity)
# I is used for ^ to introduce polynomial

summary(mr_1)

#    i.   All variables, including the intercept, are statistically significant (t value) at a significance level of 0.1 percent
#    ii.  The entire model is statistically significant (F-statistic) at a significance level of 0.1 percent (p-value)
#    iii. Adjusted R-squared: 0.3346 (rather low explanatory power) because too many important variables missing (e.g. ability)
#    iv.  Beta0 = 4.321, wage = e^4.321 = 75.26 Wage per week for Caucasian-American worker with no educatino and no experience
#         Beta1 = 0.08567, ln(wage'/wage) = 0.086, Wage increases by a factor of e^0.086 = 1.09 or 9% for each additional year of education
#         Beta2 = -0.2434, ln(wage'/wage) = -0.2434, Wage decreases by a factor of e^-0.2434 = 0.79 or 21% if worker is African-American (ethnicity = 1)
#         Beta3 = 0.07747 and Beta4 = -0.001316, because experience enters the linear regression as a linear and a quadratic term, the effect of an increase in experience on wage depends on the level of experience

# d) Estimate the model mr_2: ln(wage) = Beta0 + Beta1*education + Beta2*ethnicity + Beta3*education*ethnicity + Beta4*experience + Beta5*experience^2
#    What is the difference between both models from above (mr_1 and mr_2)?

# model mr_2 contains an interaction term in addition to model mr_1
# The interaction term between education and ethnicity allows us to distinguish between the marginal effect of education on the wage of an African-American worker and on the wage of a Caucasian-American worker

# e) Interpret model mr_2, analog to c)
mr_2 = lm(log(wage) ~ experience + I(experience^2) + education + ethnicity + education*ethnicity)
summary(mr_2)

#    i.   Ethnicity and the interaction effect are statistically significant (t value) at a significance level of 5 percent
#         All other variables, including the intercept, are statistically significant (t value) at a significance level of 0.1 percent
#         The effect of being African-American on wage now splits up between the dummy and the interaction effect and therefore is weaker for each variable
#    ii.  The entire model is statistically significant (F-statistic) at a significance level of 1 percent (p-value)
#    iii. Adjusted R-squared: 0.3347 (rather low explanatory power) has increased slightly, but still too many important variables missing (e.g. ability)
#    iv.  Interpretation of coefficients, see Solution


## Homework 3.5
# install.packages("AER")
library(AER)
# install.packages("plm")
library(plm)

data("Grunfeld", package = "AER")
?Grunfeld
summary(Grunfeld)

# a) Briefly describe the data set:
#    i.   Name the dependent variable and the independent variables
#    ii.  Which scales of measurement do the variables belong to (e.g. nominal, ordinal, interval or ratio)?
#    iii. Does the data set consist of cross-sectional, time-series or panel data?
attach(Grunfeld)

summary(invest)
head(invest)
plot(table(invest))

summary(value)
head(value)
plot(table(value))

summary(capital)
head(capital)
plot(table(capital))

summary(firm)
head(firm)
plot(table(firm))

summary(year)
head(year)
plot(table(year))

#    i.   dependent variable: invest
#         independent variables: value, capital, firm, year
#    ii.  ratio: invest, value, capital, year
#         nominal: firm
#    iii. panel: 11 firms from 1935 to 1954

# b) Plot the dependent variable against each independent variable and transform the variables if necessary.
#    Which transformations would you carry out and why?
grunfeld = subset(Grunfeld, firm %in% c("General Electric", "General Motors", "IBM"))  
# delete all observations but "GE", "GM" and "IBM"
panel_grunfeld = plm.data(grunfeld, index = c("firm", "year"))

attach(panel_grunfeld)
plot(value, invest, pch = "+")
plot(capital, invest, pch = "+")
plot(firm, invest, pch = "+")
plot(year, invest, pch = "+")

# c) Consider the model: invest = Beta0 + Beta1*value + Beta2*capital
#    How can you test the presence of unobserved individual specific effects in the above model?

# First, we carry out a pooled linear regression (SLR for panel data), that does not take into account the possibility of unobserved individual specific effects.
grunfeld_pool <- plm(invest ~ value + capital, data = panel_grunfeld, model = "pooling")
summary(grunfeld_pool)

# Then, we use the Lagrange Multiplier Test for Panel Models to test for unobserved individual specific effects
plmtest(grunfeld_pool)
# The zero hypothesis assumes no presence of unobserved individual specific effects.
# As the p-value < 2.2e-16 is very low, we reject the zero hypothesis and conclude that significant unobserved individual specific effects are present

# d) Should you use a Random Effects Regression or a Fixed Effects Regression to take into account the unobserved individual specific effects?

# Random Effects Regression
grunfeld_re <- plm(invest ~ value + capital, data = panel_grunfeld, model = "random", random.method = "walhus")
summary(grunfeld_re)

# Fixed Effects Regression
grunfeld_fe <- plm(invest ~ value + capital, data = panel_grunfeld, model = "within")
summary(grunfeld_fe)

# We now conduct a Hausman test to check whether the unobserved individual specific effects are "problematic" in our panel data model and have to be taken into account by a Fixed Effects Regression
phtest(grunfeld_re, grunfeld_fe)
# The Hausman test assumes as zero hypothesis that the Random Effects Regression is adequate to take into account unobserved individual specific effects.
# As the p-value = 0.98 is very high, we cannot reject the zero hypothesis and conclude that a Random Effects Regression is adequate to take into account unobserved individual specific effects.