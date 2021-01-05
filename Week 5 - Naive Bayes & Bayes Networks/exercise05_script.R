### BUSINESS ANALYTICS ###
### Exercise Sheet 5


## Exercise 5.3 - Naive Bayes

library(tidyverse)

train = read_csv("loan-train.csv")
test = read_csv("loan-test.csv")
names(train)

# duration				            	duration of loan in months
# account at bank		            if account at bank, current balance
# purpose					              purpose of loan
# payment behavior	            payment behavior in respect to prior loans
# amount				  	            amount of loan
# savings account	              if savings account, current balance
# employed since		            time of employment at current employer
# ratio					  	            ratio of loan regarding current wage
# highest asset value			      highest available asset
# other installment loans		    other loans at other institutions than this bank
# previous installment loans		previous installment loans at this bank
# home					                living situation
# job						                employment situation
# age						                age	

# we want to create a prediction model using Naive Bayes

# a) transform the independent attribute "age" into a categorical attribute by placing the values into buckets
#    why are categorical variables preferable when using Naive Bayes?
#    what problems can occur with numerical data?

train <- train %>%
  mutate(age = case_when(
    age <= 25 ~ "<= 25",
    age > 25 & age <= 39 ~ "25 - 39",
    age > 40 & age <= 59 ~ "40 - 59",
    age > 60 ~ "60+",
    TRUE ~ "NA"
  ))

test <- test %>%
  mutate(age = case_when(
    age <= 25 ~ "<= 25",
    age > 25 & age <= 39 ~ "25 - 39",
    age > 40 & age <= 59 ~ "40 - 59",
    age > 60 ~ "60+",
    TRUE ~ "NA"
  ))

# numerical data is not very suitable for Naive Bayes as predicting the classification of data is based on comparing data to other data entries with the exact same value for an attribute
# categorical data tends to offer more data for comparison

# b) iterate through the independent attributes to find the most suitable attribute for the 1-rule classification
#    what attributes would you use for a 1-rule classification?

# install.packages("e1071")
library(e1071)

# create table to append error rates for each attribute when used to create rule
table_1rule <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(table_1rule) <- c("attribute", "errorRate")

# loop through attributes and append to table from above
for (i in 2:15) {
  train_subdata <- train %>% select(1, i)
  nb2 <- naiveBayes(as.factor(loan) ~ ., data = train_subdata, laplace = 1)
  rules <- predict(nb2, train_subdata, type = "class")
  errorRate = nrow(train_subdata %>% filter(loan != rules)) / nrow(train_subdata)
  table_1rule[nrow(table_1rule) + 1,] <- list(colnames(train[, i]), errorRate)
  rm(errorRate, i, rules, train_subdata, nb2)
}
table_1rule

# since payment behavior has the smallest errorRate of 0.2850, I would use that attribute for a 1-rule classification

# c) create a prediction model using the Naive Bayes classifier and apply it on the test-dataset
#    build a confusion matrix and determine the model's error rate
nb <- naiveBayes(as.factor(loan) ~ ., data = train, laplace = 1)

# predict "loan" attributes for test dataset
predictions = predict(nb, test, type = "class")

# create confusion matrix
confMat <- table(true = test$loan, prediction = data.frame(predictions)$predictions)
confMat

# calculate error rate
errorRate = nrow(test %>% filter(loan != predictions)) / nrow(test)
errorRate
# the error rate is 0.225 = 22.5%