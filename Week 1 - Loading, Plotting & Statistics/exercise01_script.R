### BUSINESS ANALYTICS ###
### Exercise Sheet 1


## Exercise 1.1 - Loading a data set and statistics
library(tidyverse)

# a) Read the CSV file "LaborSupply1988.csv" into a tibble df.
df = read_csv("C://Users//Niklas Baier//Dropbox//CAREER & EDUCATION//Academic Career//M.Sc. Management & Technology//05 - Semesters//2020-2021 WS//Lectures//[IN2028] Business Analytics//Exercises//Tutorial 01//Exercise 01//LaborSupply1988.csv")

# b) How many attributes (columns) and observations (rows) does df have?
# the tidyverse way
glimpse(df)

# the other way
str(df)
nrow(df)
ncol(df)

# c) Which attributes does the data set have?
names(df)

# d) List the first rows of the data set.
head(df, n=20)

# e) What is the value range of the attribute - age?
# the tidyverse way
summarise(df, min_age=min(age), max_age=max(age))

# the other way
summary(df$age)
min(df$age)
max(df$age)
range(df$age)

# f) Calculate the average of annual hours worked by the laborers with 0, 1, 2, ... 6 kids each
# the tidyverse way
df %>% group_by(kids) %>% summarise(mean_lnhr=mean(lnhr))

# the other way
mean(df[df$kids == 0,]$lnhr)
mean(df[df$kids == 1,]$lnhr)
mean(df[df$kids == 2,]$lnhr)
mean(df[df$kids == 3,]$lnhr)
mean(df[df$kids == 4,]$lnhr)
mean(df[df$kids == 5,]$lnhr)
mean(df[df$kids == 6,]$lnhr)

# g) Calculate the average number of kids of a 40 year old person
# the tidyverse way
df %>% filter(age == 40) %>% summarise(mean_kids=mean(kids))

# the other way
mean(df[df$age == 40,]$kids)


## Exercise 1.2 - Plotting

# a) Plot a histogram of the attribute age. What is the most frequent age?
hist(df$age)
df %>% group_by(age) %>% summarise(count=n()) %>% arrange(desc(count))

# b) Plot the average number of kids against the age and interpret the resulting graph. Underpin your observation using a statistical method
# the tidyverse way
plot(df %>% group_by(age) %>% summarise(avg_kids=mean(kids)))

# the other way
plot(aggregate(x=df$kids, by=list(df$age), FUN=mean))

cor(df$kids, df$age)
# the average number of kids decreases with increasing age, the two attributes are correlated negatively

# c) Plot the log of hourly wage (lnwg) against the age
plot(df$age, df$lnwg)

# d) Plot the mean of the log of hourly wage (lnwg) against the age. How are they correlated? Also compute the correlation between log of hourly wage (lnwg) against the age.
# the tidyverse way
plot(df %>% group_by(age) %>% summarise(avg_lnwg=mean(lnwg)))

# the other way
plot(aggregate(x=df$lnwg, by=list(df$age), FUN=mean))

cor(df$lnwg, df$age)

# e) Plot lnhr against the age with different colors for disab=0 and disab=1.
plot(df$age, df$lnhr, pch=df$disab+1, col=c("red", "blue")[df$disab+1])

# f) Plot a boxplot of the log of annual hours worked (lnhr) against the number of kids. What could be observed regarding mean and variance? Is the observation meaningful for large values of kids?
boxplot(df$lnhr ~ df$kids)
# the mean increases slightly with increasing number of kids. Nothing much can be observed regarding the variance.

# we plot a bar plot to identify number of observations for different number of kids
counts <- table(df$kids)
barplot(counts, main="Number of datapoints against Number of Kids",
        xlab = "Number of Kids",
        ylab = "Number of datapoints")
# from the plot it can be observed that only two observations for 5 and 6 kids exist. Hence, the observation regarding mean and variance is not very meaningful.