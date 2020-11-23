### DATA ANALYSIS AND VISUALIZATION IN R ###
### Exercise Sheet 1


## Section 01 - R very basics

# 1
n <- 100
sum <- n * (n + 1) / 2
sum

# 2
n <- 1000
sum <- n * (n + 1) / 2
sum

# 3
n <- 1000
x <- seq(1, n)
sum(x)

# 4
log10(sqrt(100))

# 5
x <- 13
log(exp(x))

# 6
sum(1 / seq(1:100)^2)
pi^2 / 6


## Section 02 - Basic data

# 1
# install.packages("dslabs")
library(dslabs)
data(murders)
str(murders)

# 2
names(murders)
# colnames(murders)

# 3
a <- murders$abb
class(a)

# 4
length(levels(murders$region))

# 5
x <- c('DC', 'Alabama', 'Florida', 'DC', 'DC')
table(x)
table(murders$region)


## Section 03 - Working with vectors

# 1
temp <- c(35, 88, 42, 84, 81, 30)
temp

# 2
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city

# 3
names(temp) <- city
temp

# 4
temp[1:3]

# 5
temp[c("Paris", "San Juan")]

# 6
12:73

# 7
seq(1, 100, 2)

# 8
v <- seq(6, 55, 4/7)
length(v)

# 9
a <- seq(1, 10, 0.5)
class(a)

# 10
a <- seq(1, 10)
class(a)

# 11
class(1L)

# 12
x <- c("1", "3", "5")
x_int <- as.integer(x)
x_int


## Section 04 - Sorting and ranking

# 1
# install.packages("dslabs")
library(dslabs)
data(murders)
pop <- murders$population
pop <- sort(pop) # sorts ascending by default
pop[1] # smallest value at the first position

# 2
pop <- murders$population
smallest_idx <- order(pop)
smallest_idx[1] # index with the smallest population
pop[smallest_idx[1]] # smallest population value

# 3
which.min(murders$population)

# 4
states <- murders$state
states[which.min(murders$population)]

# 5
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)

ranks <- rank(murders$population)
my_df <- data.frame(state = murders$state, rank = ranks)
head(my_df)


## Section 05 - Vector arithmetics

# 1
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
names(temp) <- city
temp_F <- 5/9 * (temp - 32)
temp_F

# 2
data("na_example")
str(na_example)
mean(na_example)
ind <- is.na(na_example)
table(ind)
# sum(ind)
# length(ind[ind])
mean(na_example[!ind])
# mean(na_example, na.rm = TRUE)