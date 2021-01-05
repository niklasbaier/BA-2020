# Business Analytics WS 2020/2021
# Introduction to R
#----------------------------------------------------------
# 1. Basics

# Basic Operations
1 + 4                         # commands can be run directly from the console.
2 + 6 * 5                     # comments are written using #
1:6                           # colon operator generates a sequence

# Variables
s <- 42                       # assign value 42 to variable s
b <- s*s                      # calculate and assign the value to b
b                             # print the value of variable b

s <- "Hello World!"           # assign a string to s
s                             # previous instruction overwrites the value of 42, let's print it

s = "Hello World!"            # can also use the = symbol

f = 17.42                     # assign a float
f*f                           # evaluating a term without assigning prints the result to console

# Functions
help("round")                 # displays the help page for the function round()
round(3.154)                  # round it to an integer
round(3.154, digits=2)        # round it to 2 decimal digits, 2 is an argument to the function
?factorial                    # this works similarly as the help() function
factorial(4)

# if else Statements
num <- -4
if (num < 0) {                # if the value is less than 0
  num <- num * -1             # do this
}
num

if (num %% 2) {               # check if num is odd or even
  "ODD"                       # do this if odd
} else {
  "EVEN"                      # else run the below code
}

# Looping
for (i in 11:20) {            # for loop
  print(i)
}

i <- 1
while (i<10){                 # while loop
  print(i)
  i <- i+1
}

# Write your own function
my_function <- function() {   # my_function is the function name
  s <- 5                      
  s*s                         # last line of the function body
}
my_function()                 # calling the function

my_square <- function(a) {    # a is the argument for this function
  a*a 
}
my_square(5) 						      # 5 is the value passed as argument

# Vectors
v1 = c(3, 5, 2)               # create vector, c: combine
v1[1]                         # address elements of vector
v1 = c(v1, 8, 3, 7, 5)        # extend vector
v1[1:3]                       # address several elements
v1 = v1*2                     # multiply by a scalar
v1[2] = 14                    # set second value
v2 = seq(from=0, to=12, by=2)  # sequence from 0 to 12 in steps of 1
v2
v3 = v1+v2                    # vector addition
v3

# Matrices
m <- matrix(c(9,2,5,3,1,8), ncol=3, nrow=2)     # create matrix
m                             # print matrix
m[1,2]                        # address element in first row and second column
m[2,]                         # address whole row
m[2,3] = 14                   # address whole row
dim(m)                        # dimensions of matrix m

# Factors
eye_color <- factor(c("blue", "brown", "black", "green", "brown", "blue"))  # create a factor
eye_color
unclass(eye_color)            # show how R stores this factor

# Data Frames
n <- c("Alice", "Bob", "Charlie")
a <- c(25, 27, 23)
u <- factor(c("TUM", "LMU", "TUM"))

# create a dataframe with column names - names, age and uni and corresponding data
df <- data.frame(names=n, age=a, uni=u) 		
df

df[3]										      # address third column
df$uni                        # address column by name
df[c(1,3)]                    # select columns 1 and 3
df$by = 2020 - df$age         # create new column for birthyear
df$by
df$age = NULL                 # remove column age
df[df$by < 1994, ]            # filter rows
df[order(df$by),]             # sort in order with column "by"

# Install the package (in case you haven't already)
#install.packages("tidyverse")        # uncomment this line to install
# Load the package
library(tidyverse)

# Tibbles
tib_data <- as_tibble(df)     # convert dataframe df to tibble
tib_data                      # view the tibble
df$b                          # partial matching in dataframe
tib_data$b                    # no partial matching
tib_data[["uni"]]             # similar to tib_data$uni
tib_data[[2]]                 # similar to tib_data$uni

#-------------------------------------------------------
# 2. Data Import & Export

# readr, a tidyverse package
getwd()                                       # get working directory path
setwd()                                       # set path to required directory
testdata <- read_csv("input.csv")             # read csv
write_csv(testdata, "same_as_input.csv")      # write data to csv file

#-------------------------------------------------------
# 3. Data Exploration

data <- as_tibble(iris)           # convert iris data into a tibble
data

glimpse(data)                     # structure of the data set
summary(iris)                     # summary of data
names(data)                       # attribute/column names
ncol(data)                        # number of columns(attributes)
nrow(data)                        # number of rows(observations)
dim(data)                         # dimensions (#rows and #columns)
head(data)                        # return the first few observations from the data
tail(data)                        # return the last few observations from the data

#--------------------------------------------------------
# 4. Basic Plotting

# Plot the relationship between petal length and petal width
plot(iris$Petal.Length, iris$Petal.Width)

# Plot with different colors for species
plot(iris$Petal.Length, iris$Petal.Width, pch=as.numeric(iris$Species), col=c("green3", "red", "blue")[as.numeric(iris$Species)])

cor(iris$Petal.Length, iris$Petal.Width)

pairs(iris)

cor(iris[1:4])

hist(iris$Sepal.Length)       # histogram

# Histogram with 20 breaks and density
hist(iris$Sepal.Length, breaks=20, freq=FALSE)

# Plotting with ggplot

# Create a scatter plot with ggplot2
# First ingredient: data and mappings of features to "aesthetics")
ggplot(data=data, 
       aes(x=Sepal.Length, y=Sepal.Width, color=Species, shape=Species)) +
  # Second ingredient: type of plot ("geometry"), "point" refers to scatterplots
  geom_point() +
  # Third: labels and titles
  xlab("Sepal Length") + ylab("Sepal Width") +
  ggtitle("Iris - Sepal Length vs. Width") +
  # Lastly: Choose a visual style
  theme_minimal()

# ... and many more options, not shown here.

#-------------------------------------------------------
# 5. Descriptive Statistics

mean(data$Sepal.Length)                     # calculate mean of the column
var(data$Sepal.Length)                      # calculate variance
sd(data$Sepal.Length)                       # calculate standard deviation
cov(data$Petal.Length, data$Petal.Width)    # calculate covariance
cor(data$Petal.Length, data$Petal.Width)    # calculate correlation
cor(data[1:4])                              # calculate correlation matrix

#-------------------------------------------------------
# 6. Data Transformation

# dplyr

data <- as_tibble(quakes)     # we will work with quakes dataset
?quakes                       # this will show the details of the dataset
nrow(data)                    # number of observations in quakes dataset

# Filtering
d <- filter(data, mag > 5, stations > 20)     # filter all earthquakes with magnitude greater than 5 and reported by more than 20 stations.
nrow(d)                       # number of filtered observations

d <- filter(data, mag > 6 | stations > 60)    # magnitude greater than 6 or stations reporting is greater than 60
nrow(d)

# Arrange
d <- slice(data, 1:4)                 # let us work with subset of the data
d                                     # view the subset
arrange(d, desc(mag), stations)       # arrange the rows in descending order of "mag" and, in case of ties, ascending order of "stations"

# Select
data <- as_tibble(quakes)
glimpse(data)                           # display the structure of quakes
glimpse(select(data, lat, long))        # select columns lat and long
glimpse(select(data, lat:depth))        # select columns from lat until depth
glimpse(select(data, -(lat:depth)))     # select columns other than columns from lat until depth
glimpse(select(data, starts_with("l"))) # select all columns starting with "l"
# other helpers can be found in ?select
glimpse(rename(data, latitude=lat, longitude=long))   # rename the columns

# Mutate
mutate(data, height = 0-depth)           # add a new column height

# Summarise

# compute the mean and median of mag and stations columns respectively
summarize(data, mean_mag=mean(mag), median_stations=median(stations))     

# compute sd and min of mag and stations columns respectively
summarize(data, sd_mag=sd(mag), min_stations=min(stations))

# other possible functions like mean, min etc are available at ?summarise

# Grouping
grouped <- group_by(data, mag)      # group the data by mag
# compute the mean of stations column for every group
summarize(grouped, mean_stations=mean(stations))    


# Piping

# Method 1 - Intermediate Results
temp <- filter(data, mag > 5)
result <- summarize(temp, mean_stations=mean(stations))
result							           # one way to perform the operations

# Method 2 - Nesting Functions
summarize(
  filter(
    data, mag > 5
  ),
  mean_stations=mean(stations)
)

# Method 3 - Using Pipes
data %>% 
  filter(mag > 5) %>%
  summarize(mean_stations=mean(stations))