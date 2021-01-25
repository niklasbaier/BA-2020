### BUSINESS ANALYTICS ###
### Homework Sheet 10


## Homework 10.3 Step by step computation

# In this exercise you will compute PCAs using primitive R functions, trying to mimic the PCA computation algorithm.
# We will use the built-in iris dataset as in Week 1.

# a) Check the structure of the dataset. Which attributes are numerical? 
#    Compute a correlation matrix for them.
str(iris)

num_iris = iris[1:4]
cor(num_iris)

# b) Construct a new matrix consisting only of numerical attributes with mean values deducted.
m = colMeans(num_iris)
num_iris[1] = num_iris[1] - rep(m[1], 150)
num_iris[2] = num_iris[2] - rep(m[2], 150)
num_iris[3] = num_iris[3] - rep(m[3], 150)
num_iris[4] = num_iris[4] - rep(m[4], 150)

# c) Calculate covariance matrix, eigenvectors and eigenvalues.
c = cov(num_iris)

# d) Compute the scores by multiplying the transposed eigenvectors matrix and the zero mean matrix transposed.
#    Check the first 6 scores.
eigen(c)
vectors = data.matrix(eigen(c)$vectors)

newdata = t(vectors) %*% t(data.matrix(num_iris))
t(newdata[, 1:10])


# Homework 10.4 

# The code from Homework 10.3 is cumbersome and long. In this exercise you will use the R function princomp() to get the same result with less effort.
# Use help() to learn more about the syntax and output value. 

# a) Use princomp() function in order to compute PCA's for the dataset.
pc <- princomp(iris[, 1:4], scores = TRUE)

# b) How much standard deviation does each component have? How much variance is explained by the first component?
summary(pc)
plot(pc, type = "lines")

# c) Check the computed eigenvectors. Are they equal to the one computed in Exercise 10.3? 
#    Hint: values lower than 0.1 are hidden by default. Use print() with appropriate parameters to display all values.
loadings(pc)

print(loadings(pc), digits = 3, cutoff = 0.001)

# d) Check the first 6 scores for the dataset. Compare them with the scores computed in Exercise 10.3.
head(pc$scores)

# e) Plot the projection of the dataset points on a space consisting of two principal components only. 
#    Use biplot() for that.
biplot(pc)