### BUSINESS ANALYTICS ###
### Homework Sheet 12


## Homework 12.4

# Perform one backward pass through the network of the previous exercise (12.3) and update the parameters using gradient descent.
# Use learning rate alpha = 1.

library(tidyverse)
# only needed for plotting

options(digits = 3, scipen = 5)
# no scientific notation for common numbers

sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

d_sigmoid <- function(x) {
  sigmoid(x) * (1 - sigmoid(x))
}

# Data Matrix
X = matrix(c(0, 0, 0, 1, 1, 0, 1, 1), nrow = 2)
X

# True labels
Y = matrix(c(0, 1, 1, 1), nrow = 1)
Y

data <- data.frame(x1 = X[1,], x2 = X[2,], label = Y[1,])
data

# Element-wise loss function
# y and y_hat should be row vectors of equal length
# Note that this functino fails for y_hat values of exactly 0 or 1, but those will never be produced by a sigmoid activatio function
loss <- function(y_hat, y) {
  l = -(y * log(y_hat) + (1 - y) * log(1 - y_hat))
  assign("l", l, envir = globalenv())
  # save l in gloval environment for later use
  return(l)
}

# Empirical risk function
# y and y_hat should be row vectors of equal length
risk <- function(y_hat, y) {
  L = sum(loss(y_hat, y)) / length(y_hat)
  assign("L", L, envir = globalenv())
  # save L in global environment for later use
  return(L)
}

# Initialize neural net parameters
# layer 1: 2 hidden nodes --> W1 is 2x2, b1 is 2x1
W1 = matrix(c(1, 0, 0, 1), nrow = 2)
b1 = matrix(c(0, 0))

# layer 2: 1  node --> W2 is 2x1, b2 is 1x1
W2 = matrix(c(1, -1), nrow = 1)
b2 = matrix(c(0))

# Define feed forward pass
forward <- function(x) {
  Z1 = sweep(W1 %*% x, 1, -b1) # read: W1X + b1
  A1 = sigmoid(Z1)
  Z2 = sweep(W2 %*% A1, 1, -b2)
  A2 = sigmoid(Z2)
  
  
  # update values in global environment for later use in backward pass
  assign('A0', x, envir = globalenv()) # avoid possible name conflict w/ 'x'
  assign('Z1', Z1, envir = globalenv())
  assign('A1', A1, envir = globalenv())
  assign('Z2', Z2, envir = globalenv())
  assign('A2', A2, envir = globalenv())
  
  return(A2)
}

# Define backward pass
backward <- function(y, learning_rate = 1) {
  n = length(y)
  dZ2 = A2 - y
  dW2 = (dZ2 %*% t(A1)) / n
  db2 = rowSums(dZ2) / n
  dZ1 = t(W2) %*% dZ2 * (A1 * (1 - A1))
  dW1 = (dZ1 %*% t(A0))
  db1 = rowSums(dZ1) / n
  
  # update parameters
  assign('W1', W1 - learning_rate * dW1,  envir = globalenv())
  assign('b1', b1 - learning_rate * db1,  envir = globalenv())
  assign('W2', W2 - learning_rate * dW2,  envir = globalenv())
  assign('b2', b2 - learning_rate * db2,  envir = globalenv())
}

# function for plotting nn outputs (you don't need to understand this)
plot_nn <- function(data = NULL){
  grid <- as_tibble(expand.grid(data.frame(x1 = seq(0, 1, by = .01), x2 = seq(0, 1, by = .01))))
  grid$y_hat = as.numeric(forward(t(grid %>% select(x1, x2))))
  
  g <- ggplot(grid, aes(x = x1, y = x2, col = y_hat)) + 
    geom_point() + theme_minimal() +
    coord_equal() + 
    scale_color_viridis_c()
  
  if (!is.null(data)) {
    data$label <- factor(data$label)
    g <- g + geom_point(data = data, aes(x = x1, y = x2, shape = label), col = 'red', size = 4)
  }
  
  g
}

plot_nn(data = data)

# train the neural net --> 100 iterations of gradient descent
for(i in 1:100){
  print(risk(forward(X), Y))
  backward(Y)
}

forward(X)

plot_nn(data = data)


# Let's try a somewhat more complicated dataset.
# Can our neural network learn to correctly classify it?
newX = matrix(c(0, 0, 0, .5, 0, 1, .5, 0, .5, .5, .5, 1, 1, 0, 1, .5, 1, 1), nrow = 2)
newY = matrix(c(0, 0, 1, 1, 1, 0, 0, 0, 1), nrow = 1)
newdata <-data.frame(x1 = newX[1,], x2 = newX[2,], label = newY[1,])

plot_nn(data = newdata)

for(i in 1:1000){
  print(risk(forward(newX), newY))
  backward(newY)
}
forward(newX)

plot_nn(data = newdata)


# How about if we add more nodes in the hidden layer? (10 instead of 2)

W1 = matrix(rnorm(20), nrow = 10)
b1 = matrix(rnorm(10), nrow = 10)
W2 = matrix(rnorm(10), nrow = 1)
b2 = matrix(rnorm(1), nrow = 1)

plot_nn(data = newdata)

for(i in 1:1000){
  print(risk(forward(newX), newY))
  backward(newY)
}
forward(newX)

plot_nn(data=newdata)