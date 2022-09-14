mypackages = c("leaps", "glmnet")   
tmp = setdiff(mypackages, rownames(installed.packages())) 
if (length(tmp) > 0) install.packages(tmp)

library(leaps)  # regsubsets
library(glmnet)  # glmnet for lasso

# generate data

# set random seed in case you want to reproduce the result
set.seed(542)  
n = 1000
x1 = rnorm(n)
x1 = rnorm(n)
x2 = rnorm(n)
e = rnorm(n)
# x3 is irrelevant as it depends on x1 and x2
x3 = 2/3 * x1 + 2/3 * x2 + 1/3 * e
epsilon = rnorm(n)
beta = c(2, 3)
y = beta[1] * x1 + beta[2] * x2 + epsilon
myData = data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

# Use AIC/BIC to select the best sub-model

IC = regsubsets(y ~ ., myData, method = "exhaustive")
sumIC = summary(IC)
sumIC$bic

sumIC

msize = apply(sumIC$which, 1, sum)
AIC = n * log(sumIC$rss/n) + 2 * msize
BIC = n * log(sumIC$rss/n) + log(n) * msize

# Both select the models with x1 and x2, so they selected the correct predictors
AIC; BIC

# Next, we use LASSO with lambda.min and lambda.1se 
# to check if it can select the correct model.
par(mfrow=c(1,1))

mylasso.cv = cv.glmnet(as.matrix(myData[, c('x1', 'x2', 'x3')]), as.vector(myData$y))

# big lambda penalizes too much and causes under fit (larger RSS)
# mylasso.cv = cv.glmnet(as.matrix(myData[, c('x1', 'x2', 'x3')]), 
                       #as.vector(myData$y), lambda = c(1, 2))
plot(mylasso.cv)

coef(mylasso.cv, s = 'lambda.1se')

coef(mylasso.cv, s = 'lambda.min')

# lasso doesn't drop x3 because unless lambda is 0
# it picks a small x3 instead.
mylasso = glmnet(as.matrix(myData[, c('x1', 'x2', 'x3')]), as.vector(myData$y))
par(mfrow = c(2, 1))
plot(mylasso, label=TRUE, xvar = "norm")
plot(mylasso, label=TRUE, xvar = "lambda")

# Compare prediction performance

N = 1000
mytestData = matrix(0, N, 4)
colnames(mytestData) = c("x1", "x2", "x3", "y")
mytestData = as.data.frame(mytestData)

mytestData$x1 = rnorm(N)
mytestData$x2 = rnorm(N)
mytestData$x3 = 2/3 * x1 + 2/3 * x2 + 1/3 * rnorm(N)
mytestData$y = beta[1] * mytestData$x1 + 
  beta[2] * mytestData$x2 + rnorm(N)


# For Lasso, you can form prediction using one of the following approaches:
#  Use the coefficients from lambda.1se;
#  Use the coefficients from lambda.min;
#  Refit a LS model using variables selected by lambda.1se;
#  Refit a LS model using variables selected by lambda.min.

tmp = predict(mylasso.cv, s="lambda.min", 
              newx=data.matrix(mytestData)[, -4])
mean((mytestData$y - tmp)^2)

tmp = predict(mylasso.cv, s="lambda.1se", 
              newx=data.matrix(mytestData)[, -4])
mean((mytestData$y - tmp)^2)

myfit.full = lm(y ~ ., myData)
tmp = predict(myfit.full, newdata=mytestData)
mean((mytestData$y - tmp)^2)

myfit.AIC = lm(y ~ x1 + x2, myData)
tmp = predict(myfit.AIC, newdata=mytestData)
mean((mytestData$y - tmp)^2)

## predictions on training data

tmp = predict(mylasso.cv, s="lambda.min", 
              newx=data.matrix(myData)[, 2:4])
mean((myData$y - tmp)^2)

tmp = predict(mylasso.cv, s="lambda.1se", 
              newx=data.matrix(myData)[, 2:4])
mean((myData$y - tmp)^2)

myfit.full = lm(y ~ ., myData)
tmp = predict(myfit.full, newdata=myData)
mean((myData$y - tmp)^2)












