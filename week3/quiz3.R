# Q1

library(leaps)  # regsubsets
library(glmnet)  # glmnet for lasso

# generate data

# set random seed in case you want to reproduce the result
p = 3
# set.seed(542)  
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)
e = rnorm(n)
# x3 is irrelevant as it depends on x1 and x2
# x3 = rnorm(n)
x3 = 2/3 * x1 + 2/3 * x2 + 1/3 * e
epsilon = rnorm(n)
beta = c(3, 1, 2, 0)
y = beta[1] + beta[2] * x1 + beta[3] * x2 + beta[4] * x3 + epsilon
myData = data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

b = regsubsets(y ~ ., data=myData, method = "exhaustive")
rs = summary(b)
# Best model after adding 1,2,3...p predictors.
rs$which
b$rss

coef(b, 1:3)

m1 = lm(y ~ x1, data = myData)
m13 = lm(y ~ x1 + x3, data = myData)
m2 = lm(y ~ x2, data = myData)
m23 = lm(y ~ x2 + x3, data = myData)
m123 = lm(y ~ x1 + x2 + x3, data = myData)
m12 = lm(y ~ x1 + x2, data = myData)

##
N = 1000
mytestData = matrix(0, N, 4)
colnames(mytestData) = c("x1", "x2", "x3", "y")
mytestData = as.data.frame(mytestData)

mytestData$x1 = rnorm(N)
mytestData$x2 = rnorm(N)
mytestData$x3 = 2/3 * x1 + 2/3 * x2 + 1/3 * e
mytestData$y = beta[1] + beta[2] * mytestData$x1 + beta[3] * mytestData$x2 + 
  beta[4] * mytestData$x3 + rnorm(N)

p1 = predict(m1, newdata = mytestData)
p13 = predict(m13, newdata = mytestData)
p2 = predict(m2, newdata = mytestData)
p23 = predict(m23, newdata = mytestData)
p123 = predict(m123, newdata = mytestData)
p12 = predict(m12, newdata = mytestData)

(sum((mytestData$y - p1)^2))
(sum((mytestData$y - p13)^2))
(sum((mytestData$y - p2)^2))
(sum((mytestData$y - p23)^2))
(sum((mytestData$y - p123)^2))
(sum((mytestData$y - p12)^2))


## Q4

par(mfrow=c(1,1))
mylasso.cv = cv.glmnet(as.matrix(myData[, c('x1', 'x2', 'x3')]), 
                       as.vector(myData$y), alpha = 1)
plot(mylasso.cv)

coef(mylasso.cv, s = 'lambda.1se')
coef(mylasso.cv, s = 'lambda.min')

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

## Q7

myData = data.frame(y = y, x1 = x1, x2 = x2)
X = as.matrix(myData[, c('x1', 'x2')])
Y = as.vector(myData$y)

rid1 = glmnet(X, Y, alpha = 0, lambda = 0.5, standardize = FALSE)
coef.glmnet(rid1)

# we double x1
X[,1] = 2 * X[,1]
rid2 = glmnet(X, Y, alpha = 0, lambda = 0.5, standardize = FALSE)
coef.glmnet(rid2)

# beta1_hat changes but not proportionally as in OLS, this is due to
# the shrinkage effect. 
coef.glmnet(rid2)[2] * 2 == coef.glmnet(rid1)[2]
# also, the other betas_hat will change
coef.glmnet(rid2)[1] == coef.glmnet(rid1)[1]
coef.glmnet(rid2)[3] == coef.glmnet(rid1)[3]

## Q8 

myData = data.frame(y = y, x1 = x1, x2 = x2)
X = as.matrix(myData[, c('x1', 'x2')])
Y = as.vector(myData$y)

rid1 = glmnet(X, Y, alpha = 0, lambda = 0.5, standardize = TRUE)
coef.glmnet(rid1)

# we double x1
X[,1] = 2 * X[,1]
rid2 = glmnet(X, Y, alpha = 0, lambda = 0.5, standardize = TRUE)
coef.glmnet(rid2)

# beta1_hat changes proportionally as in OLS, due to standardize = TRUE
coef.glmnet(rid2)[2] * 2 == coef.glmnet(rid1)[2]
# the other features stay the same
coef.glmnet(rid2)[1] == coef.glmnet(rid1)[1]
coef.glmnet(rid2)[3] == coef.glmnet(rid1)[3]

## Q10

myData = data.frame(y = y, x1 = x1, x2 = x2)

b = regsubsets(y ~ ., data=myData)
(rs = summary(b))
rs$bic
rs$cp

myData[, 'x1'] = myData[, 'x1'] * 2
b = regsubsets(y ~ ., data=myData)
(rs = summary(b))
rs$bic
rs$cp

## Q11

prostate = read.table("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data",
                      header = TRUE)
names(prostate)
traindata = prostate[prostate$train==TRUE,][, -length(names(prostate))]
testdata = prostate[prostate$train==FALSE,][, -length(names(prostate))]
dim(traindata)
dim(testdata)

mod1 = lm(lpsa ~ ., data = traindata)

preds = predict(mod1, newdata = testdata)

round(sum((testdata$lpsa - preds)^2), 2)


## Q12,Q13

smod = summary(regsubsets(lpsa ~ ., data=traindata, method = "exhaustive"))
smod
smod$which
smod$cp
smod$bic

n = dim(traindata)[1]
p = dim(traindata)[2]-1
  
msize = apply(smod$which, 1, sum)
AIC = n * log(smod$rss/n) + 2 * msize
BIC = n * log(smod$rss/n) + log(n) * msize

# Using my own aic/bic calculation
smod$which[which.min(AIC),]
smod$which[which.min(BIC),]

sum(smod$which[which.min(AIC),])-1
sum(smod$which[which.min(BIC),])-1

# Using aic/bic from summary
smod$which[which.min(smod$cp),]
smod$which[which.min(smod$bic),]

sum(smod$which[which.min(smod$cp),])-1
sum(smod$which[which.min(smod$bic),])-1

## Q14

aic_predictors = smod$which[which.min(AIC), 2:(p+1)]
aic_data = traindata[, aic_predictors]
aic_lm = lm(lpsa ~ ., data = aic_data)
aic_preds = predict(aic_lm, newdata = testdata)
sum((testdata$lpsa - aic_preds)^2)

bic_predictors = smod$which[which.min(BIC), 2:(p+1)]
bic_data = traindata[, bic_predictors]
bic_lm = lm(lpsa ~ ., data = bic_data)
bic_preds = predict(bic_lm, newdata = testdata)
sum((testdata$lpsa - bic_preds)^2)





















