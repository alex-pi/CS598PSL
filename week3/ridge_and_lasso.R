mypackages = c("MASS", "glmnet")   # required packages
tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
if (length(tmp) > 0) install.packages(tmp)
lapply(mypackages, require, character.only = TRUE)
set.seed(2134)


## Prepare data

myData = Boston
names(myData)[14] = "Y"
iLog = c(1, 3, 5, 6, 8, 9, 10, 14);
myData[, iLog] = log(myData[, iLog]);
myData[, 2] = myData[, 2] / 10;
myData[, 7] = myData[, 7]^2.5 / 10^4
myData[, 11] = exp(0.4 * myData[, 11]) / 1000;
myData[, 12] = myData[, 12] / 100;
myData[, 13] = sqrt(myData[, 13]);

# Move the last column of myData, the response Y, to the 1st column.
myData = data.frame(Y = myData[,14], myData[,-14]);
names(myData)[1] = "Y";
names(myData)

n = dim(myData)[1]; 
p = dim(myData)[2]-1;
X = as.matrix(myData[, -1]);  # some algorithms need the matrix/vector 
Y = myData[, 1];

## Split the data into two parts: 80% for training and 20% for testing
ntest = round(n*0.2)
ntrain = n - ntest;
test.id = sample(1:n, ntest);
Ytest = myData[test.id, 1];

## Full Model

full.model = lm( Y ~ ., data = myData[-test.id, ]);  
Ytest.pred = predict(full.model, newdata= myData[test.id, ]);
sum((Ytest - Ytest.pred)^2)/ntest # averaged MSE on the test set

######### Ridge using glmnet; lambda chosen by 10-fold CV ##########

myridge = glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
plot(myridge, label = TRUE, xvar = "lambda")

## Check the output from glmnet for ridge regression.

summary(myridge)
length(myridge$lambda)  # retrieve the lambda value
dim(myridge$beta)       # coefficients for 13 non-intercept predictors
length(myridge$a0)      # intercept

# the 13 coefficients (including intercept) can also be retrieved using
# coef(myridge)
dim(coef(myridge))

# The two coefficient matrices should be the same
sum((coef(myridge) - rbind(myridge$a0, myridge$beta))^2)

## Ridge regression coefs could change sign along the path

round(myridge$beta[8, ], dig = 2)

## How are the intercepts computed?

k = 2; 

# convoluted way for colMeans(X[-test.id, ])
my.mean = apply(X[-test.id, ], 2, mean)  # 13x1 mean vector for training X
mean(Y[-test.id]) - sum(my.mean * myridge$beta[, k])

myridge$a0[k]  # intercept for lambda = myridge$lambda[k]

# Check whether our intercept formula is true for all intercepts 
# should be close to 0
sum((mean(Y[-test.id]) - my.mean %*% myridge$beta  - myridge$a0)^2)

## Selection lambda by 10-fold CV. The CV results are stored in

cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0) 
plot(cv.out)

# mean CV error
cv.out$cvm

# estimate of standard error of cvm
cv.out$cvsd

#lambda.min: the value of lambda that gives the minimum cvm
#lambda.1se: the largest value of lambda 
#   (i.e., the largest regularization, the smallest df) 
#   whose cvm is within 1 standard error of the cvm of lambda.min.

## Using our own lambda sequence

lam.seq = exp(seq(-6, 2, length=100))
cv.out = cv.glmnet(X[-test.id,], Y[-test.id], alpha=0, lambda=lam.seq)  
plot(cv.out)

## lambda with smallest cvm
cv.out$lambda[which.min(cv.out$cvm)]
cv.out$lambda.min

## lambda.1se lambda within 1 cvm standard error from lambda.min

tmp.id = which.min(cv.out$cvm)
max(cv.out$lambda[cv.out$cvm < cv.out$cvm[tmp.id] + cv.out$cvsd[tmp.id]])
cv.out$lambda.1se

## Evaluate prediction performance

myridge = glmnet(X[-test.id,], Y[-test.id], alpha=0, lambda = lam.seq)
Ytest.pred = predict(myridge, s = cv.out$lambda.1se, newx=X[test.id,])
mean((Ytest.pred - Y[test.id])^2)

Ytest.pred=predict(myridge, s = cv.out$lambda.min, newx=X[test.id,])
mean((Ytest.pred - Y[test.id])^2)


########## Lasso using glmnet; lambda chosen by 10-fold CV. ##########

mylasso = glmnet(X[-test.id,], Y[-test.id], alpha = 1)
summary(mylasso)

par(mfrow = c(1, 2))
plot(mylasso, label=TRUE, xvar = "norm")
plot(mylasso, label=TRUE, xvar = "lambda")

par(mfrow=c(1,1))
mylasso$df

cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
plot(cv.out)

## Try our own lambda sequences.

lam.seq =  exp(seq(-4, 2, length=100))
#lam.seq =  exp(seq(-6, -1, length=100))
cv.out2 = cv.glmnet(X[-test.id,], Y[-test.id], alpha = 1, lambda = lam.seq)
plot(cv.out2)

## Check how lambda.min and lambda.1se are computed.

cv.out$lambda.min
tmp.id=which.min(cv.out$cvm)
cv.out$lambda[tmp.id]

cv.out$lambda.1se
max(cv.out$lambda[cv.out$cvm < cv.out$cvm[tmp.id] + cv.out$cvsd[tmp.id]])

## Retrieve Lasso coefficients.

mylasso.coef.min = predict(mylasso, s=cv.out$lambda.min, type="coefficients")
mylasso.coef.1se = predict(mylasso, s=cv.out$lambda.1se, type="coefficients")
cbind(mylasso.coef.min, mylasso.coef.1se)

# number of variables selected (including the intercept)
sum(mylasso.coef.1se != 0)

# names of selected non-intercept variables
# row.names(mylasso.coef.1se)[nonzeroCoef(mylasso.coef.1se)[-1]]
row.names(mylasso.coef.1se)[which(mylasso.coef.1se != 0)][-1]

## Apply the fitted model for prediction on the test data.

mylasso = glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
Ytest.pred = predict(mylasso, s = cv.out$lambda.min, newx = X[test.id,])
mean((Ytest.pred - Y[test.id])^2)

Ytest.pred = predict(mylasso, s = cv.out$lambda.1se, newx = X[test.id,])
mean((Ytest.pred - Y[test.id])^2)

## Refit the model selected by Lasso to reduce bias.

# Variables selected by lambda.1se 
mylasso.coef.1se = predict(mylasso, s = cv.out$lambda.1se, type="coefficients")
var.sel = row.names(mylasso.coef.1se)[which(mylasso.coef.1se != 0)][-1]

var.sel; 

tmp.X = X[, colnames(X) %in% var.sel]
mylasso.refit = coef(lm(Y[-test.id] ~ tmp.X[-test.id, ]))
Ytest.pred = mylasso.refit[1] + tmp.X[test.id,] %*% mylasso.refit[-1]
mean((Ytest.pred - Y[test.id])^2)

























