library(pls)

myData = read.csv("Coding3_dataH.csv", header=FALSE)
dim(myData)
View(myData)

n = dim(myData)[1]
ntest = round(n * 0.75)  # test set size
ntrain = n - ntest

uin_4 = 2110
set.seed(uin_4)

test.ids = sample(1:n, ntest)
test = myData[test.ids,]
train = myData[-test.ids,]


ridgeless = function(train, test, eps = 1e-10){
  Xtrain = train[, -1]
  Ytrain = train[, 1]
  Xtest = test[, -1]
  Ytest  = test[, 1]
  
  Xtrain = scale(Xtrain, center = TRUE, scale = FALSE)
  Xmean = attr(Xtrain, "scaled:center")
  Xtest = scale(Xtest, Xmean, scale = FALSE)
  
  ##############################################
  # Your code for computing Ytrain.hat and Ytest.hat
  ##########################################
  
  s = svd(Xtrain)
  
  # Only d
  d = sum(s$d > eps)
  
  # s$d is of len n because on Xtrain n < p 
  # so in this case there are only n principal components of X
  # D would be of n x n, but we only keep d singular values of X
  D_dd = diag(s$d[1:d])
  # n * d
  U_nd = s$u[, 1:d]
  # p x d
  V_pd = s$v[, 1:d]
  
  # 2 ways to get the principal components of Xtrain
  # F_nd = U_nd %*% D_dd
  F_nd = Xtrain %*% V_pd
  
  # Seems like since p > n, the design matrix only has n by d
  #A_hat = t(F_nd) %*% Ytrain / colSums(F_nd^2)
  A_hat = t(F_nd) %*% Ytrain / colSums(D_dd^2)
  intercept = mean(Ytrain)
  
  Ytrain.hat = F_nd %*% A_hat + intercept
  mean((Ytrain - Ytrain.hat)^2)
  
  # Fit lm model for reference 
  lm_mod = lm(V1 ~ ., data = train)
  
  B_hat = V_pd %*% A_hat 
  
  # prediction
  Ytest.hat = Xtest %*% B_hat + intercept  
  
  return(list(
    train.err = mean((Ytrain - Ytrain.hat)^2), 
    train.lm.err = mean((Ytrain - lm_mod$fitted.values)^2),
    test.err = mean ((Ytest - Ytest.hat)^2)
  ))
}

errs = ridgeless(train, test)
train = train[, 1:6]
test = test[, 1:6]
ridgeless(train, test)

train = train[, 1:150]
test = test[, 1:150]
ridgeless(train, test)

T = 30
n = dim(myData)[1]
ncols = dim(myData)[2]
sim_results = matrix(nrow = T, ncol = ncols - 5)

for(t in 1:T) {

  ntest = round(n * 0.75)  # test set size
  ntrain = n - ntest
  
  test.ids = sample(1:n, ntest)
  test = myData[test.ids,]
  train = myData[-test.ids,]  
  
  for(d in 6:ncols) {
    #print(dim(train[, 1:d]))
    errs = ridgeless(train[, 1:d], test[, 1:d])
    sim_results[t, d-5] = errs$test.err
  }
}

log_median_errs = log(apply(sim_results, 2, median))

plot(1:(ncols-5), log_median_errs, xlab='# of features', 
     ylab='Log Median Test Error', col="blue", cex=1.5)


cbind(Ytest, Ytest.hat)
mean((Ytest - Ytest.hat)^2)

