library(glmnet) 
library(pls)

myData = read.csv("Coding2_Data2.csv", header = TRUE)

X = data.matrix(myData[,-1])  
Y = myData[,1] 

T = 10
n = length(Y)
ntest = round(n * 0.25)  # test set size
ntrain = n - ntest  # training set size
all.test.id = matrix(0, ntest, T)  # 
for(t in 1:T){
  all.test.id[, t] = sample(1:n, ntest)
}

# Full Model

full_model = function(data, test.ids) {

  mse = rep(NaN, dim(test.ids)[2])
  for(i in 1:dim(test.ids)[2]) {
    test.id = test.ids[, i] 
    full.model = lm(Y ~ ., data = data[-test.id, ])
    Ytest.pred = predict(full.model, newdata = myData[test.id, ])
    mse[i] = mean((myData$Y[test.id] - Ytest.pred)^2)
  }
  
  return(mse)
}

full_model(myData, all.test.id)

# Ridge Regression

ridge_reg = function(X_data, Y_data, test.ids) {
  
  mylasso.lambda.seq = exp(seq(-10, 1, length.out = 100))
  
  mse = list(
    lam_min = rep(NaN, dim(test.ids)[2]),
    lam_1se = rep(NaN, dim(test.ids)[2])
  )
  
  for(i in 1:dim(test.ids)[2]) {  
    test.id = test.ids[, i]
    cv.out = cv.glmnet(X_data[-test.id, ], Y_data[-test.id], alpha = 0, 
                       lambda = mylasso.lambda.seq)
    
    best.lam = cv.out$lambda.min
    Ytest.pred = predict(cv.out, s = best.lam, newx = X_data[test.id, ])
    mse$lam_min[i] = mean((Y_data[test.id] - Ytest.pred)^2)
    
    best.lam = cv.out$lambda.1se
    Ytest.pred = predict(cv.out, s = best.lam, newx = X_data[test.id, ])
    mse$lam_1se[i] = mean((Y_data[test.id] - Ytest.pred)^2)
  }
  
  return(mse)
}

ridge_reg(X, Y, all.test.id)

# Lasso

lasso_reg = function(data, test.ids) {
  
  X_data = data.matrix(data[,-1])  
  Y_data = data[,1] 
  
  mse = list(
    lam_min = rep(NaN, dim(test.ids)[2]),
    lam_1se = rep(NaN, dim(test.ids)[2]),
    refit = rep(NaN, dim(test.ids)[2])
  )
  
  for(i in 1:dim(test.ids)[2]) {  
    test.id = test.ids[, i]
    cv.out = cv.glmnet(X_data[-test.id, ], Y_data[-test.id], alpha = 1)
    best.lam = cv.out$lambda.min
    Ytest.pred = predict(cv.out, s = best.lam, newx = X_data[test.id, ])
    mse$lam_min[i] = mean((Y_data[test.id] - Ytest.pred)^2)
    
    best.lam = cv.out$lambda.1se
    Ytest.pred = predict(cv.out, s = best.lam, newx = X_data[test.id, ])
    mse$lam_1se[i] = mean((Y_data[test.id] - Ytest.pred)^2)
    
    # Lasso refit
    mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
    var.sel = row.names(mylasso.coef)[which(mylasso.coef != 0)[-1]]
    mylasso.refit = lm(Y ~ ., data[-test.id, c("Y", var.sel)])
    Ytest.pred = predict(mylasso.refit, newdata = data[test.id, ])
    mse$refit[i] = mean((Ytest.pred - Y_data[test.id])^2)
  }
  
  return(mse)
}


lasso_reg(myData, all.test.id)

# PCR

pcr_reg = function(data, test.ids) {
  
  mse = rep(NaN, dim(test.ids)[2])
  
  for(i in 1:dim(test.ids)[2]) {
    test.id = test.ids[, i]
    mypcr = pcr(Y ~ ., data= data[-test.id, ], validation="CV")
    CVerr = RMSEP(mypcr)$val[1, , ]
    adjCVerr = RMSEP(mypcr)$val[2, , ]
    best.ncomp = which.min(CVerr) - 1 
    
    if (best.ncomp==0) {
      Ytest.pred = mean(data$Y[-test.id])
    } else {
      Ytest.pred = predict(mypcr, data[test.id,], ncomp=best.ncomp)
    }
    mse[i] = mean((Ytest.pred - data$Y[test.id])^2)
  }
  
  return(mse)
}

pcr_reg(myData, all.test.id)


cc = boxplot(df_results,  col = terrain.colors(dim(df_results)[2], alpha = 0.5))

for(i in 1:dim(df_results)[2]) {
  mses = df_results[, i]
  myjitter = jitter(rep(i, length(mses)), amount = 0.2)
  points(myjitter, mses, pch=20, col=rgb(0,0,0,.9))
}



