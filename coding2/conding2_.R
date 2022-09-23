myData = read.csv("Coding2_Data.csv")
X = as.matrix(myData[, -14])
y = myData$Y
lam.seq = exp(seq(-1, -8, length.out = 80))

one_var_lasso = function(r, z, lam) {
  
  ##############################
  # YOUR CODE
  ##############################
  n = length(y)
  
  z_len = t(z) %*% z
  a_num = t(r) %*% z
  
  a = a_num / z_len
  lam_ = (2*n*lam) / z_len
  a_sign = sign(a)
  
  ## derivation cases
  x = a + lam_ / 2
  if(a > lam_ / 2) {
    x = a - lam_ / 2
  } 
  
  if(abs(a) <= lam_ / 2) {
    x = 0
  }
  
  ## select same sign as a
  if((a < 0 && x > 0) || (a > 0 && x < 0)) {
    x = x * -1
  } 
  
  return(x)
  
}

MyLasso = function(X, y, lam.seq, maxit = 100) {
  
  # Input
  # X: n-by-p design matrix without the intercept 
  # y: n-by-1 response vector 
  # lam.seq: sequence of lambda values (arranged from large to small)
  # maxit: number of updates for each lambda 
  
  # Output
  # B: a (p+1)-by-length(lam.seq) coefficient matrix 
  #    with the first row being the intercept sequence
  
  n = length(y)
  p = dim(X)[2]
  nlam = length(lam.seq)
  B = matrix(0, ncol = nlam, nrow = (p+1))
  rownames(B) = c("Intercept", colnames(X)) 
  
  ##############################
  # YOUR CODE: 
  # (1) new.X = centered & scaled X; 
  # (2) record the centers and scales used in (1) 
  ##############################
  
  ones = rep(1, n)
  X_means = colMeans(X)
  Xn_means = ones %*% t(X_means)
  X_centered = X - Xn_means
  
  X_sds = apply(X, 2, sd)
  Xn_sds = ones %*% t(X_sds)
  new.X = X_centered / Xn_sds
  
  #print(apply(new.X, 2, sd))
  #print(summary(new.X))
  
  # Initialize coef vector b and residual vector r
  b = rep(0, p)
  r = y
  
  # Triple nested loop
  for (m in 1:nlam) {
    for (step in 1:maxit) {
      for (j in 1:p) {
        r = r + (new.X[, j] * b[j])
        b[j] = one_var_lasso(r, new.X[, j], lam.seq[m])
        r = r - new.X[, j] * b[j]
      }
    }
    B[-1, m] = b
  }

  
  ##############################
  # YOUR CODE:
  # scale back the coefficients;
  # update the intercepts stored in B[1, ]
  ##############################
  
  B[-1, ] = B[-1, ]  / X_sds
  inter_=colSums(B[-1,] * -X_means)
  B[1,] = mean(y) + inter_
  
  return(B)
}

B = MyLasso(X, y, lam.seq, maxit = 100)

j=9
m=1
r = r + (new.X[, j] * b[j])
z = new.X[, j]
lam = lam.seq[m]

z_len = t(z) %*% z
a_num = t(r) %*% z

a = a_num / z_len
lam_ = (2*n*lam) / z_len
a_sign = sign(a)

## derivation cases
x = a + lam_ / 2
if(a > (lam_ / 2)) {
  x = a - lam_ / 2
} 

if(abs(a) <= lam_ / 2) {
  x = 0
}

## select same sign as a
if((a < 0 && x > 0) || (a > 0 && x < 0)) {
  x = x * -1
} 

#b[j] = one_var_lasso(r, new.X[, j], lam.seq[m])

myData = read.csv("Coding2_Data.csv")
X = as.matrix(myData[, -14])
y = myData$Y
lam.seq = exp(seq(-1, -8, length.out = 80))
myout = MyLasso(X, y, lam.seq)

library(glmnet)
lasso.fit = glmnet(X, y, alpha = 1, lambda = lam.seq)
max(abs(coef(lasso.fit) - myout))

