options(digits=8)
options()$digits

library(mclust)
dim(faithful)
head(faithful)

Estep <- function(data, G, para){
  # Your Code
  # Return the n-by-G probability matrix
  X = data
  A = t(X)
  p = dim(X)[2]
  n = nrow(X)
  r_nG = matrix(0, n, G)
  inv_S = solve(para$Sigma)
  det_S = det(para$Sigma)
  
  for (k in 1:G) {
    A_mu_diff = A - para$mean[,k]
    # What I think is the density for component k
    g_den = exp(-1/2 * colSums((inv_S %*% A_mu_diff) * A_mu_diff)) / sqrt((2*pi)^p * det_S)
    r_n = para$prob[k] * g_den
    r_nG[,k]= r_n
  }
  
  # Normalize
  Z = r_nG / rowSums(r_nG)  
  return(Z)
}

Mstep <- function(data, G, para, post.prob){ 
  # Your Code
  # Return the updated parameters
  
  para.new <- list(prob = NULL, 
                mean = NULL, 
                Sigma = NULL, 
                loglik = NULL)
  
  X = as.matrix(data)
  A = t(X)
  n = nrow(X)
  p = dim(X)[2]
  
  # new prob
  para.new$prob = apply(post.prob, 2, mean)  
  
  # new mu
  norm_w = colSums(post.prob)  
  para.new$mean = t(t(A %*% post.prob) / norm_w)
  
  # new Sigma
  c = matrix(0, p, p)
  for (k in 1:G) {
    # R works column by column, this is easier with python
    X_ = t(A - para.new$mean[,k])
    # my brain does not understand this diagonal trick
    c =  c + (t(X_) %*% diag(post.prob[,k]) %*% X_)
  }
  
  para.new$Sigma = c / n
  
  return(para.new)
}

loglik <- function(data, G, para){
  # compute loglikelihood
  
  X = data
  A = t(X)
  inv_S = solve(para$Sigma)
  det_S = det(para$Sigma)
  loglikeli = 0
  
  for (k in 1:G) {
    A_mu_diff = A - para$mean[,k]
    # What I think is the log density for component k
    log_den = (-1/2 * log(det_S)) - (1/2 * colSums((inv_S %*% A_mu_diff) * A_mu_diff))
    logli_k = sum(log(para$prob[k]) + log_den)
    loglikeli = loglikeli + logli_k
  }
  
  return(loglikeli)
}

myEM <- function(data, itmax, G, para){
  # itmax: number of of iterations
  # G:     number of components
  # para:  list of (prob, mean, Sigma, loglik)
  
  for(t in 1:itmax){
    post.prob <- Estep(data, G, para)
    para <- Mstep(data, G, para, post.prob)
  }
  
  # update para$loglik
  para$loglik = loglik(data, G, para)
  
  return(para)
}

n <- nrow(faithful)
G <- 3
uin_4 = 2110
set.seed(uin_4)  # replace 234 by the last 4-dig of your University ID
gID <- sample(1:G, n, replace = TRUE)
Z <- matrix(0, n, G)
for(k in 1:G)
  Z[gID == k, k] <- 1 
msEst = mstep(modelName="EEE", faithful , Z)
ini0 <- msEst$parameters

para0 <- list(prob = ini0$pro, 
              mean = ini0$mean, 
              Sigma = ini0$variance$Sigma, 
              loglik = NULL)
para0

## Estep ##
X = faithful
A = t(X)
p = dim(X)[2]
r_nG = matrix(0, n, G)
inv_S = solve(para0$Sigma)
det_S = det(para0$Sigma)

for (k in 1:G) {
  A_mu_diff = A - para0$mean[,k]
  # What I think is the density for component k
  g_den = exp(-1/2 * colSums((inv_S %*% A_mu_diff) * A_mu_diff)) / sqrt((2*pi)^p * det_S)
  r_n = para0$prob[k] * g_den
  r_nG[,k]= r_n
}

# Normalize
z1 = r_nG / rowSums(r_nG)

z1 = Estep(faithful, G, para0)

# Validate
z1v = estep(modelName = msEst$modelName, data = faithful,
      parameters = msEst$parameters)$z

## Mstep ##

para1 <- list(prob = NULL, 
              mean = NULL, 
              Sigma = NULL, 
              loglik = NULL)

post.prob = z1
n = nrow(X)

# new prob
pi_prob = apply(post.prob, 2, mean)

# new mu
X = as.matrix(faithful)
norm_w = colSums(post.prob)
mu_pG = matrix(0, p, G)
k=1
for (k in 1:G) {
  mu_1p = post.prob[, k] %*% X
  mu_pG[, k] = mu_1p
}

para1$mean = t(t(mu_pG) / norm_w)

## trying no loop for mu
para1$mean = t(t(t(X) %*% post.prob) / norm_w)

# new cov

A = t(X)
k=1
c = matrix(0, p, p)
for (k in 1:G) {
  # R works column by column, this is easier with python
  X_ = t(A - para1$mean[,k])
  # my brain does not understand this diagonal trick
  c =  c + (t(X_) %*% diag(post.prob[,k]) %*% X_)
}

c / n

c = matrix(0, p, p)
for (k in 1:G) {
  for (i in 1:n) {
    a = X[i, ] - para1$mean[,k]
    b = a %*% t(a)
    c = c + (post.prob[i, k] * b)   
  }
}

newSigma = c / n

a=X[i, ] - para1$mean[,k]
b=a %*% t(a)
post.prob[i, k] * b

newPara = Mstep(faithful, G, para0, z1)

msEst2 = mstep(modelName="EEE", faithful , z1)
ini1 <- msEst2$parameters

ini1$variance$Sigma

# compute loglikelihood
X = faithful
A = t(X)
p = dim(X)[2]
inv_S = solve(newPara$Sigma)
det_S = det(newPara$Sigma)
loglikeli = 0
para = newPara

k=1
i=1
for (i in 1:n) {
  logli_k = 0
  for (k in 1:G) {
    a = as.matrix(X[i, ] - para$mean[,k])
    b = exp(a %*% inv_S %*% t(a) / -2) / sqrt((2*pi)^p * det_S)
    logli_k = logli_k + (para$prob[k]  * b) 
  }
  loglikeli = loglikeli + log(logli_k)
}

for (i in 1:n) {
  for (k in 1:G) {
    a = as.matrix(X[i, ] - para$mean[,k])
    b = sum(inv_S %*% t(a) * t(a))
    loglikeli = loglikeli + (para$prob[k] - (1/2 * log(det_S)) - (1/2 * b) )
  }
}


k=1
for (k in 1:G) {
  A_mu_diff = A - newPara$mean[,k]
  # What I think is the log density for component k
  log_den = (-1/2 * log(det_S)) - (1/2 * colSums((inv_S %*% A_mu_diff) * A_mu_diff))
  logli_k = sum(log(newPara$prob[k]) + log_den)
  loglikeli = loglikeli + logli_k
}