options(digits=8)
options()$digits

library(mclust)
dim(faithful)

Estep <- function(data, G, para) {
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

Mstep <- function(data, G, para, post.prob) { 
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

loglik <- function(data, G, para) {
  # compute loglikelihood
  
  X = data
  A = t(X)
  p = dim(X)[2]
  inv_S = solve(para$Sigma)
  det_S = det(para$Sigma)
  loglikeli = 0
  
  for (i in 1:n) {
    logli_k = 0
    for (k in 1:G) {
      a = as.matrix(X[i, ] - para$mean[,k])
      b = exp(a %*% inv_S %*% t(a) / -2) / sqrt((2*pi)^p * det_S)
      logli_k = logli_k + (para$prob[k]  * b) 
    }
    loglikeli = loglikeli + log(logli_k)
  }
  
  return(loglikeli)
}

myEM <- function(data, itmax, G, para) {
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
G <- 2
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

myEM(data=faithful, itmax=20, G=G, para=para0)

Rout <- em(modelName = "EEE", data = faithful,
           control = emControl(eps=0, tol=0, itmax = 20), 
           parameters = ini0)

list(prob = Rout$para$pro, 
     mean = Rout$para$mean, 
     Sigma = Rout$para$variance$Sigma, 
     loglik = Rout$loglik)
