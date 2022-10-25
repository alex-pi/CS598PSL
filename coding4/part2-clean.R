
library(HMM)

# Data is an observed sequence of X
data = scan("coding4_part2_data.txt")

options(digits=8)
options()$digits

# 2 hidden states
mz = 2
# 3 observable values for x
mx = 3

# Initialize the 3 parameters theta
ini.w = rep(1, mz)
ini.w = ini.w / sum(ini.w)
# Initial transition matrix
ini.A = matrix(1, 2, 2)
ini.A = ini.A / rowSums(ini.A)
# Initial emission matrix
ini.B = matrix(1:6, 2, 3)
ini.B = ini.B / rowSums(ini.B)

ini.para = list(mz = 2, mx = 3, w = ini.w,
                A = ini.A, B = ini.B)

BW.onestep = function(x, para) {
  # Input: 
  # x: T-by-1 observation sequence
  # para: mx, mz, and current para values for
  #    A: initial estimate for mz-by-mz transition matrix
  #    B: initial estimate for mz-by-mx emission matrix
  #    w: initial estimate for mz-by-1 initial distribution over Z_1
  # Output the updated parameters after one iteration
  # We DO NOT update the initial distribution w
  
  T = length(x)
  mz = para$mz
  mx = para$mx
  A = para$A
  B = para$B
  w = para$w
  alp = forward.prob(x, para)
  beta = backward.prob(x, para)
  
  myGamma = array(0, dim=c(mz, mz, T-1))
  #######################################
  ## YOUR CODE: 
  ## Compute gamma_t(i,j) P(Z[t] = i, Z[t+1]=j), 
  ## for t=1:T-1, i=1:mz, j=1:mz, 
  ## which are stored in an array, myGamma
  #######################################
  for(t in 1:(T-1)) {
    for(i in 1:mz) {
      for(j in 1:mz) {
        logs_ = log(c(alp[t, i], A[i, j], B[j, x[t+1]], beta[t+1, j]))
        myGamma[i, j, t] = exp(sum(logs_))
      }
    }
  }
  
  # M-step for parameter A
  #######################################
  ## YOUR CODE: 
  ## A = ....
  #######################################
  newA = matrix(0, mz, mz)
  
  # sum all the mz by mz matrices
  for(t in 1:(T-1)) {
    newA = newA + myGamma[,,t]
  }
  # Convert to probability vectors for each Zi
  newA = newA / rowSums(newA)
  
  # M-step for parameter B
  #######################################
  ## YOUR CODE: 
  ## B = ....
  #######################################
  
  newB = matrix(0, mz, mx)
  #i=1;l=2
  for (i in 1:mz) {
    for (l in 1:mx) {
      Ts = which(x == l)
      if (any(Ts==T)) {
        newB[i,l] = sum(myGamma[,i,T-1]) 
      }
      Ts = Ts[Ts != T]
      newB[i,l] = newB[i,l]+sum(myGamma[i,,Ts]) 
    }
  }
  
  newB = newB / rowSums(newB)  
  
  para$A = newA
  para$B = newB
  return(para)
}

myBW = function(x, para, n.iter = 100) {
  # Input:
  # x: T-by-1 observation sequence
  # para: initial parameter value
  # Output updated para value (A and B; we do not update w)
  
  for(i in 1:n.iter) {
    para = BW.onestep(x, para)
  }
  return(para)
}

options(digits=8)
options()$digits
iters = 100

myout = myBW(data, ini.para, n.iter = iters)

myout$A
myout$B

hmm0 = initHMM(c("A", "B"), c(1, 2, 3),
               startProbs = ini.w,
               transProbs = ini.A, 
               emissionProbs = ini.B)
Rout = baumWelch(hmm0, data, maxIterations=iters, delta=1E-9, pseudoCount=0)
Rout$hmm$transProbs

Rout$hmm$emissionProbs
