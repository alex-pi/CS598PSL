
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

forward.prob = function(x, para){
  # Output the forward probability matrix alp 
  # alp: T by mz, (t, i) entry = P(x_{1:t}, Z_t = i)
  T = length(x)
  mz = para$mz
  A = para$A
  B = para$B
  w = para$w
  alp = matrix(0, T, mz)
  
  # fill in the first row of alp
  alp[1, ] = w * B[, x[1]]
  # Recursively compute the remaining rows of alp
  for(t in 2:T){
    tmp = alp[t-1, ] %*% A
    alp[t, ] = tmp * B[, x[t]]
  }
  return(alp)
}

backward.prob = function(x, para){
  # Output the backward probability matrix beta
  # beta: T by mz, (t, i) entry = P(x_{1:t}, Z_t = i)
  T = length(x)
  mz = para$mz
  A = para$A
  B = para$B
  w = para$w
  beta = matrix(1, T, mz)
  
  # The last row of beta is all 1.
  # Recursively compute the previous rows of beta
  for(t in (T-1):1){
    tmp = as.matrix(beta[t+1, ] * B[, x[t+1]])  # make tmp a column vector
    beta[t, ] = t(A %*% tmp)
  }
  return(beta)
}

x = data
para = ini.para

T = length(x)
mz = para$mz
mx = para$mx
A = para$A
B = para$B
w = para$w
alp = forward.prob(x, para)
beta = backward.prob(x, para)

myGamma = array(0, dim=c(mz, mz, T-1))

t=1;i=1;j=1
for(t in 1:(T-1)) {
  for(i in 1:mz) {
    for(j in 1:mz) {
      logs_ = log(c(alp[t, i], A[i, j], B[j, x[t+1]], beta[t+1, j]))
      #myGamma[i, j, t] = exp(sum(logs_))
      myGamma[i, j, t] = sum(logs_)
    }
  }
}

newA = matrix(0, mz, mz)

# sum all the mz by mz matrices
for(t in 1:(T-1)) {
  newA = newA + myGamma[,,t]
}

for (i in 1:mz) {
  for (j in 1:mz) {
    temp = myGamma[i, j, 1]
    for (t in 1:(T - 1)) {
      temp2 = myGamma[i, j, t]
      newA[i, j] = newA[i, j] + temp2
    }
  }
} 

# Convert to probability vectors for each Zi
newA = newA / rowSums(newA)

newB = matrix(0, mz, mx)

for (i in 1:mz) {
  for (l in 1:mx) {
    Ts = which(x == l)
    Ts[Ts==T]=T-1
    newB[i,l] = sum(myGamma[i,,Ts]) 
  }
}

i=1;l=2
for (i in 1:mz) {
  for (l in 1:mx) {
    Ts = which(x == l)
    for (t in Ts) {
      #t_idxs[t_idxs==T]=T-1
      t_ = ifelse(t == T, t-1, t)
      newB[i,l] = newB[i,l] + sum(myGamma[i, ,t_])
    }
  }
}

newB = newB / rowSums(newB)

hmm0 = initHMM(c("A", "B"), c(1, 2, 3),
              startProbs = ini.w,
              transProbs = ini.A, 
              emissionProbs = ini.B)
Rout = baumWelch(hmm0, data, maxIterations=1, delta=1E-9, pseudoCount=0)
Rout$hmm$transProbs

Rout$hmm$emissionProbs

f = forward(hmm0, x)
b = backward(hmm0, x)
probObservations = f[1, length(x)]
for (i in 2:length(hmm0$States)) {
  j = f[i, length(x)]
  if (j > -Inf) {
    probObservations = j + log(1 + exp(probObservations - 
                                         j))
  }
}
alp
