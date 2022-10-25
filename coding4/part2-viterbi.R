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

hmm0 = initHMM(c("A", "B"), c(1, 2, 3),
               startProbs = ini.w,
               transProbs = ini.A, 
               emissionProbs = ini.B)
Rout = baumWelch(hmm0, data, maxIterations=100, delta=1E-9, pseudoCount=0)
Rout$hmm$transProbs

Rout$hmm$emissionProbs

Rout.Z = viterbi(Rout$hmm, data)

## Viterbi

para = list(mz = 2, mx = 3, w = ini.w,
            A = Rout$hmm$transProbs, B = Rout$hmm$emissionProbs)

T = length(x)
mz = para$mz
A = para$A
B = para$B
w = para$w
log.A = log(A)
log.w = log(w)
log.B = log(B)

# Compute delta (in log-scale)
delta = matrix(0, T, mz) 
# fill in the first row of delta
delta[1, ] = log.w + log.B[, x[1]]

#v = array(NA, c(T, mz))
#for (state in 1:mz) {
#  v[1, state] = log(para$w[state] * para$B[state, x[1]])
#}
t=2;state=1;previousState=2
for (t in 2:T) {
  for (state in 1:mz) {
    maxi = NULL
    for (previousState in 1:mz) {
      temp = delta[t - 1, previousState] + log.A[previousState, state]
      maxi = max(maxi, temp)
    }
    delta[t, state] = log.B[state, x[t]] + maxi
  }
}

for (t in 2:T) {
  for (state in 1:mz) {
    temp = delta[t - 1, ] + log.A[, state]
    maxi = max(temp)
    delta[t, state] = log.B[state, x[t]] + maxi
  }
}

# Compute the most prob sequence Z
Z = rep(0, T)
# start with the last entry of Z
Z[T] = which.max(delta[T, ])

for (t in (T - 1):1) {
  for (state in 1:mz) {
    cond = max(delta[t, ] + log.A[, Z[t + 1]] ) == 
      delta[t, state] + log.A[state, Z[t + 1]]
    
    
    if (cond) {
      Z[t] = state
      break
    }
  }
}

t=T - 1;state=1
for (t in (T - 1):1) {
  Z[t] = which.max(delta[t, ] + log.A[, Z[t + 1]] )
}

Z[Z==1] = 'A'
Z[Z==2] = 'B'

all.equal(Rout.Z, Z)
