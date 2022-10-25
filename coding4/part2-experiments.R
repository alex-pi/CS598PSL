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

x = data
para = ini.para

T = length(x)
mz = para$mz
mx = para$mx
A = para$A
B = para$B
w = para$w
alp = log(forward.prob(x, para))
beta = log(backward.prob(x, para))

myGamma = array(0, dim=c(mz, mz, T-1))

t=1;i=1;j=1
for(t in 1:(T-1)) {
  for(i in 1:mz) {
    for(j in 1:mz) {
      #logs_ = log(c(alp[t, i], A[i, j], B[j, x[t+1]], beta[t+1, j]))
      
      #myGamma[i, j, t] = exp(sum(logs_))
      myGamma[i, j, t] = alp[t, i] + log(A[i, j]) + log(B[j, x[t + 1]]) + beta[t + 1, j]
    }
  }
}

probObservations = alp[T, 1]
for (i in 2:mz) {
    probObservations = alp[T, i] + log(1 + exp(probObservations - 
                                                 alp[T, i]))
}

newA = matrix(0, mz, mz)
for (i in 1:mz) {
  for (j in 1:mz) {
    temp = alp[1, i] + log(A[i, j]) + log(B[j, x[2]]) + beta[2, j]
    for (t in 2:(T - 1)) {
      temp2 = alp[t, i] + log(A[i, j]) + log(B[j, x[t + 1]]) + beta[t + 1, j]
      if (temp2 > -Inf) {
        temp = temp2 + log(1 + exp(temp - temp2))
      }
    }
    newA[i, j] = exp(temp - probObservations)
  }
}

for (i in 1:mz) {
  for (j in 1:mz) {
    temp = myGamma[i, j, 1]
    #temp = alp[1, i] + log(A[i, j]) + log(B[j, x[2]]) + beta[2, j]
    for (t in 2:(T - 1)) {
      temp2 = myGamma[i, j, t]
      #temp2 = alp[t, i] + log(A[i, j]) + log(B[j, x[t + 1]]) + beta[t + 1, j]
      if (temp2 > -Inf) {
        temp = temp2 + log(1 + exp(temp - temp2))
      }
    }
    newA[i, j] = exp(temp - probObservations)
  }
}



newA = (newA/apply(newA, 1, sum))

newB = matrix(0, mz, mx)
for (i in 1:mz) {
  for (s in 1:mx) {
    temp = -Inf
    for (t in 1:T) {
      if (s == x[t]) {
        j = alp[t, i] + beta[t, i]
        if (j > -Inf) {
          temp = j + log(1 + exp(temp - j))
        }
      }
    }
    newB[i, s] = exp(temp - probObservations)
  }
}

newB = matrix(0, mz, mx)
for (i in 1:mz) {
  for (s in 1:mx) {
    #temp = -Inf
    temp = sum(myGamma[i, , 1])
    for (t in 1:T) {
      if (s == x[t]) {
        t_ = ifelse(t == T, t-1, t)
        #j = alp[t, i] + beta[t, i]
        j = sum(myGamma[i, , t_])
        if (j > -Inf) {
          temp = j + log(1 + exp(temp - j))
        }
      }
    }
    newB[i, s] = exp(temp - probObservations)
  }
}

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

newB = (newB/apply(newB, 1, sum))

