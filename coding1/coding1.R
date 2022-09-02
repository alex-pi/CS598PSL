
uin_4 = 2110
set.seed(uin_4)
library(class)

# Generate Centers

p = 2;      
csize = 10;     # number of centers
sigma = 1;      # sd for generating the centers 
m1 = matrix(rnorm(csize*p), csize, p)*sigma + 
  cbind( rep(1, csize), rep(0, csize))
m0 = matrix(rnorm(csize*p), csize, p)*sigma + 
  cbind( rep(0, csize), rep(1, csize))

# Generate Data

sim_params = list(
  csize = 10,      # number of centers
  p = 2,           # dimension
  s = sqrt(1/5),   # standard deviation for generating data
  n = 100,         # training size per class
  N = 5000,        # test size per class
  m0 = m0,         # 10 centers for class 0
  m1 = m1         # 10 centers for class 1
)

generate_sim_data = function(sim_params){
  p = sim_params$p
  s = sim_params$s 
  n = sim_params$n 
  N = sim_params$N 
  m1 = sim_params$m1 
  m0 = sim_params$m0
  csize = sim_params$csize
  
  id1 = sample(1:csize, n, replace = TRUE);
  id0 = sample(1:csize, n, replace = TRUE);
  Xtrain = matrix(rnorm(2*n*p), 2*n, p)*s + rbind(m1[id1,], m0[id0,])
  Ytrain = factor(c(rep(1,n), rep(0,n)))
  id1 = sample(1:csize, N, replace=TRUE);
  id0 = sample(1:csize, N, replace=TRUE); 
  Xtest = matrix(rnorm(2*N*p), 2*N, p)*s + rbind(m1[id1,], m0[id0,])
  Ytest = factor(c(rep(1,N), rep(0,N)))
  
  
  # Return the training/test data along with labels
  list(
    Xtrain = Xtrain,
    Ytrain = Ytrain,
    Xtest = Xtest,
    Ytest = Ytest
  )
}

mydata = generate_sim_data(sim_params)

# Visualization

tmp.X = mydata$Xtrain
tmp.Y = mydata$Ytrain
m0 = sim_params$m0        # 10 centers for class 0
m1 = sim_params$m1  

n = nrow(tmp.X)
mycol = rep("blue", n)
mycol[tmp.Y == 0] = "red"
plot(tmp.X[, 1], tmp.X[, 2], type = "n", xlab = "", ylab = "")
points(tmp.X[, 1], tmp.X[, 2], col = mycol);
points(m1[, 1], m1[, 2], pch = "+", cex = 2, col = "blue");    
points(m0[, 1], m0[, 2], pch = "+", cex = 2, col = "red");   
legend("bottomright", pch = c(1,1), col = c("red", "blue"), 
       legend = c("class 0", "class 1"))  


my_knn2 = function(traindata, testdata, Ytrain, k) {
  
  Ypred <- vector(mode = 'integer', length = dim(testdata)[1])
  
  for (i in 1:dim(testdata)[1]) {
    dist = rowSums(sweep(traindata, 2, testdata[i,])^2)
    k_nearest_classes = cbind(dist, Ytrain)[order(dist)[1:k], , drop=F]
    
    k_nearest_classes = data.frame(k_nearest_classes)
    
    r1 <- k_nearest_classes %>% 
      group_by(Ytrain) %>% 
      summarise(count = n()) %>% 
      top_n(1, count)
    Ypred[i] = r1$Ytrain[1] - 1
  }
  
  factor(Ypred)
}

my_knn3 = function(traindata, testdata, Ytrain, k) {
  
  Ypred <- vector(mode = 'integer', length = dim(testdata)[1])
  
  for (i in 1:dim(testdata)[1]) {
    dist = rowSums(sweep(traindata, 2, testdata[i,])^2)
    k_nearest_classes = cbind(dist, Ytrain)[order(dist)[1:k], , drop=F]
    k_nearest_classes = data.frame(k_nearest_classes)
    
    knc_agg=aggregate(k_nearest_classes$Ytrain, 
                by=list(Ytrain=k_nearest_classes$Ytrain), FUN=length)
    knc_agg=knc_agg[knc_agg$x == max(knc_agg$x),]
    Ypred[i] = voted_max - 1
  }
  
  factor(Ypred)
}

my_knn = function(traindata, testdata, Ytrain, k) {
  
  Ypred <- vector(mode = 'integer', length = dim(testdata)[1])
  
  for (i in 1:dim(testdata)[1]) {
    dist = rowSums(sweep(traindata, 2, testdata[i,])^2)
    k_nearest_classes = cbind(dist, Ytrain)[order(dist)[1:k], , drop=F]
    k_nearest_classes = data.frame(k_nearest_classes)
    
    voted_max = as.integer(names(which.max(table(k_nearest_classes$Ytrain)))[1])
    Ypred[i] = voted_max - 1
  }
  
  factor(Ypred)
}

traindata = mydata$Xtrain
testdata = mydata$Xtrain
Ytrain = mydata$Ytrain
k = 1

my_knn(traindata, testdata, Ytrain, k)

traindata = mydata$Xtrain
testdata = mydata$Xtest
Ytrain = mydata$Ytrain
Ytest = mydata$Ytest
N = sim_params$N

## Using my slow knn
test.pred.k1 = my_knn(traindata, testdata, Ytrain, k = 1)

test.pred.k3 = my_knn(traindata, testdata, Ytrain, k = 3)

test.pred.k5 = my_knn(traindata, testdata, Ytrain, k = 5)

test.pred.k4 = my_knn(traindata, testdata, Ytrain, k = 4)

test.err.k1 = sum(Ytest != test.pred.k1)/(2*N)

test.err.k3 = sum(Ytest != test.pred.k3)/(2*N)

test.err.k5 = sum(Ytest != test.pred.k5)/(2*N)

test.err.k4 = sum(Ytest != test.pred.k4)/(2*N)

## Using knn from class
test.pred.ck1 = knn(traindata, testdata, Ytrain, k = 1)

test.pred.ck3 = knn(traindata, testdata, Ytrain, k = 3)

test.pred.ck5 = knn(traindata, testdata, Ytrain, k = 5)

test.err.ck1 = sum(Ytest != test.pred.ck1)/(2*N)

test.err.ck3 = sum(Ytest != test.pred.ck3)/(2*N)

test.err.ck5 = sum(Ytest != test.pred.ck5)/(2*N)



table(test.pred.k1, test.pred.ck1)
table(test.pred.k3, test.pred.ck3)
table(test.pred.k5, test.pred.ck5)

which(test.pred.k1 != test.pred.ck1)
diff_idx = which(test.pred.k3 != test.pred.ck3)
which(test.pred.k5 != test.pred.ck5)

diff_point = testdata[diff_idx, , drop=FALSE]
dist = rowSums(sweep(traindata, 2, diff_point)^2)

## even when k=3, it seems like kNN includes a forth point due to distances to
## third and forth points are very similar. This creates a tie so the predicted
## probabilities are 0.5
k_nearest_classes = cbind(dist, Ytrain)[order(dist)[1:4], , drop=F]
k_nearest_classes = data.frame(k_nearest_classes)
voted_max = as.integer(names(which.max(table(k_nearest_classes$Ytrain)))[1])
Ypred[i] = voted_max - 1

Ytest[diff_idx]
test.pred.k3[diff_idx]
test.pred.ck3[diff_idx]


compute_cv_error <- function(traindata, Ytrain, k = 1, foldNum = 10){
  
  n = nrow(traindata)
  fold_size = floor(n/foldNum)
  error = 0
  random_idx = sample(1 : n)
  for(runId in 1:foldNum){
    # this line produces sets like (when foldNum=10 and n=100): 1:10, 11:20 ... 91:100
    testSetIndex = ((runId-1)*fold_size + 1):(ifelse(runId == foldNum, n, runId*fold_size))
    testSetIndex = random_idx[testSetIndex]
    trainX = traindata[-testSetIndex, ]
    trainY = Ytrain[-testSetIndex]
    testX = traindata[testSetIndex, ]
    testY = Ytrain[testSetIndex]
    predictY = knn(trainX, testX, trainY, k)
    error = error + sum(predictY != testY) 
  }
  error = error / n
  error
}

cvKNN <- function(traindata, Ytrain, k_candidates, foldNum) {
  n = nrow(traindata)
  cvErrorRates = rep(NaN, length(k_candidates))
  
  for(i in 1:length(k_candidates)) {
    cvErrorRates[i] = compute_cv_error(traindata, Ytrain, k_candidates[i])
  }
  
  result = list()
  result$bestK = max(k_candidates[cvErrorRates == min(cvErrorRates)])
  result$cvError = min(cvErrorRates)
  result
}

for(i in 1:10) {
  print(cvKNN(traindata, Ytrain, seq(1, 180), 10))
}

testdata = mydata$Xtest
Ytest = mydata$Ytest
N = sim_params$N

m1 = sim_params$m1
m0 = sim_params$m0
s = sim_params$s
br_preds = rep(NaN, N*2)

for(i in 1:(N*2)) {
  d1 = sum(exp(-apply((t(m1) - testdata[i, ])^2, 2, sum) / (2 * s^2)))
  d0 = sum(exp(-apply((t(m0) - testdata[i, ])^2, 2, sum) / (2 * s^2)))
  
  br = d1 / d0
  br_preds[i] = ifelse(br >= 1, 1, 0)
  
}

print(br_preds)
sum(br_preds == 0)
sum(br_preds == 1)
table(Ytest, br_preds)
