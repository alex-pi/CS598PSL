
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


my_knn = function(traindata, testdata, Ytrain, k) {
  
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

traindata = mydata$Xtrain
testdata = mydata$Xtrain
Ytrain = mydata$Ytrain
k = 1

my_knn(traindata, testdata, Ytrain, k)

Ypred <- vector(mode = 'integer', length = dim(testdata)[1])

for (i in 1:dim(testdata)[1]) {
  dist = rowSums(sweep(traindata, 2, testdata[i,])^2)
  k_nearest_classes = cbind(dist, Ytrain)[order(dist)[1:k], , drop=F]

  k_nearest_classes = data.frame(k_nearest_classes)

  r1 <- k_nearest_classes %>% 
    group_by(Ytrain) %>% 
    summarise(count = n()) %>% 
    top_n(1, count)
  #print(r1$Ytrain[1])
  #voted_idx = max(table(k_nearest_classes$Ytrain))
  #print(k_nearest_classes[voted_idx,])
  Ypred[i] = r1$Ytrain[1] - 1
}
