## Example 1 kNN vs Linear Regression

p = 2;      
s = 1;      # sd for generating the centers              
m1 = c(1,0);
m0 = c(0,1);

# 1) generates a 200x2 matrix with all X values
# 2) then adds 100x2 with m1 repeated and 100x2 with m2 
#     repeated.

# Note that when we add 1,0 or 0,1 (the centers), the mean
# is moving from 0 to 1 for x1 and x2.
# For instance, for the first half we add m1, so x1 shifts
# its mean to 1, while x2 stays on 0.
# They also multiply by s, but it's 1 anyways

set.seed (1303)

n = 100;  # generate the train data; n for each class
traindata = matrix(rnorm(2*n*p), 2*n, p)*s + 
  rbind(matrix(rep(m1, n), nrow = n, byrow=TRUE),
        matrix(rep(m0, n), nrow = n, byrow=TRUE))

dim(traindata)

# Convert 0 and 1 to classes instead of numbers
Ytrain = factor(c(rep(1,n), rep(0,n)))


# Generate 2N test samples similarly.
N = 5000; 
testdata = matrix(rnorm(2*N*p), 2*N, p)*s + 
  rbind(matrix(rep(m1, N), nrow = N, byrow=TRUE), 
        matrix(rep(m0, N), nrow = N, byrow=TRUE))
Ytest = factor(c(rep(1,N), rep(0,N)))

plot(traindata[,1], traindata[,2], type="n", xlab="", ylab="")
# add points from class 1
# first half of the n observations
points(traindata[1:n,1], traindata[1:n,2], col="blue"); 

# add points from class 0
# second half of the n observations
points(traindata[(n+1):(2*n),1], traindata[(n+1):(2*n),2], 
       col="red"); 

# add the 10 centers for class 1
points(m1[1], m1[2], pch = "+", cex = 2, col = "blue"); 

# add the 10 centers for class 0
points(m0[1], m0[2], pch = "+", cex = 2, col = "red");   

legend("bottomright", pch = c(1,1), col = c("red", "blue"), 
       legend = c("class 1", "class 0"))


### Plotting with ggplot

library(ggplot2)

## ggplot only accepts dataframe
mytraindata = data.frame(X1 = traindata[,1], 
                         X2 = traindata[,2], 
                         Y = Ytrain)


## plot the data and color by the Y label
qplot(X1, X2, data = mytraindata, colour = Y)

## change the shape and size of the points
qplot(X1, X2, data = mytraindata, colour = Y, 
      shape = I(1), size = I(3))

## change x,y labels and add titles
qplot(X1, X2, data = mytraindata, colour = Y, 
      shape = I(1), size = I(3),
      main = "Scatter Plot (Training Data)", 
      xlab="", ylab="")

## want to remove the gray background? Change theme
qplot(X1, X2, data = mytraindata, colour = Y, 
      shape = I(1), size = I(3)) + theme_bw()

## more theme options
## A single command would be a little lengthy. We can save 
## the output of qplot into an object "myplot"

myplot = qplot(X1, X2, data = mytraindata, colour = Y,
               shape = I(1), size = I(3))
myplot = myplot + theme_bw();
myplot + theme(legend.position="left")
?theme

## How to adding the two centers on 
## existing plot, we need to use the command "ggplot"
?ggplot
myplot = ggplot(mytraindata, aes(X1, X2))

## use user-specified color
myplot + geom_point(aes(color = Y), shape = 1, size = 3) + scale_color_manual(values = c("red", "blue"))

## add the two centers; 
## change shape and size;
## use user-sepecified color
myplot = ggplot(mytraindata,aes(X1, X2)) +
  geom_point(aes(colour = Y), shape = 1, size = 2) + 
  scale_color_manual(values = c("red", "blue"))   

myplot + 
  geom_point(data = data.frame(X1 = m1[1], X2 = m1[2]), 
             aes(X1, X2), colour = "blue", shape = 3, size = 5) +
  geom_point(data = data.frame(X1 = m0[1], X2 = m0[2]), 
             aes(X1, X2), colour = "red", shape = 3, size = 5)

## K-NN method

library(class) 

## Choice of the neighhood size. 
## Here I just use the values from the textbook
myk = c(151, 101, 69, 45, 31, 21, 11, 7, 5, 3, 1)
m = length(myk);

train.err.knn = rep(0,m);
test.err.knn = rep(0, m);

for( j in 1:m){
  Ytrain.pred = knn(traindata, traindata, Ytrain, k = myk[j])
  train.err.knn[j] = sum(Ytrain != Ytrain.pred)/(2*n)
  Ytest.pred = knn(traindata, testdata, Ytrain,k = myk[j])
  test.err.knn[j] = sum(Ytest != Ytest.pred)/(2*N)
}

## Least Sqaure Method

RegModel = lm(as.numeric(Ytrain) - 1 ~ traindata)
Ytrain_pred_LS = as.numeric(RegModel$fitted > 0.5)

# Use the coefficients to predict on test data
Ytest_pred_LS = RegModel$coef[1] + RegModel$coef[2] * testdata[,1] + 
  RegModel$coef[3] * testdata[,2]
Ytest_pred_LS = as.numeric(Ytest_pred_LS > 0.5)

## cross tab for training data and training error
table(Ytrain, Ytrain_pred_LS);   
train.err.LS = sum(Ytrain !=  Ytrain_pred_LS) / (2*n);  

## cross tab for test data and test error
table(Ytest, Ytest_pred_LS);     
test.err.LS = sum(Ytest !=  Ytest_pred_LS) / (2*N); 

## Bayes Error

Ytest_pred_Bayes = as.numeric(2*testdata %*% matrix(m1-m0, nrow=2) > (sum(m1^2)-sum(m0^2)));

test.err.Bayes = sum(Ytest !=  Ytest_pred_Bayes) / (2*N)

## Plot the Performance

plot(c(0.5,m), range(test.err.LS, train.err.LS, test.err.knn, train.err.knn),
     type="n", xlab="Degree of Freedom", ylab="Error", xaxt="n")

df = round((2*n)/myk)
axis(1, at = 1:m, labels = df)
axis(3, at = 1:m, labels = myk)

# as k gets smaller, the error increases, less bias more variance
# the U shape on test data is not clear, but it decreases a bit to then increase.
points(1:m, test.err.knn, col="red", pch=1);
lines(1:m, test.err.knn, col="red", lty=1);
# for train data, error decreases as k gets smaller, no U shape.
points(1:m, train.err.knn, col="blue", pch=1);
lines(1:m, train.err.knn, col="blue", lty=2);

# df=3 since we use 3 parameters Beta 0,1  and 2
points(3, test.err.LS, pch=2, cex=2, col="red")
points(3, train.err.LS, pch=2, cex=2, col="blue")

# Bayes is our ideal error (non-attainable)
abline(test.err.Bayes, 0, col="purple")
