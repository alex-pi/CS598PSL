library(randomForest)

load("BostonHousing1.Rdata")
mydata = Housing1
n = nrow(mydata)
ntest = round(n * 0.3)
set.seed(1234)
test.id = sample(1:n, ntest)


rfModel = randomForest(Y ~ ., data = mydata[-test.id, ],
                       importance = T, ntree=400); 
names(rfModel)

# This is how many (randomly selected) predictors it takes for 
# each tree
rfModel$mtry

# Avg. Test Error
yhat.test = predict(rfModel, mydata[test.id, ])
sum((mydata$Y[test.id] - yhat.test)^2)/length(test.id)

# Two Training Errors
yhat.train = predict(rfModel, mydata[-test.id, ])

# Avg. training error.
sum((mydata$Y[-test.id] - yhat.train) ^2)/(n - ntest)

# Using predictions from the model itslef
# This is an error from the Cross Validation done by ramforest,
# that is why it is closer to the test error.
sum((mydata$Y[-test.id] - rfModel$predicted) ^2)/(n - ntest)

#We can evaluate the training error by obtaining the prediction 
#on the training set (same as the regular training error, i.e., 
#may underestimate the realerror). But randomForest provides an 
#estimate of the training error based on OOB samples, 
#which is similar to CV errors, i.e., an unbiased estimate 
#of the real classification error.

# For each observations, it says how many times
# it WAS not used for fitting the model (the 400 trees).
# Or how many times it was Out of Bag.
rfModel$oob.times[1:5]

# Same length as training samples
length(rfModel$oob)

## oob.times --> ntree * exp(-1) = ntree * 0.368
# This is the average out of bag times.
rfModel$ntree * exp(-1)

# What is the probability of a sample to not be in the Bag?
# (1- 1/n)^n, and the limit when n->inf is e^-1
mean(rfModel$oob.times)

# The plot function for randomForest
tmp = rfModel$mse
par(mfrow=c(1, 2))
plot(rfModel)
plot(c(0, rfModel$ntree), range(tmp), type="n",
     xlab = "Number of trees", ylab="Error")
lines(tmp)

par(mfrow=c(1, 1))

## Variable importance

#%IncMSE: increase in MSE of predictions if variable j being 
#permuted (for OOB samples) – shuffle the value of variable j, 
#record the change of MSE for each tree, and average over all trees. 
#Can be normalize by standard error with option “scale = TRUE” 
#(default is unscaled).

#IncNodePurity: total decrease of RSS from splitting on variable j, 
#averaged over all trees.

## default %IncMSE is normalized
rfModel$importance

importance(rfModel, scale = F)

cbind(importance(rfModel, scale = TRUE), 
      importance(rfModel, scale = F)[,1]/rfModel$importanceSD)

par(mfrow = c(1,2))
varImpPlot(rfModel, type=1)
varImpPlot(rfModel, type=2)