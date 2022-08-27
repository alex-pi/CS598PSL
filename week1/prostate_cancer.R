prostate <- read.table("prostate.data")
names(prostate)  

prostate$train
table(prostate$train)  # 67 training vs 30 testing

## Fit a linear regression model to predict lpsa
traindata = prostate[prostate$train==TRUE,]
testdata = prostate[prostate$train==FALSE,]

## Remove the "train" indicator column
traindata = within(traindata, rm(train))
testdata = within(testdata, rm(train))

myfit = lm(lpsa ~ . , data=traindata)
myfit  ## display the estimated LS coefficients
summary(myfit)  ## more output

mypredict = predict(myfit, newdata=testdata)

## mean squared error on the training and test sets. 
sum((traindata$lpsa - myfit$fitted)^2)/nrow(traindata)  
sum((testdata$lpsa - mypredict)^2)/nrow(testdata)    
