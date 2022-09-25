library(ggplot2)
library(rpart)
library(rpart.plot)
library(tree) 
data = load("BostonHousing1.Rdata")  # data: Housing1


# Q2

# Fit a regression tree using just two predictors
par(mfrow=c(1,2))
trfit= rpart(Y ~ age + lstat, data=Housing1)
rpart.plot(trfit)

preds = predict(trfit)
(sum((Housing1$Y - preds)^2))

Housing1['loglstat'] = log(Housing1$lstat)

trfit2= rpart(Y ~ age + loglstat, data=Housing1)
rpart.plot(trfit2)

preds2 = predict(trfit2)
(sum((Housing1$Y - preds2)^2))

sum(preds - preds2)

printcp(trfit)
printcp(trfit2)

# Q3

par(mfrow=c(1,2))
trfit= rpart(Y ~ age + lstat, data=Housing1)
rpart.plot(trfit)

preds = predict(trfit)
(sum((Housing1$Y - preds)^2))

Housing1['logY'] = log(Housing1$Y)

trfit2= rpart(logY ~ age + lstat, data=Housing1)
rpart.plot(trfit2)

preds2 = predict(trfit2)
(sum((Housing1$logY - preds2)^2))

log(preds) - preds2

printcp(trfit)
printcp(trfit2)

# Q5

