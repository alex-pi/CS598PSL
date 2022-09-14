library(MASS)
myData = Boston
names(myData)[14] = "Y"
iLog = c( 1,3,5,6,8,9,10,14 );
myData[,iLog] = log( myData[,iLog] );
myData[,2] = myData[,2] / 10;
myData[,7] = myData[,7]^2.5 / 10^4
myData[,11] = exp( 0.4 * myData[,11] ) / 1000;
myData[,12] = myData[,12] / 100;
myData[,13] = sqrt( myData[,13] );

full.model = lm( Y ~ ., data = myData);  
n = dim(myData)[1];
summary(full.model)

p = 13

library(leaps)
b = regsubsets(Y ~ ., data=myData, nvmax = p)
rs = summary(b)
# Best model after adding 1,2,3...p predictors.
rs$which

msize = 1:p;
par(mfrow=c(1,2))
# to fairly compare models with different predictors
# we use aic and bic
Aic = n*log(rs$rss/n) + 2*msize;
Bic = n*log(rs$rss/n) + msize*log(n);

# from the graph seems like both select p=9
# it might not be the same set of 9 parameters...
plot(msize, Aic, xlab="No. of Parameters", ylab = "AIC");
plot(msize, Bic, xlab="No. of Parameters", ylab = "BIC");

cbind(rs$which[which.min(Aic),], rs$which[which.min(Bic), ])

# leaps does not return AIC, but BIC. Its BIC differs from what has 
# been computed above, but the difference is a constant, 
# so the two BIC formulae (ours and the one used by leaps) 
# are essentially the same.

cbind(rs$bic, Bic, rs$bic - Bic)

# What are the 2nd and 3rd best models in terms of AIC/BIC?

# for each p return the top 3 models
b = regsubsets(Y ~ ., data=myData, nbest = 3, nvmax = p)
rs = summary(b)
rs$which

msize = apply(rs$which, 1, sum) - 1
par(mfrow=c(1,2))
Aic = n*log(rs$rss/n) + 2*msize;
Bic = n*log(rs$rss/n) + msize*log(n);
plot(msize, Aic, xlab="No. of Parameters", ylab = "AIC");
plot(msize, Bic, xlab="No. of Parameters", ylab = "BIC");

# Although the top model returned by AIC and the one by BIC are the same; 
# the 2nd and the 3rd best models returned by the two criteria are different:
# AIC favors larger models while BIC favors smaller models

plot(msize[msize > 3], Aic[msize > 3], xlab="No. of Parameters", ylab = "AIC");
plot(msize[msize > 3], Bic[msize > 3], xlab="No. of Parameters", ylab = "BIC");

# top three models by AIC
rs$which[order(Aic)[1:3],]

# top three models by BIC
rs$which[order(Bic)[1:3],]


# AIC and BIC
stepAIC = step(full.model, direction="both")

n = dim(myData)[1]
stepBIC = step(full.model, direction="both", k=log(n))  


# AIC and BIC with setwise selection also choose the same model with p=9
sel.var.AIC = attr(stepAIC$terms, "term.labels")
sel.var.BIC = attr(stepBIC$terms, "term.labels")

length(sel.var.AIC)
length(sel.var.BIC)

sel.var.BIC %in% sel.var.AIC




