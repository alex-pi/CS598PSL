library(ggplot2)
library(rpart)
library(rpart.plot)
library(tree) 
load("BostonHousing1.Rdata")  # data: Housing1

# Fit a regression tree using just two predictors
trfit= tree(Y ~ lon + lat, data=Housing1)
small.tree = prune.tree(trfit, best=7)
small.tree

par(mfrow=c(1,2))
plot(small.tree)
text(small.tree, cex=.75)

price.quantiles = cut(Housing1$Y, quantile(Housing1$Y, 0:20/20),
                      include.lowest=TRUE)
plot(Housing1$lat, Housing1$lon, col=grey(20:1/21)[price.quantiles],
     pch=20, ylab="Longitude", xlab="Latitude")
partition.tree(small.tree, ordvars=c("lat","lon"), add=TRUE)

detach("package:tree")

## Fit a regression tree using all predictors
set.seed(1234)
tr1 = rpart(Y ~ ., data = Housing1)
par(mfrow=c(1,2))
plot(tr1)
rpart.plot(tr1)

# This shows the cross-validation done during the tree construction
printcp(tr1)

#CP nsplit rel error  xerror     xstd
#1  0.464749      0   1.00000 1.00266 0.074595
#2  0.157620      1   0.53525 0.55009 0.037433
#3  0.077637      2   0.37763 0.39504 0.034672

# CP, scale version of alpha, or penalty for keeping a split.
# rel error uses the RSS of the root node as reference.
# xerror is similar, but its the cross val error.
# xstd is the sd for the cross val error.

# if CP is between 0.464749 and 0.157620, the optimal tree has
# one split, and so on down the table.

# this gives a tree with 1 split, because 0.3 is between 0.464749,0.157620
prune(tr1, cp=0.3)
prune(tr1, cp=0.2)
# This one is with 2 splits
prune(tr1, cp=0.156)
printcp(prune(tr1, cp=0.156))

par(mfrow=c(1,1))
plotcp(tr1)


# Not sure whether xerror has reached the bottom. Let’s start with a bigger tree.
tr2 = rpart(Y ~ ., data = Housing1, 
            control = list(cp = 0, xval = 10))
plot(tr2)

# The optional subtree would be
# select the minimal xerror.
# then prune using a cp on that range.

# We can also use the 1se principal.
# 1. find minimal xerror
# 2. sum that xerror with its xstd,
# 3. then find a row where max(xerror) < sum from 2. 
printcp(tr2)
plotcp(tr2)


# get index of CP with lowest xerror
opt = which.min(tr2$cptable[, "xerror"])  # 28
# get the optimal CP value
tr2$cptable[opt, 4]

# upper bound for equivalent optimal xerror
tr2$cptable[opt, 4] + tr2$cptable[opt, 5]

# row IDs for CPs whose xerror is equivalent to min(xerror)
tmp.id = which(tr2$cptable[, 4] <= tr2$cptable[opt, 4] +
                 tr2$cptable[opt, 5])
# CP.1se = any value between row (tmp.id) and (tmp.id-1)
CP.1se = 0.0032

# Prune tree with CP.1se
tr3 = prune(tr2, cp = CP.1se)


# Note that CP(1) = relerror(1) - relerror(2)
# CP(2) = relerror(2) - relerror(3)

# relerror(1) - relerror(2), is the improvement on Relative RSS for
# allowing one split,
# but it also represents the maximum price we want to pay for that split

# Understand the relationship between the 1st column and the 3rd column of the CP table.
cbind(tr2$cptable[, 1], c(-diff(tr2$cptable[, 3]), 0))

# Prediction
?predict.rpart  # check use the fitted tree to do prediction


## Handle categorical predictors
set.seed(1234)
n = nrow(Housing1); 
m = 30 
X = as.factor(sample(1:m, n, replace = TRUE))
tmp = data.frame(Y = Housing1$Y, X = X)
myfit = rpart(Y ~ X, data = tmp)
myfit

group.mean = as.vector(tapply(tmp$Y, tmp$X, mean))
order(group.mean)

group.mean[order(group.mean)] #same as sort(group.mean)

tmp$Z= rnorm(n)  # add a numerical feature
myfit = rpart(Y ~ X + Z, data = tmp)
rpart.plot(myfit)

myfit

id1 = which(! (tmp$X %in% c(1, 22, 25, 26, 6, 21)))
length(id1)  # 419

id2 = id1[which(tmp$Z[id1] > 0.95)]
length(id2)  # 54

group.mean = as.vector(tapply(tmp$Y[id2], tmp$X[id2], mean))
order(group.mean)

group.mean

