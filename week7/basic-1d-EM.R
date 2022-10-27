library(mclust)
data = faithful
data = data[,1]  # just cluster the 1st dimension

set.seed(123)
K=2; n=length(data)
z=matrix(0,n, K); 
id=sample(1:K, n, replace=TRUE);
for(i in 1:n) z[i,id[i]]=1;
z[1:3, ]
#[,1] [,2]
#[1,]    1    0  # ob 1 assigned to class 1
#[2,]    0    1  # ob 2 assigned to class 2
#[3,]    1    0  # ob 3 assigned to class 1

tmp=mstep(modelName="V", data, z)
summary(tmp)

# gamma_i's = z

# mixing weight (pi, 1-pi)
apply(z, 2, mean)
tmp$parameters$pro

# mu_1, mu_2 
sum(z[,1]*data)/sum(z[,1])
sum(z[,2]*data)/sum(z[,2])
tmp$parameters$mean

# data.center: nx1, data minus group mean
# compute data.center with "lm" command
data.center = lm(data ~ z)$res 
# check how the 1st ob of data.center is computed
data.center[1]
data[1] - mean(data[z[,1]==1])

# sigma_1, sigma_2
sum(z[,1]*data.center^2)/sum(z[,1])
sum(z[,2]*data.center^2)/sum(z[,2])
tmp$parameters$variance$sigmasq

tmp2 = estep(modelName="V", data, tmp$parameters)
tmp2$z # (gamma_i, 1-gamma_i)

# mixing weight (pi, 1-pi)
myp = tmp$parameters$pro  

# means for class 1 and 2
mymean = tmp$parameters$mean 

# variances for class 1 and 2
myvar = tmp$parameters$variance$sigmasq
myvar   

g1 = myp[1]*dnorm(data, mymean[1], sqrt(myvar[1]))
g2 = myp[2]*dnorm(data, mymean[2], sqrt(myvar[2]))

# the two columns should be the same
cbind(g1/(g1+g2), tmp2$z[,1]) 