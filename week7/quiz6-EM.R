library(mclust)
#data = faithful
data = c(5, 15, 25, 30, 40)  # just cluster the 1st dimension

set.seed(123)
K=2; n=length(data)
z=matrix(0,n, K); 
id=sample(1:K, n, replace=TRUE);

for(i in 1:n) z[i,id[i]]=1;
z[1:3, ]


tmp=mstep(modelName="V", data, z)
summary(tmp)

# mixing weight (pi, 1-pi)
apply(z, 2, mean)
tmp$parameters$pro

# mu_1, mu_2 
sum(z[,1]*data)/sum(z[,1])
sum(z[,2]*data)/sum(z[,2])
tmp$parameters$mean

z = matrix(c(0.2,0.8,0.2,0.8,0.8,0.2,0.9,0.1,0.9,0.1), 5,2, byrow = TRUE)

tmp2=mstep(modelName="V", data, z)

apply(z, 2, mean)
tmp2$parameters$pro

# mu_1, mu_2 
sum(z[,1]*data)/sum(z[,1])
sum(z[,2]*data)/sum(z[,2])
tmp2$parameters$mean
