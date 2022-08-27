

csize = 10;       # number of centers
p = 2;      
s = 1;      # sd for generating the centers within each class   

## Generate the 20 centers, 10 for each group.
m1 = matrix(rnorm(csize*p), csize, p)*s + cbind( rep(1,csize), rep(0,csize));
m0 = matrix(rnorm(csize*p), csize, p)*s + cbind( rep(0,csize), rep(1,csize));

