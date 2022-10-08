library(splines)

mydata = read.csv("ST_Weekly.csv", header=TRUE)

ts = as.matrix(mydata[, 2:53])
row.names(ts) = mydata[,1]
ts1 = t(t(ts) - rowMeans(ts))
ts2 = scale(ts, center = TRUE, scale = FALSE)

dim(ts)

fx = 1:52

F = ns(fx, df=9, intercept=FALSE)
# We subtract the mean because we want
# all columns orthogonal to the intercept.
# Also, center data like this or using scale function
# 
F = scale(F, center = TRUE, scale = FALSE)
dim(F)
Ft = t(F)

Bt = solve(Ft %*% F) %*% Ft %*% t(ts)
B = t(Bt)
dim(B)

Bkm = kmeans(B, centers=6, nstart=10)

show_graphs = function(km, ts_data, fx, centers_fn) {
  par(mfrow = c(2, 3))
  for(ci in 1:6) {
    ts_idxs = which(km$cluster == ci)
    ts_cluster_i = ts_data[ts_idxs, ]
    
    xl = c(1, 52)
    yl = c(min(ts_data), max(ts_data))
    
    plot(NULL, NULL, 
         xlab = "Weeks",
         ylab = "Sales",
         xlim = xl, 
         ylim = yl, col = "gray")
    
    for(j in 1:dim(ts_cluster_i)[1]) {
      lines(x = fx, y = ts_cluster_i[j,], col = "gray")
    }
    
    ci_center = centers_fn(km$centers[ci, ])
    lines(x = fx, y = ci_center, col="red")
  }
}

show_graphs(Bkm, ts, fx, function(centers) F %*% centers)

TSkm = kmeans(ts, centers=6, nstart=10)

show_graphs(TSkm, ts, fx, function(centers) centers)
