p1Data = read.csv("Coding3_Data.csv", header=TRUE)
dim(p1Data)
View(p1Data)

x = p1Data$x
n = length(x);
lev = rep(0, n)

sp = 0.20
y = rep(0, n); y[1]=1
yi = loess(y ~ x, data = data.frame(cbind(x, y)), span = span, 
           control = loess.control(surface = "direct"))$fitted

lo.lev <- function(x1, sp){
  # x1: n-by-1 feature vector
  # sp: a numerical value for "span"
  
  n = length(x1);
  lev = rep(0, n)
  
  ##############################################
  # YOUR CODE: Compute the diagonal entries of the 
  #            smoother matrix S and 
  #            store it in a vector "lev"
  # Tip: check how we compute the smoother matrix
  #      for smoothing spline models
  ##############################################
  
  for(i in 1:n){
    y = rep(0, n); y[i]=1;
    yi = loess(y ~ x1, data = data.frame(cbind(x1, y)), span = sp, 
               control = loess.control(surface = "direct"))$fitted
    # Add element of diagonal of S matrix
    lev[i]= yi[i];
  } 
  
  return(lev)
}

lo.lev(x, span)

x = p1Data$x
y = p1Data$y
lom = loess(y ~ x, data = data.frame(cbind(x, y)), span = sp, 
            control = loess.control(surface = "direct"))

onestep_CV <- function(x1, y1, sp){
  
  ##############################################
  #  YOUR CODE: 
  #  1) Fit a loess model y1 ~ x1 with span = sp, and extract 
  #     the corresponding residual vector
  #  2) Call lo.lev to obtain the diagonal entries of S
  #  3) Compute LOO-CV and GCV
  ##############################################
  
  lom = loess(y1 ~ x1, data = data.frame(cbind(x1, y1)), span = sp, 
              control = loess.control(surface = "direct"))
  y_res = lom$residuals
  S_diag = lo.lev(x1, sp)
  
  loocv = mean((y_res / (1 - S_diag))^2)
  mean_tr = sum(S_diag) / length(x1)
  gcv = mean((y_res / (1 - mean_tr))^2)
  
  
  return(list(cv = loocv, gcv = gcv))
}

onestep_CV(x, y, sp)


myCV <- function(x1, y1, span){
  # x1: feature vector of length n
  # y1: response vector of length n
  # span: a sequence of values for "span"
  
  m = length(span)
  cv = rep(0, m)
  gcv = rep(0, m)
  
  for(i in 1:m){
    tmp = onestep_CV(x1, y1, span[i])
    cv[i] = tmp$cv
    gcv[i] = tmp$gcv
  }
  return(list(cv = cv, gcv = gcv))
}

span1 = seq(0.2, 0.90, by = 0.05)

cv.out = myCV(p1Data$x, p1Data$y, span1)

myout = data.frame(CV = cv.out$cv, 
                   GCV = cv.out$gcv, 
                   span = span1)
myout$span[myout$GCV == min(myout$GCV)]
myout$span[myout$CV == min(myout$CV)]
myout

spangcv.min = 0.5
plot(p1Data$x, p1Data$y, xlab="", ylab="", col="red");
fx = 1:50/50;
fy = sin(12*(fx+0.2))/(fx+0.2)
lines(fx, fy, col=8, lwd=2);
f = loess(y ~ x, p1Data, span = spangcv.min)
lines(fx, predict(f, data.frame(x = fx), surface = "direct"), 
      lty=2, lwd=2, col="blue")