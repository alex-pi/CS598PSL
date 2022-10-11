myData = read.csv("https://liangfgithub.github.io/Data/noxData.csv")

library(splines)

head(myData)

# Q8
lmcubic = lm(nox ~ poly(dis, degree = 3), data = myData)
summary(lmcubic)
q8=sum((myData$nox - lmcubic$fitted.values)^2)
round(q8, 2)

# Q9
q9=predict(lmcubic, newdata = data.frame(dis=6))
round(q9, 2)

# Q11
lm4 = lm(nox ~ poly(dis, degree = 4), data = myData)
q11=sum((myData$nox - lm4$fitted.values)^2)
round(q11, 2)

# Q12
q12=predict(lm4, newdata = data.frame(dis=6))
round(q12, 2)

# Q14
myfit1 = lm(nox ~ bs(dis, df=3), data=myData)

op1 = lm(nox ~ bs(dis, knots = median(dis)), data=myData)

op2 = lm(nox ~ bs(dis, df=4, intercept = TRUE), data=myData)

op3 = lm(nox ~ bs(dis, knots = quantile(dis, prob=c(0.25,0.5,0.75)))
         , data=myData)

# Q16
par(mfrow=c(1,1)); 
set.seed(1234)
n = 30 
err = 1
x = runif(n)
y = sin(12*(x+0.2))/(x+0.2) + rnorm(n, 0, err);
plot(x, y, col="red");

fx = 1:70/50;
fy = sin(12*(fx+0.2))/(fx+0.2)
lines(fx, fy, col=8, lwd=2);

plot(x,y, xlab='x', ylab='y');
lines(fx, fy, col=8, lwd=1.5);
lines(predict(smooth.spline(x, y, lambda = 1e+05),fx),  lty=2, col='blue', lwd=1.5);
title('df=5')

plot(x,y, xlab='x', ylab='y');
lines(fx, fy, col=8, lwd=1.5);
lines(predict(smooth.spline(x, y, df = 9),fx),  lty=2, col='blue', lwd=1.5);
title('df=9')