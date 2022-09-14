auto <- read.table("Q2Auto.data", header = TRUE)
names(auto)  

View(auto)

q1lm = lm(mpg ~ displacement, data = auto)
q1lm
round(q1lm$coefficients[2], 2)

suma = summary(q1lm)
names(summary(q1lm))
summary(q1lm)$r.squared

# Sum of Squares total
(SST = sum((auto$mpg - mean(auto$mpg))^2))
# Sum of Squares Regression
(SSR = sum((q1lm$fitted.values - mean(auto$mpg))^2))
# Sum of Squares Error SSE or RSS (Residual Sum of Squares)
(SSE = sum((auto$mpg - q1lm$fitted.values)^2))
# se_2 is estimate of the variance for y
n = dim(auto)[1]
p = 2 # 2 parameters B0 and B1 
# degrees of freedom is n - p
(s2_e = SSE / (n - 2))
# The RSE Residual Standard Error is
(s_e = sqrt(s2_e))
suma$sigma


(r_2 = 1 - (SSE / SST))
r_2 == summary(q1lm)$r.squared
round(summary(q1lm)$r.squared, 2)

q3lm = lm(mpg ~ .-name, data = auto)
round(q3lm$coefficients['displacement'], 2)


## Q4
b0 = 50
b1 = 20
b2 = 0.07
b3 = 35
b4 = 0.01
b5 = -10

x1_gpa = 4
x2_iq = 100

# full model (x3=1 male)
# Y = b0 + b1*x1 + b2*x2 + b3*x3 + b4*x1*x2 + b5*x1*x3
# x3=0 male
# Y = b0 + b1*x1 + b2*x2 + b4*x1*x2

# x3=1 female
# Y = b0 + b1*x1 + b2*x2 + b3 + b4*x1*x2 + b5*x1
# Y = (b0 + b3) + (b1 + b5 + b4*x2)x1

# Y changes positively for 1 unit of increase on x1 (gpa)
# also, the slope for x1 depends on x2
# Y = b0 + (b1 + b4x2)x1 + b2*x2
# Same idea as above.
# Y = b0 + (b2 + b4x1)x2 + b1*x1



x3_gen = 1 # female
(y_pred_m = b0 + b1*x1_gpa + b2*x2_iq + b3*x3_gen + b4*x1_gpa*x2_iq + b5*x1_gpa*x3_gen)

x3_gen = 0 # male
(y_pred_m = b0 + b1*x1_gpa + b2*x2_iq + b3*x3_gen + b4*x1_gpa*x2_iq + b5*x1_gpa*x3_gen)
(y_pred_f = b0 + b1*x1_gpa + b2*x2_iq + b4*x1_gpa*x2_iq)

# when gpa changes
x1_gpa = 3.4
x3_gen = 1 # female
(y_pred_m = b0 + b1*x1_gpa + b2*x2_iq + b3*x3_gen + b4*x1_gpa*x2_iq + b5*x1_gpa*x3_gen)

x3_gen = 0 # male
(y_pred_m = b0 + b1*x1_gpa + b2*x2_iq + b3*x3_gen + b4*x1_gpa*x2_iq + b5*x1_gpa*x3_gen)
(y_pred_f = b0 + b1*x1_gpa + b2*x2_iq + b4*x1_gpa*x2_iq)


#Q6 coefficients change when we scale the predictors?
auto$disp2 = auto$displacement/10
q6lm = lm(mpg ~ displacement * horsepower * origin, data = auto)
q6lm_2 = lm(mpg ~ disp2 * horsepower * origin, data = auto)

q6lm$coefficients
q6lm_2$coefficients
q6lm$coefficients[2] / 10

# Q7

x1_gpa = 4
x2_iq = 100

# Y = b0 + b1*x1 + b2*x2 + b3*x3 + b4*x1*x2 + b5*x1*x3

# x3=0 male
# Y = b0 + b1*x1 + b2*x2 + b4*x1*x2 
Y = b0 + b1*x1_gpa + b2*x2_iq + b4*x1_gpa*x2_iq

# x3=1 female
# Y = b0 + b1*x1 + b2*x2 + b3 + b4*x1*x2 + b5*x1
Y = (b0 + b3) + (b1 + b5 + b4*x2_iq)x1_gpa + b2*x2_iq

Y = (b0 + b3) + (b1 + b5)x1_gpa + b2*x2_iq + b4*(x1_gpa*x2_iq)

# Q15

# Y = b0 + b1*x1 + b2*x2 + b3*x3 + b4*x1*x2 + b5*x1*x3
x3 = 1 #female before
nx3 = 1 - x3 #female now
x3 = 1 - nx3

# So we rewrite the eq
# Y = a0 + a1*x1 + a2*x2 + a3*(1 - nx3) + a4*x1*x2 + a5*x1*(1 - nx3)
# Y = a0 + a1*x1 + a2*x2 + a3 - a3*nx3 + a4*x1*x2 + a5*x1 - a5*x1*nx3
# Y = (a0 + a3) + (a1+a5)*x1 + a2*x2 - a3*nx3 + a4*x1*x2 - a5*x1*nx3

x3_gen = 1 # female before
(y_pred_f = b0 + b1*x1_gpa + b2*x2_iq + b3*x3_gen + b4*x1_gpa*x2_iq + b5*x1_gpa*x3_gen)

nx3 = 1 - x3_gen# female now
(y_pred_f = (b0 + b3) + (b1+b5)*x1_gpa + b2*x2_iq - b3*nx3 + b4*x1_gpa*x2_iq - b5*x1_gpa*nx3)

a0 = b0 + b3
a1 = b1+b5
a2 = b2
a3 = -b3
a4 = b4
a5 = -b5

(y_pred_f = a0 + a1*x1_gpa + a2*x2_iq + a3*nx3 + a4*x1_gpa*x2_iq + a5*x1_gpa*nx3)


# Q21

x1 = rnorm(dim(auto)[1])
x2 = rnorm(dim(auto)[1])
x3 = rnorm(dim(auto)[1])

fmod = lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration, data = auto)


summary(fmod)$r.squared
sum((auto$mpg - fmod$fitted.values)^2) #RSS

fadd = lm(mpg ~ ., data = auto)

summary(fadd)$r.squared
sum((auto$mpg - fadd$fitted.values)^2) #RSS

auto$x1 = x1
auto$x2 = x2
auto$x3 = x3

# Q22

fadd = lm(mpg ~ displacement + horsepower + weight + acceleration, data = auto)

fadd$coefficients
summary(fadd)$r.squared
sum((auto$mpg - fadd$fitted.values)^2) #RSS
summary(fadd)$sigma

library("dplyr")  

auto2 <- auto %>% slice(rep(1:n(), each = 2))

fadd = lm(mpg ~ displacement + horsepower + weight + acceleration, data = auto2)

fadd$coefficients
summary(fadd)$r.squared
sum((auto$mpg - fadd$fitted.values)^2) #RSS
summary(fadd)$sigma

# Q23

auto$y = 3 
q23 = lm(y ~ displacement + horsepower + weight + acceleration, data = auto) # if y is constant there is no rank deficiency
qr(auto[,3:6])$rank # 4 predictors are linearly independent

auto_s = auto[,3:6]
auto_s$x = 1.2 
auto_s$intercept = 1
qr(auto_s)$rank # rank is < p + 1, x and intercept are dependent
auto_s$y = auto$mpg
# if we include a constant predictor there is rank deficiency
q23 = lm(y ~ ., data = auto_s) 

auto <- read.table("Q2Auto.data", header = TRUE)

qr(auto[1:2, 3:6])$rank # n=2 and p=4, n<p, rank is 2 in this case.
# when p > n there is rank deficiency
lm(mpg ~ displacement + horsepower + weight + acceleration, data = auto[1:2, ])

auto_s = auto[,3:6]
auto_s$x = auto$displacement * 2
dim(auto_s)
qr(auto_s)$rank #rank should be 5, but x depends on displacement, so it is 4
auto_s$y = auto$mpg
lm(y ~ displacement + horsepower + weight + acceleration + x , data = auto_s)


# Q24

set.seed(42)
sample_size <- 100000
x <- rnorm(sample_size, mean = 0)
y <- rnorm(sample_size, mean = 0)
lm(y ~ x)
plot(x, y)
abline(lm(y ~ x), lwd = 3, col = "darkorange")

beta_0 <- 0
beta_1 <- 6
sigma <- 1

# These are the variances used for beta1 and beta2 distributions
#var_beta_1_hat <- sigma ^ 2 / Sxx
#var_beta_0_hat <- sigma ^ 2 * (1/sample_size + mean(x) ^ 2 / Sxx)

num_samples <- 1
beta_0_hats <- rep(0, num_samples)
beta_1_hats <- rep(0, num_samples)

for(i in 1:num_samples) {
  eps <- rnorm(sample_size, mean = 0, sd = sigma)
  y <- beta_0 + beta_1 * x + eps
  
  sim_model <- lm(y ~ x)
  
  beta_0_hats[i] <- sim_model$coefficients[1]
  beta_1_hats[i] <- sim_model$coefficients[2]
}

# Q25

sim_model$coefficients
summary(sim_model)$r.squared
sum((y - sim_model$fitted.values)^2) #RSS
summary(sim_model)$sigma

y = c(y, sim_model$fitted.values[100])
x = c(x, x[100])
sim_model <- lm(y ~ x)

# Q26

mod1 = lm(mpg ~ displacement, data = auto)
summary(mod1)$r.squared
mod2 = lm(mpg ~ horsepower, data = auto)
summary(mod2)$r.squared
mod3 = lm(mpg ~ displacement + horsepower, data = auto)
summary(mod3)$r.squared


data2 = read.table("quiz2_toydata2.dat", sep=",")
summary(lm(y ~ x1, data=data2))
summary(lm(y ~ x2, data=data2))
summary(lm(y ~ x1 + x2, data=data2))


data1 = read.table("quiz2_toydata1.dat", sep=",")
summary(lm(y ~ x1, data=data1))
summary(lm(y ~ x2, data=data1))
myfit = lm(y ~ x1 + x2, data=data1)
summary(myfit)
round(sum(myfit$res^2), dig = 5)