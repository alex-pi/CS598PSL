# Q1

b_hat = c(-6, 0.5, 1)
x_test = c(1, 3, 3.5)

log_p = b_hat %*% x_test

round(exp(log_p) / (1 + exp(log_p)), 2)


#

library("ISLR")
library("pROC")
library("glmnet")
library("MASS")

head(Caravan)

test = Caravan[1:1000,]
train = Caravan[-(1:1000),]

fit_glm = glm(Purchase ~ ., data = train, family = binomial)
       
sum(train$Purchase == "Yes")
sum(train$Purchase == "No")

sum(test$Purchase == "Yes")
sum(test$Purchase == "No")

sum(fit_glm$fitted.values >= 0.5)
sum(fit_glm$fitted.values < 0.5)

# 0 Means No 
# 1 Means Yes
sum(fit_glm$fitted.values < 0.5)


preds1 = ifelse(predict(fit_glm, test, type = "response") > 0.25, 
       "Yes", 
       "No")

make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

make_conf_mat(preds1, test$Purchase)

round(auc(test$Purchase, predict(fit_glm, test, type = "response")), 3)

preds2 = ifelse(predict(fit_glm, test, type = "response") > 0.7407, 
                "Yes", 
                "No")

make_conf_mat(preds2, test$Purchase)
###

fit1 = glm(Purchase~., data=train, family=binomial)
fit2 = glm(Purchase~ 1, data=train, family=binomial)
sel.model = stepAIC(fit2, direction = "forward", 
                     scope=list(upper=fit1,lower=fit2), trace=0)
sel.model = step(fit2, direction = "forward", 
                 scope=list(upper=fit1,lower=fit2), trace=0)

preds3 = ifelse(predict(sel.model, test, type = "response") > 0.25, 
                "Yes", 
                "No")

make_conf_mat(preds3, test$Purchase)

round(auc(test$Purchase, predict(sel.model, test, type = "response")), 3)

###

n = dim(train)[1]
bic.model = step(fit2, direction = "forward", 
                    scope=list(upper=fit1,lower=fit2), trace=0,
                 k=log(n))

preds3 = ifelse(predict(bic.model, test, type = "response") > 0.25, 
                "Yes", 
                "No")

make_conf_mat(preds3, test$Purchase)

round(auc(test$Purchase, predict(bic.model, test, type = "response")), 3)

###
lasso.out = glmnet(data.matrix(train[-86]), train$Purchase,
                   standardize = TRUE, intercept = TRUE,
                   alpha = 1, lambda = 0.004, family = "binomial") 

sel.vars = predict(lasso.out, type="nonzero", 
                   s = lasso.out$lambda.min)$s0

preds4 = ifelse(predict(lasso.out, data.matrix(test[-86]), 
                        type = "response") > 0.25, 
                "Yes", 
                "No")

make_conf_mat(preds4, test$Purchase)

round(auc(test$Purchase, c(predict(lasso.out, as.matrix(test[-86]), 
                                 type = "response"))), 3)








