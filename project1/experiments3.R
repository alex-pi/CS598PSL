dataj = load_split(3)

test = dataj$test[, -1]
train = dataj$train[, -1]

## Drop assumed irrelevant
train = drop_irrelevant(train, c())
test = drop_irrelevant(test, c())

## Transform factors
test = convert_factors(test)
train = convert_factors(train)


X = model.matrix(Sale_Price ~ ., train)[, -1]
Y = train$Sale_Price

test$Sale_Price = dataj$y[, 2]
X_test = model.matrix(Sale_Price ~ ., test)

fit_lasso_cv = cv.glmnet(X, Y, alpha = 1)

best.lam = fit_lasso_cv$lambda.min
Ytest.pred = predict(fit_lasso_cv, s = best.lam, newx = X_test)
sqrt(mean((log(Y_test) - Ytest.pred)^2))