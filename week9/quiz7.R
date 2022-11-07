train = read.table(gzfile("zip.train.gz"))
test = read.table(gzfile("zip.test.gz"))

X_train = train[,-1]
Y_train = train$V1

X_test = test[,-1]
Y_test = test$V1

dig_lda = lda(X_train, Y_train);
Y_pred = predict(dig_lda, X_test)$class
table(Y_test, Y_pred)

#Q5

sum(Y_test == 4)

#Q6
which(Y_pred == 4)
which(Y_test == 4)

length(intersect(which(Y_pred == 4), which(Y_test == 4)))

#Q7

sum(Y_pred == 4)

#Q8

length(setdiff(which(Y_pred == 4), which(Y_test == 4)))

#Q9

grouping(Y_pred)

#Q1011

p = predict(dig_lda, X_test)

# Posterior probabilties for the 4th image.
p$posterior[4,order(p$posterior[4,], decreasing = TRUE)]
