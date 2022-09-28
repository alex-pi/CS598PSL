

load_split = function(j) {
  train_name = paste("train_", j, ".csv", sep="")
  test_name = paste("test_", j, ".csv", sep="")
  y_name = paste("test_y_", j, ".csv", sep="")
  
  data = list(
    train = read.csv(train_name, stringsAsFactors = FALSE),
    test = read.csv(test_name, stringsAsFactors = FALSE),
    y = read.csv(y_name, stringsAsFactors = FALSE)
  )
  
  return(data)
}

benchmk1 = 0.125
for (j in 1:10) {
  print(paste("###### Test on split ", j, "#####"))
  # Prepare files for jth file
  jth = load_split(i)
  write.csv(train, "train.csv", row.names=FALSE)
  write.csv(test, "test.csv", row.names=FALSE)
  
  source("mymain.R")
  
  rmse = test_split(i, lasso_eval)
  print(rmse)
  print(rmse < benchmk)
}

uin_4 = 15052
set.seed(uin_4)

benchmk = 0.135
for (i in 1:5) {
  print(paste("###### Test on split ", (i+5), "#####"))
  rmse = test_split(i+5, lasso_eval)
  print(rmse)
  print(rmse < benchmk)
}