library(dplyr)

data <- read.csv("Ames_data.csv", stringsAsFactors = FALSE)
testIDs <- read.table("project1_testIDs.dat")

save_splits = function(data, testIDs) {
  
  for(j in 1:dim(testIDs)[2]) {
    
    train <- data[-testIDs[,j], ]
    test <- data[testIDs[,j], ]
    test.y <- test[, c(1, 83)]
    test <- test[, -83]
    write.csv(train, paste("train_", j, ".csv", sep=""),row.names=FALSE)
    write.csv(test, paste("test_", j, ".csv", sep=""),row.names=FALSE)
    write.csv(test.y,paste("test_y_", j, ".csv", sep=""),row.names=FALSE)
  }
}

## Save 3 files for each split, train_j.csv, test_j.csv, test_y_j.csv
save_splits(data, testIDs)

## Test a submission file on a split
test_submission = function(submi_file, benchmk, split) {
  
  test.y = read.csv('test_y.csv', stringsAsFactors = FALSE)
  
  pred = read.csv(submi_file)
  names(test.y)[2] = "True_Sale_Price"
  pred = merge(pred, test.y, by="PID")
  rmse = sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))
  
  print(paste(submi_file, " on split=", split, " - ", (rmse < benchmk), " - rmse=", rmse))  
}


for (j in 1:10) {

  # Prepare files for jth file
  train_name = paste("train_", j, ".csv", sep="")
  test_name = paste("test_", j, ".csv", sep="")
  y_name = paste("test_y_", j, ".csv", sep="")
  
  file.copy(train_name, 'train.csv', overwrite = TRUE)
  file.copy(test_name, 'test.csv', overwrite = TRUE)
  file.copy(y_name, 'test_y.csv', overwrite = TRUE)
  
  benchmk = ifelse(j <= 5, 0.125, 0.135)
  print(paste("###### Test on split ", j, ", benchmark ", benchmk, " #####"))
  
  ptm <- proc.time()
  source("mymain.R")
  #source("ap41_mymain_10am.R")
  sprintf(fmt = "\nTotal execution time: (%.2f seconds)\n", 
          (proc.time() - ptm)[['elapsed']]) %>% cat()
  
  test_submission("mysubmission1.txt", benchmk, j)
  test_submission("mysubmission2.txt", benchmk, j)
}
