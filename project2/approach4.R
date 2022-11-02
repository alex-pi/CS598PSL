library(lubridate)
library(tidyverse)

mypredict = function(){
  print(paste("######## t = ", t, "########"))
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1)) 
  test_current <- test %>%
    filter(Date >= start_date & Date < end_date)
  
  # find the unique pairs of (Store, Dept) combo that appeared in both training and test sets
  train_pairs <- train[, 1:2] %>% 
    count(Store, Dept) %>% 
    filter(n != 0)
  test_pairs <- test_current[, 1:2] %>% 
    count(Store, Dept) %>% 
    filter(n != 0)
  
  # These are the number of Store/Dept splits.
  unique_pairs <- intersect(train_pairs[, 1:2], test_pairs[, 1:2])
  
  # pick out the needed training samples, 
  # convert to dummy coding, 
  # then put them into a list
  train_split <- unique_pairs %>% 
    left_join(train, by = c('Store', 'Dept')) %>% 
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>% 
    mutate(Yr = year(Date))
  
  train_split[train_split$Weekly_Sales < 0,]$Weekly_Sales = 0.1
  
  # This splits one tibble into many. One for each Store/Dept combination.
  train_split = as_tibble(model.matrix(~ Weekly_Sales + Store + Dept + Yr + Wk, train_split)) %>% 
    group_split(Store, Dept)
  
  # do the same for the test set
  test_split <- unique_pairs %>% 
    left_join(test_current, by = c('Store', 'Dept')) %>% 
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>% 
    mutate(Yr = year(Date))
  test_split = as_tibble(model.matrix(~ Store + Dept + Yr + Wk, test_split)) %>% mutate(Date = test_split$Date) %>% group_split(Store, Dept)
  
  # pre-allocate a list to store the predictions
  test_pred <- vector(mode = "list", length = nrow(unique_pairs))
  
  # perform regression for each split, note we used lm.fit instead of lm
  for (i in 1:nrow(unique_pairs)) {
    print(paste("######## i = ", i, "########"))
    tmp_train <- train_split[[i]]
    if(dim(tmp_train)[1] < 3) {
      print(paste("--- skipping ", i))
      next
    }
    tmp_test <- test_split[[i]]
    
    X = as.matrix(tmp_train[, 3:56])
    Y = as.matrix(tmp_train$Weekly_Sales)
    ridge.out = glmnet(X, Y, alpha = 0)
    
    X_test = as.matrix(tmp_test[, 2:55])
    Ytest.pred = predict(ridge.out, s = ridge.out$lambda.min, 
                         newx = X_test)
    
    test_pred[[i]] <- cbind(tmp_test[, 2:3], Date = tmp_test$Date, Weekly_Pred = Ytest.pred[,100])
  }
  
  # turn the list into a table at once, 
  # this is much more efficient then keep concatenating small tables
  test_pred <- bind_rows(test_pred)
}


