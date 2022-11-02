library(lubridate)
library(tidyverse)
library(prophet)

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
  
  train_split <- unique_pairs %>% 
    left_join(train, by = c('Store', 'Dept')) 
  
  train_split = train_split %>% 
    group_split(Store, Dept)
  
  # do the same for the test set
  test_split <- unique_pairs %>% 
    left_join(test_current, by = c('Store', 'Dept')) 
  test_split = test_split %>% 
    group_split(Store, Dept)
  
  # pre-allocate a list to store the predictions
  test_pred <- vector(mode = "list", length = nrow(unique_pairs))
  
  # perform regression for each split, note we used lm.fit instead of lm
  for (i in 1:nrow(unique_pairs)) {
    print(paste("######## i = ", i, "########"))
    tmp_train <- train_split[[i]][,3:4]
    if(dim(tmp_train)[1] < 2) {
      print(paste("--- skipping ", i))
      next
    }
    names(tmp_train) = c('ds', 'y') 
    tmp_test <- test_split[[i]]
    
    #mycoef <- lm.fit(as.matrix(tmp_train[, -(2:4)]), tmp_train$Weekly_Sales)$coefficients
    #mycoef[is.na(mycoef)] <- 0
    #tmp_pred <- mycoef[1] + as.matrix(tmp_test[, 4:55]) %*% mycoef[-1]
    m <- prophet(tmp_train, 
                 daily.seasonality = TRUE,
                 weekly.seasonality = TRUE, 
                 yearly.seasonality = TRUE,
                 )
    num_weeks = n_distinct(tmp_test$Date)
    
    future <- make_future_dataframe(m, periods=num_weeks, freq = 'week')
    forecast <- predict(m, future)
    
    #test_pred[[i]] <- cbind(tmp_test[, 2:3], Date = tmp_test$Date, Weekly_Pred = tmp_pred[,1])
    test_pred[[i]] <- cbind(tmp_test[, 1:2], Date = tmp_test$Date, 
                            Weekly_Pred = tail(forecast, num_weeks)$yhat)
  }
  
  # turn the list into a table at once, 
  # this is much more efficient then keep concatenating small tables
  test_pred <- bind_rows(test_pred)
  return(test_pred)
}