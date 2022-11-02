library(lubridate)
library(tidyverse)

mypredict = function() {
  print(paste("$$$$$$$ t = ", t, "$$$$$$$"))
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
    group_split(Dept)
  
  test_split <- unique_pairs %>% 
    left_join(test_current, by = c('Store', 'Dept')) %>% 
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>% 
    mutate(Yr = year(Date))
  
  test_split = as_tibble(model.matrix(~ Store + Dept + Yr + Wk, test_split)) %>% 
    mutate(Date = test_split$Date) %>% 
    group_split(Dept)
  
  # pre-allocate a list to store the predictions
  test_pred <- vector(mode = "list", length = nrow(unique_pairs))

  for (i in 1:length(train_split)) {
    print(paste("######## i = ", i, "########"))
    dept_train = train_split[[i]][,1:4]
    
    num_comp = 8
    train_svd_ = dept_train %>% 
      select(Store, Dept, Date, Weekly_Sales) %>%
      spread(Date, Weekly_Sales)
    
    ## Do SVD
    if(nrow(train_svd_) > num_comp) {
      
      svd_matrix = as.matrix(train_svd_[, 3:dim(train_svd_)[2]])
      svd_matrix[is.na(svd_matrix)] = 0 
      
      store_means = rowMeans(svd_matrix)
      svd_matrix = svd_matrix - store_means
      
      z = svd(svd_matrix, nu=num_comp, nv=num_comp) # get u,d,v
      d = diag(z$d[1:num_comp]) # diagonal matrix
      svd_matrix = z$u %*% d %*% t(z$v) # recreate the matrix
      svd_matrix = svd_matrix + store_means
      #dim(svd_matrix)
      
      ## Reinsert lower rank matrix into train tibble
      train_svd_[, 3:dim(train_svd_)[2]] = svd_matrix
      
      dept_train = train_svd_ %>%
        gather(Date, Weekly_Sales, -Store, -Dept)
    }
    ## End Do SVD
    
    ## TODO try to see how to get following 2 operations out of the loop.
    dept_train = dept_train %>% 
      mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>% 
      mutate(Yr = year(Date))
    train_svd_mm = as_tibble(model.matrix(~ Weekly_Sales + Store + Dept + Yr + Wk, dept_train))
    
    mycoef <- lm.fit(as.matrix(train_svd_mm[, -(2:4)]), train_svd_mm$Weekly_Sales)$coefficients
    mycoef[is.na(mycoef)] <- 0
    
    ## Get predictions
    tmp_test = test_split[[i]]
    tmp_pred <- mycoef[1] + as.matrix(tmp_test[, 4:55]) %*% mycoef[-1]
    
    test_pred[[i]] <- cbind(tmp_test[, 2:3], Date = tmp_test$Date, Weekly_Pred = tmp_pred[,1])
  }
  
  test_pred <- bind_rows(test_pred)
}