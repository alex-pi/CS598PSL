get_train_svd = function(train_, num_comp = 8) {
  
  train_split = train_ %>% 
    group_split(Dept)
  
  svd_list <- vector(mode = "list", length = length(train_split))
  
  for (i in 1:length(train_split)) {
    #print(paste("######## i = ", i, "########"))
    dept_train = train_split[[i]][,1:4]
    
    #num_comp = 12
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
      
    }
    
    svd_list[[i]] = train_svd_
  }
  
  train_svd = bind_rows(svd_list)
  
  train_new = train_svd %>%
    gather(Date, Weekly_Sales, -Store, -Dept)
  
  return(train_new)
}

dim(train)
train_new = get_train_svd(train)
