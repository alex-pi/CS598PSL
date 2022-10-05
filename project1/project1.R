library(glmnet) 
library(dplyr)
library(ggplot2)
library(xgboost)
library(gbm)

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

save_splits(data, testIDs)

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

report_nulls_zeros = function(df) {

  null_vals <- sum(is.na(df))
  
  # List of columns with missing values
  null_cols <- which(colSums(is.na(df))>0)
  
  # Reporting back
  sprintf(fmt="We are missing %d values in our data at given percentages in the following columns:\n",
          null_vals) %>% cat()
  
  for (i in null_cols)
  {
    col_name <- names(df[, i, drop=FALSE])
    null_val <- sum(is.na(df[col_name]))
    null_per <- (null_val / nrow(df))*100
    sprintf(fmt = " -%s: %d (%.2f%%)\n", 
            col_name, null_val, null_per) %>% cat()
  }
  
  zero_vals <- sum(df==0, na.rm = TRUE)
  
  # List of columns with missing values
  zero_cols <- which(colSums(df==0, na.rm = FALSE) > 0)
  
  print('Now reporting number of zeros.')
  # Reporting back
  for (i in zero_cols)
  {
    col_name <- names(df[, i, drop=FALSE])
    zero_val <- sum(df[col_name] == 0)
    zero_per <- (zero_val / nrow(df))*100
    sprintf(fmt = " -%s: %d (%.2f%%)\n", 
            col_name, zero_val, zero_per) %>% cat()
  }
}

drop_irrelevant = function(df, cols) {
  idxs = which(names(df) %in% cols)
  
  #c('Street', 'Utilities', 'Condition_2', 'Roof_Matl', 'Heating', 'Pool_QC', 
  #  'Misc_Feature', 'Low_Qual_Fin_SF', 'Pool_Area', 'Longitude','Latitude')
  df = subset(df, select = -idxs )
  return(df)
}

convert_factors = function(df) {

  fac_names <- names(df[, sapply(df, class) == "character"])
  df[fac_names] <- lapply(df[fac_names], factor)
  
  cat_nominals = c('Full_Bath', 'Garage_Cars', 'Bedroom_AbvGr', 
                   'Kitchen_AbvGr', 'Fireplaces')

  df[cat_nominals] <- lapply(df[cat_nominals], factor)
    
  return(df)
}

add_product_interactions = function(df) {
  
  classes <- sapply(df, class)
  df_numeric = df[, (classes == "numeric" | classes == "integer")]
  num_names = colnames(df_numeric)
  
  for (cname in num_names) {
    if(cname == 'Sale_Price') {
      next
    }
    
    for (cnamet in num_names) {
      if(cnamet == 'Sale_Price') {
        next
      }      
      new_name = paste(cname, 'x', cnamet, sep='')
      #print(new_name)
      df[new_name] = as.numeric(df[, cname]) * as.numeric(df[, cnamet])
    }
  }
  
  return(df)
  
}

log_transform_data = function(df, cols = c()) {
  
  classes <- sapply(df, class)
  df_numeric = df[, (classes == "numeric" | classes == "integer")]
  print(names(df_numeric))
  #sk_test = lapply(df_numeric, skewness)
  alpha = 0.01
  
  for(name in names(df_numeric)) {
    all_the_same = (length(unique(df_numeric[, name])) == 1)
    if(all_the_same) {
      next
    }
    sw_test = shapiro.test(df_numeric[, name])
    #print(sw_test)
    p_val = sw_test$p.value
    if(p_val >= alpha || any(df_numeric[, name] < 0)) 
      next
    
    df_numeric[which(df_numeric[name] == 0), name] = 1
    
    df_numeric[, name] = log(df_numeric[, name])
  }
  
  df[names(df_numeric)] = df_numeric
  
  return(df)
  
}

winsorization = function(df, quantile_cut = 0.95) {
  winsor.vars <- c("Lot_Frontage", "Lot_Area", 
                   "Mas_Vnr_Area", "BsmtFin_SF_2", 
                   "Bsmt_Unf_SF", "Total_Bsmt_SF", 
                   "Second_Flr_SF", 'First_Flr_SF', 
                   "Gr_Liv_Area", "Garage_Area", 
                   "Wood_Deck_SF", "Open_Porch_SF", 
                   "Enclosed_Porch", "Three_season_porch",
                   "Screen_Porch", "Misc_Val")
 
  for(var in winsor.vars){
    tmp <- df[, var]
    myquan <- quantile(tmp, probs = quantile_cut, na.rm = TRUE)
    tmp[tmp > myquan] <- myquan
    df[, var] <- tmp
  }
  
  return(df)
}

expand_predictors = function(df) {
  exp_df = as.data.frame(model.matrix(~., df))
  
  colnames(exp_df)[1] = 'Reference_Level'
  #exp_df = exp_df[, -1]
  
  print(paste("After expand: ", dim(exp_df)))
  
  return(exp_df)
}

expand_factors = function(df) {
  #df # train data without "PID" and "Sale_Price"
  
  categorical.vars = colnames(df)[
    which(sapply(df,
                 function(x) class(x)=="character"))]
  
  # Keep only no categoricals
  new_df = df[, !colnames(df) %in% categorical.vars, 
              drop=FALSE]
  
  n.train <- nrow(new_df)
  for(var in categorical.vars){
    mylevels <- sort(unique(df[, var]))
    m <- length(mylevels)
    m <- ifelse(m>2, m, 1)
    new_columns <- matrix(0, n.train, m)
    col.names <- NULL
    for(j in 1:m){
      new_columns[df[, var]==mylevels[j], j] <- 1
      col.names <- c(col.names, paste(var, '_', mylevels[j], sep=''))
    }
    colnames(new_columns) <- col.names
    new_df <- cbind(new_df, new_columns)
  }
  
  return(new_df)
}

order_predictors = function(df) {
  
  df = df[order(colnames(df))]
  
  return(df)
}

conciliate_predictors = function(dftrnames, dfte) {
  
  #dftrnames = colnames(dftr)[order(colnames(dftr))]
  dftrnames = dftrnames[order(dftrnames)]
  dftenames = colnames(dfte)[order(colnames(dfte))]
  
  (missing_lvls = setdiff(dftrnames, dftenames))
  (new_lvls = setdiff(dftenames, dftrnames))
  #sprintf(fmt = "Missing predictors on test set: %s\n", 
  #        missing_lvls) %>% cat()
  #sprintf(fmt = "New predictors on test set: %s\n", 
  #        new_lvls) %>% cat()  
  
  ## Merge
  dfte[missing_lvls] = 0
  dfte = dfte[, -which(colnames(dfte) %in% new_lvls)]  
  
  #dftr = dftr[order(colnames(dftr))]
  dfte = dfte[order(colnames(dfte))]
  
  sprintf(fmt = "Are both sets equal: %s\n", 
          all.equal(dftrnames, colnames(dfte))) %>% cat() 
  
  return(list(
    test_df = dfte,
    #train_df = dftr,
    cols_added = missing_lvls,
    cols_removed = new_lvls
  ))
}

lasso_eval = function(X, Y, X_test, Y_test) {
  ## Lasso/Ridge testing
  cv.out = cv.glmnet(X, Y, alpha = 1) 
  sel.vars = predict(cv.out, type="nonzero", 
                     s = cv.out$lambda.min)$s1
  
  cv.out = cv.glmnet(as.matrix(X[, sel.vars]), 
                     Y, alpha = 0)
  
  Ytest.pred = predict(cv.out, s = cv.out$lambda.min, 
                       newx = X_test[, sel.vars])
  
  lasso_ridge_rmse = sqrt(mean((log(Y_test) - Ytest.pred)^2))
  
  cv.out = cv.glmnet(X, Y, alpha = 0.2)
  
  Ytest.pred = predict(cv.out, s = cv.out$lambda.min, newx = X_test)
  
  elas_rmse = sqrt(mean((log(Y_test) - Ytest.pred)^2))
  
  return(list(
    lasso_ridge_rmse = lasso_ridge_rmse,
    elas_rmse = elas_rmse
  ))
}

boosting_eval = function(X, Y, X_test, Y_test) {
  
  gbm.model <- gbm.fit(
    x = X,
    y = Y,
    distribution = "gaussian",
    n.trees = 7000,
    interaction.depth = 3,
    shrinkage = 0.1,
    #cv.folds = 5,
    #n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  Ytest.pred = predict(gbm.model, n.trees = gbm.model$n.trees, 
                       X_test)
  gbm_rmse = sqrt(mean((log(Y_test) - Ytest.pred)^2))  
  
  
  xgb.model <- xgboost(data = X, 
                       label = Y, max_depth = 6,
                       eta = 0.05, nrounds = 5000,
                       subsample = 0.5,
                       verbose = FALSE)
  
  Ytest.pred = predict(xgb.model, X_test)
  xgb_rmse = sqrt(mean((log(Y_test) - Ytest.pred)^2))
  
  return(list(
    xgb_rmse = xgb_rmse,
    gbm_rmse = gbm_rmse
  ))
}

test_split = function(jth_split, evaluation_fn) {
  ## Prepare jth split
  dataj = load_split(jth_split)
  
  # Remove PID
  test = dataj$test[, -1]
  train = dataj$train[, -1]
  
  ## Only one field has NAs
  train$Garage_Yr_Blt[is.na(train$Garage_Yr_Blt)] = 0
  test$Garage_Yr_Blt[is.na(test$Garage_Yr_Blt)] = 0
  
  ## Drop assumed irrelevant
  irrelevant_cols = c('Street',
                      'Utilities',
                      'Condition_2',
                      'Roof_Matl',
                      'Heating',
                      'Pool_QC',
                      'Low_Qual_Fin_SF',
                      'Pool_Area',
                      'Longitude','Latitude'
  )
  train = drop_irrelevant(train, irrelevant_cols)
  test = drop_irrelevant(test, irrelevant_cols)
  
  ## winsorization
  train = winsorization(train)
  test = winsorization(test)
  
  ## Transform factors
  #test = convert_factors(test)
  #train = convert_factors(train)
  
  # get the index of Sale_Price
  y_index = length(names(train))
  
  ## log transforms
  #test = log_transform_data(test)
  #train = log_transform_data(train)
  
  train$Sale_Price = log(train$Sale_Price)
  

  train.x = train[, -y_index]
  train.y = train[, y_index, drop=FALSE]
  #sel_cols = names(train)[-y_index]
  #test = test[, sel_cols]
  
  ## product interactions
  ##train.x = add_product_interactions(train.x)
  ##test = add_product_interactions(test)
  
  ## Expand predictors
  #[, -y_index]
  #train_exp = expand_predictors(train.x)
  #train = cbind(train_exp, train.y)
  #test = expand_predictors(test)
  
  ## Encode factors
  train.x = expand_factors(train.x)
  train.x = order_predictors(train.x)
  #print(colnames(train.x))
  train = cbind(train.x, train.y)
  test = expand_factors(test)
  
  
  ## Conciliate before testing
  test_c = conciliate_predictors(colnames(train.x), test)
  
  ## Matrix forms
  #X_test = data.matrix(test)
  X_test = data.matrix(test_c$test_df)
  Y_test = data.matrix(dataj$y[, 2])
  
  X = data.matrix(train.x)  
  Y = train$Sale_Price
  
  return(evaluation_fn(X, Y, X_test, Y_test))
}

uin_4 = 15052
set.seed(uin_4)

#rmse = rep(0, list())

benchmk = 0.125
for (i in 1:5) {
  print(paste("###### Test on split ", i, "#####"))
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


uin_4 = 15052
set.seed(uin_4)

benchmk = 0.125
for (i in 1:5) {
  print(paste("###### Test on split ", i, "#####"))
  rmse = test_split(i, boosting_eval)
  print(rmse)
  print(rmse < benchmk)
}

uin_4 = 15052
set.seed(uin_4)

benchmk = 0.135
for (i in 1:5) {
  print(paste("###### Test on split ", (i+5), "#####"))
  rmse = test_split(i+5, boosting_eval)
  print(rmse)
  print(rmse < benchmk)
}




