# Step 0: Load necessary libraries
library(glmnet, warn.conflicts = FALSE, quietly = TRUE) 
library(xgboost, warn.conflicts = FALSE, quietly = TRUE)
###########################################
# Step 0: Load necessary libraries
#
# Here I define common functions.
# 

drop_irrelevant = function(df, cols) {
  idxs = which(names(df) %in% cols)
  
  df = subset(df, select = -idxs )
  return(df)
}

winsorization = function(df, quantile_cut = 0.95) {
  winsor.vars <- c("Lot_Frontage", "Lot_Area", 
                   "Mas_Vnr_Area", "BsmtFin_SF_2", 
                   "Bsmt_Unf_SF", "Total_Bsmt_SF", 
                   "Second_Flr_SF", 'First_Flr_SF', 
                   "Gr_Liv_Area", "Garage_Area", 
                   "Wood_Deck_SF", "Open_Porch_SF", 
                   "Enclosed_Porch", 
                   "Screen_Porch"
  )
  
  for(var in winsor.vars) {
    tmp <- df[, var]
    myquan <- quantile(tmp, probs = quantile_cut, na.rm = TRUE)
    tmp[tmp > myquan] <- myquan
    df[, var] <- tmp
  }
  
  return(df)
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

# Only used in the prediction phase
conciliate_predictors = function(dftrnames, dfte) {
  
  #dftrnames = colnames(dftr)[order(colnames(dftr))]
  dftrnames = dftrnames[order(dftrnames)]
  dftenames = colnames(dfte)[order(colnames(dfte))]
  
  (missing_lvls = setdiff(dftrnames, dftenames))
  (new_lvls = setdiff(dftenames, dftrnames))
  
  ## Merge
  dfte[missing_lvls] = 0
  dfte = dfte[, -which(colnames(dfte) %in% new_lvls)]  
  
  dfte = dfte[order(colnames(dfte))]
  
  #sprintf(fmt = "Are both sets equal: %s\n", 
  #        all.equal(dftrnames, colnames(dfte))) %>% cat() 
  
  return(list(
    test_df = dfte,
    cols_added = missing_lvls,
    cols_removed = new_lvls
  ))
}

###########################################
# Step 1: Preprocess training data
#         and fit two models
#
train <- read.csv("train.csv", stringsAsFactors = FALSE)
#
# YOUR CODE
# 

# Setting a seed
set.seed(15052)

# Remove PID
train = train[, -1]

## Only one field has NAs
train$Garage_Yr_Blt[is.na(train$Garage_Yr_Blt)] = 0

## Drop assumed irrelevant
irrelevant_cols = c('Street',
                    'Utilities',
                    'Condition_2',
                    'Roof_Matl',
                    'Heating',
                    'Low_Qual_Fin_SF',
                    "Three_season_porch", #winzo?
                    'Pool_Area',
                    'Pool_QC',
                    'Misc_Feature', #?
                    'Misc_Val', #winzo?
                    'Longitude','Latitude'
)
train = drop_irrelevant(train, irrelevant_cols)

## winsorization
train = winsorization(train)

# get the index of Sale_Price
y_index = length(names(train))

train$Sale_Price = log(train$Sale_Price)

train.x = train[, -y_index]
train.y = train[, y_index, drop=FALSE]

## Encode factors
train.x = expand_factors(train.x)
train.x = order_predictors(train.x)
train_names = colnames(train.x)
#print(colnames(train.x))
train = cbind(train.x, train.y)

X = data.matrix(train.x)  
Y = train$Sale_Price

lasso.out = cv.glmnet(X, Y, alpha = 1) 
sel.vars = predict(lasso.out, type="nonzero", 
                   s = lasso.out$lambda.min)$s1

ridge.out = cv.glmnet(as.matrix(X[, sel.vars]), 
                      Y, alpha = 0)

xgb.model <- xgboost(data = X, 
                     label = Y, max_depth = 6,
                     eta = 0.05, nrounds = 5000,
                     subsample = 0.5,
                     verbose = FALSE)

###########################################
# Step 2: Preprocess test data
#         and output predictions into two files
#
test <- read.csv("test.csv", stringsAsFactors = FALSE)
#
# YOUR CODE
# 

# Remove PID
pids = test[, 1]
test = test[, -1]

test$Garage_Yr_Blt[is.na(test$Garage_Yr_Blt)] = 0

test = drop_irrelevant(test, irrelevant_cols)

test = winsorization(test)

test = expand_factors(test)

## Conciliate before testing
test_c = conciliate_predictors(train_names, test)

## Matrix forms
#X_test = data.matrix(test)
X_test = data.matrix(test_c$test_df)

Ytest.pred = predict(ridge.out, s = ridge.out$lambda.min, 
                     newx = X_test[, sel.vars])

colnames(Ytest.pred) = c('Sale_Price')
mysubmission1_df = data.frame(PID=pids, exp(Ytest.pred), row.names = NULL)
write.csv(mysubmission1_df, "mysubmission1.txt", row.names=FALSE)

Ytest.pred = predict(xgb.model, X_test)

names(Ytest.pred) = c('Sale_Price')
mysubmission2_df = data.frame(PID=pids, Sale_Price=exp(Ytest.pred), row.names = NULL)
write.csv(mysubmission2_df, "mysubmission2.txt", row.names=FALSE)
