
source("mymain.R")

# read in train / test dataframes
train <- readr::read_csv('train_ini.csv')
test <- readr::read_csv('test.csv')
#t  = 1

# wae: record weighted mean absolute error WMAE
num_folds <- 10
wae <- rep(0, num_folds)
ptm <- proc.time()

for (t in 1:num_folds) {
  # *** THIS IS YOUR PREDICTION FUNCTION ***
  test_pred <- mypredict()
  
  # read new data from fold_t 
  fold_file <- paste0('fold_', t, '.csv')
  new_train <- readr::read_csv(fold_file, 
                               col_types = cols())
  
  # extract predictions matching up to the new data
  scoring_tbl <- new_train %>% 
    left_join(test_pred, by = c('Date', 'Store', 'Dept'))
  
  # compute WMAE
  actuals <- scoring_tbl$Weekly_Sales
  preds <- scoring_tbl$Weekly_Pred
  preds[is.na(preds)] <- 0
  weights <- if_else(scoring_tbl$IsHoliday, 5, 1)
  wae[t] <- sum(weights * abs(actuals - preds)) / sum(weights)
  
  # update train data and get ready to predict at (t+1)
  train <- train %>% add_row(new_train)
}

sprintf(fmt = "\nTotal execution time: (%.2f seconds)\n", 
        (proc.time() - ptm)[['elapsed']]) %>% cat()

print(wae)
print(mean(wae))
