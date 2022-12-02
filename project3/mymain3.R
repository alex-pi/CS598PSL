library( text2vec )
library( glmnet )
require( xgboost )

#####################################
# Load libraries
# Load your vocabulary and training data
#####################################
myvocab <- scan( file = "myvocab3.txt", what = character() )
train   <- read.table( "train.tsv", stringsAsFactors = FALSE, header = TRUE )

# train$review <- gsub( "[\'\"]", "", train$review )   # remove contractions/possessives
train$review <- gsub( '<.*?>', ' ', train$review )     # remove HTML tags
if ( regexp != "" ) {
  train$review <- gsub( regexp, ' ', train$review )   # other
}

it_train   = itoken( train$review, preprocessor = tolower, tokenizer = word_tokenizer )
vectorizer = vocab_vectorizer( create_vocabulary( myvocab, ngram = n_gram ) )
dtm_train  = create_dtm( it_train, vectorizer )

#####################################
# Train a binary classification model
#####################################

NFOLDS = nb_folds
t1     = Sys.time()

if ( loss_type == 2 ) {
  # Gradient boosting tree
  xgboost_classifier <- xgboost(
    data      = as.matrix( dtm_train ),
    label     = train$sentiment,
    max_depth = 4,
    eta       = 0.115,
    nrounds   = max_iterations,
    subsample = sample_rate,
    verbose   = FALSE
  )
} else {
  # ridge/lasso regularization
  glmnet_classifier = cv.glmnet(
    x            = dtm_train,
    y            = train$sentiment,
    family       = 'binomial',
    alpha        = loss_type,       # 0=L2 penalty (Ridge), 1=L1 penalty (lasso)
    type.measure = "auc",           # interested in area under ROC curve
    nfolds       = NFOLDS,          # 4-fold cross-validation
    thresh       = threshold,       # high value is less accurate, but faster training
    maxit        = max_iterations   # again lower number of iterations for faster training
  )
}

d_time = difftime( Sys.time(), t1, units = 'sec' )
print( d_time )
results[j,1] = d_time

#####################################
# Load test data, and 
# Compute prediction
#####################################
test <- read.table( "test.tsv", stringsAsFactors = FALSE, header = TRUE )

# test$review = gsub( "[\'\"]", "", test$review )   # remove contractions/possessives
test$review <- gsub( '<.*?>', ' ', test$review )    # remove HTML tags
if ( regexp != "" ) {
  test$review <- gsub( regexp, ' ', test$review )   # other
}

it_test    = itoken( test$review, preprocessor = tolower, tokenizer = word_tokenizer )
vectorizer = vocab_vectorizer( create_vocabulary( myvocab, ngram = n_gram ) )
dtm_test   = create_dtm( it_test, vectorizer )

if ( loss_type == 2 ) {
  # gradient boosting tree
  preds = predict( xgboost_classifier, dtm_test, type = 'response' )
} else {
  # Lasso/ridge regression
  preds = predict( glmnet_classifier, dtm_test, type = 'response' )[,1]
}

output = cbind( test$id, preds )
output = as.data.frame( output )
colnames( output ) = c( 'id', 'prob' )

#####################################
# Store your prediction for test data in a data frame
# "output": col 1 is test$id
#           col 2 is the predicted probs
#####################################
write.table( output, file = "mysubmission.txt", row.names = FALSE, sep='\t' )
