library(text2vec)
library(glmnet)

#####################################
# Load libraries
# Load your vocabulary and training data
#####################################
myvocab <- scan(file = "myvocab.txt", what = character())
train <- read.table("train.tsv", stringsAsFactors = FALSE, header = TRUE)

train$review <- gsub('<.*?>', ' ', train$review)
it_train = itoken(train$review,
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer)
vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                ngram = c(1L, 4L)))
dtm_train = create_dtm(it_train, vectorizer)

#####################################
# Train a binary classification model
#####################################

NFOLDS = 4
t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = dtm_train, y = train$sentiment, 
                              family = 'binomial', 
                              # L1 penalty
                              alpha = 1,
                              # interested in the area under ROC curve
                              type.measure = "auc",
                              # 4-fold cross-validation
                              nfolds = NFOLDS,
                              # high value is less accurate, but has faster training
                              thresh = 1e-3,
                              # again lower number of iterations for faster training
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))


#####################################
# Load test data, and 
# Compute prediction
#####################################
test <- read.table("test.tsv", stringsAsFactors = FALSE,
                   header = TRUE)

test$review <- gsub('<.*?>', ' ', test$review)
it_test = itoken(test$review,
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer)
vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                ngram = c(1L, 4L)))
dtm_test = create_dtm(it_test, vectorizer)

preds = predict(glmnet_classifier, dtm_test, type = 'response')[,1]

output = cbind(test$id, preds)
output = as.data.frame(output)
colnames(output) = c('id', 'prob')

#####################################
# Store your prediction for test data in a data frame
# "output": col 1 is test$id
#           col 2 is the predicted probs
#####################################
write.table(output, file = "mysubmission.txt", 
            row.names = FALSE, sep='\t')