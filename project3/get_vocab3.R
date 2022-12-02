library( text2vec )
library( glmnet )
library( dplyr )

# global variables
sample_rate = 0.55    # default: 0.6
loss_type   = 1       # 0=L2 (ridge), 1=L1 (lasso)
nb_terms    = 1000    # max number of terms for resulting vocabulary

# start the clock
time_start = proc.time()

# read data from file
train = read.table( "alldata.tsv", stringsAsFactors = FALSE, header = TRUE )

#--------------------------------------
# regular expressions to filter the text before processing.
# Uncomment the line to enable it.
#--------------------------------------

# remove contractions and possessives, then merge the floating 's' and/or 't' to the word
# train$review = gsub( "[\'\"]", "", train$review )   # prevent orphaned t's and s's.

# remove HTML tags
train$review <- gsub( '<.*?>', ' ', train$review )

# remove underscores to force multi-word terms to words.
# train$review <- gsub( '[_]', ' ', train$review )

# remove non-alphanumeric characters
# train$review <- gsub( '[^a-zA-Z0-9]', ' ', train$review )

# remove non-alphabetic characters
# train$review <- gsub( '[^a-zA-Z]', ' ', train$review )


#------------------------------------------
# stop words
#------------------------------------------

stopwords = c("i", "me", "my", "myself", 
               "we", "our", "ours", "ourselves", 
               "you", "your", "yours", 
               "their", "they", "his", "her", 
               "she", "he", "a", "an", "and",
               "is", "was", "are", "were", 
               "him", "himself", "has", "have", 
               "it", "its", "the", "us"
)

# separate reviews into positive and negative
review_positive = train$review[ train$score > 5 ]
review_negative = train$review[ train$score < 5 ]

# split into word string, then flatten into a vector of words
# library( purrr )
# words_positive = flatten_chr( str_split( review_positive, " ", simplify=FALSE ) )
# words_negative = flatten_chr( str_split( review_negative, " ", simplify=FALSE ) )

str_positive = paste( review_positive, collapse=" " )
str_negative = paste( review_negative, collapse=" " )

words_positive = strsplit( str_positive, "[ ,\n]" )[[1]]
words_negative = strsplit( str_negative, "[ ,\n]" )[[1]]

# find words common to both positive and negative reviews
words_common = intersect( words_positive, words_negative )

words_all      = union( words_positive, words_negative )
words_uncommon = setdiff( words_all, words_common )

# remove garbage, empty entries, and NA
words_common = words_common[ words_common != "" ]
words_common = words_common[ !is.na( words_common ) ]

words_uncommon = words_uncommon[ words_uncommon != "" ]
words_uncommon = words_uncommon[ !is.na( words_uncommon ) ]

#-------------------------------------------------
# append modifications to the stop_words list so they 
# are dropped from the vocabulary.
# uncomment a line to enable it.  Choose only 1 at a time.
#-------------------------------------------------
# stop_words = union( stopwords, words_common )  # words in pos AND neg reviews
# stop_words = union( stopwords, words_uncommon ) # words in pos OR neg reviews
stop_words = c( "" )      # full vocabulary - no words removed.


it_train = itoken( 
  train$review, 
  preprocessor = tolower, 
  tokenizer    = word_tokenizer
)

tmp.vocab = create_vocabulary(
  it_train, 
  stopwords = stop_words, 
  ngram = c( 1L, 4L ) 
)

tmp.vocab = prune_vocabulary(
  tmp.vocab, 
  term_count_min     = 10,     # default: 10
  doc_proportion_min = 0.001,  # default: 0.001
  doc_proportion_max = 0.5     # default: 0.5
)

dtm_train  = create_dtm( it_train, vocab_vectorizer( tmp.vocab ) )

T_         = 50; 
term_count = data.frame( myvocab = "", freq = 0 )

str = NULL

for( i in 1:T_ ) {
  
  cat( str, "--- ", i, " (of ", T_, ") ---\n", sep="" )
  
  # randomly sample a subset of the data, e.g., 60%
  n             = dim( dtm_train )[1]
  sample_idx    = sample( 1:n, (n * sample_rate), replace=F )
  dtm_train_sub = dtm_train[ sample_idx, ]
  y_subset      = train$sentiment[ sample_idx ]
  
  # try lasso on this subset
  tmpfit = glmnet(
    x      = dtm_train_sub,
    y      = y_subset, 
    alpha  = loss_type,
    family ='binomial' 
  )
  
  # record selected variables using lambda.min
  idx       = which.min( tmpfit$lambda )
  myvocab   = colnames( dtm_train_sub )[ which( tmpfit$beta[, idx] != 0 ) ]
  sub_vocab = as_tibble( table( myvocab ) )
  
  term_count = sub_vocab %>% 
    full_join( term_count, by = "myvocab") %>%
    replace( is.na(.), 0 ) %>%
    mutate( freq = n + freq ) %>%
    select( -c(n) )
}

term_count = term_count %>% arrange( desc( freq ) )

time_end = proc.time()

write( term_count[1:nb_terms, ]$myvocab, "myvocab3.txt" )

cat( str, "Total time: ", (time_end - time_start)[3], "seconds \n" )
