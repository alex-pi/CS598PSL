library(text2vec)
library(glmnet)
library(dplyr)

train = read.table("alldata.tsv",
                   stringsAsFactors = FALSE,
                   header = TRUE)
train$review = gsub('<.*?>', ' ', train$review)

stop_words = c("i", "me", "my", "myself", 
               "we", "our", "ours", "ourselves", 
               "you", "your", "yours", 
               "their", "they", "his", "her", 
               "she", "he", "a", "an", "and",
               "is", "was", "are", "were", 
               "him", "himself", "has", "have", 
               "it", "its", "the", "us")
it_train = itoken(train$review,
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer)
tmp.vocab = create_vocabulary(it_train, 
                              stopwords = stop_words, 
                              ngram = c(1L,4L))
tmp.vocab = prune_vocabulary(tmp.vocab, term_count_min = 10,
                             doc_proportion_max = 0.5,
                             doc_proportion_min = 0.001)
dtm_train  = create_dtm(it_train, vocab_vectorizer(tmp.vocab))

T_ = 50; 
term_count = data.frame(myvocab = "", freq = 0)

for( i in 1:T_){
  print(paste("#####", i))
  # randomly sample a subset of the data, e.g., 60%
  n = dim(dtm_train)[1]
  sample_idx = sample(1:n, n*0.6, replace=F)
  dtm_train_sub = dtm_train[sample_idx, ]
  y_subset = train$sentiment[sample_idx]
  # try lasso on this subset
  tmpfit = glmnet(x = dtm_train_sub, 
                  y = y_subset, 
                  alpha = 1,
                  family='binomial')
  
  # record selected variables using lambda.min
  idx = which.min(tmpfit$lambda)
  myvocab = colnames(dtm_train_sub)[which(tmpfit$beta[, idx] != 0)]
  sub_vocab = as_tibble(table(myvocab))
  
  term_count = sub_vocab %>% 
    full_join(term_count, by = "myvocab") %>%
    replace(is.na(.), 0) %>%
    mutate(freq = n + freq) %>%
    select(-c(n))
}

term_count = term_count %>%
  arrange(desc(freq))

write(term_count[1:982, ]$myvocab, "myvocab2.txt")
