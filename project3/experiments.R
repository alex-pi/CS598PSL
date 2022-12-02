

mylasso.coef.min = predict(glmnet_classifier, s=glmnet_classifier$lambda.min, type="coefficients")
mylasso.coef.1se = predict(glmnet_classifier, s=glmnet_classifier$lambda.1se, type="coefficients")
#cbind(mylasso.coef.min, mylasso.coef.1se)

# number of variables selected (including the intercept)
print(paste("Positive coefficients:", sum(mylasso.coef.1se > 1)))
print(paste("Negative coefficients:", sum(mylasso.coef.1se < 0)))

mylasso.coef.1se_e = exp(mylasso.coef.1se)

# names of selected non-intercept variables
# row.names(mylasso.coef.1se)[nonzeroCoef(mylasso.coef.1se)[-1]]
pos_words = row.names(mylasso.coef.1se_e)[which(mylasso.coef.1se_e > 1)][-1]
pos_coef = mylasso.coef.1se_e[which(mylasso.coef.1se_e > 1)][-1]
posdf = data.frame(pos_words=pos_words, pos_coef=pos_coef)
posdf[order(posdf$pos_coef, decreasing = TRUE), ][1:10,]
posdf$pos_coefl = log(posdf$pos_coef)

posdf[posdf$pos_words == "this_excellent",]
posdf[posdf$pos_words == "excellent",]
posdf[posdf$pos_words == "also",]
posdf[posdf$pos_words == "very_entertaining",]

posdf$pos_coef_sc = scale(posdf$pos_coef, scale=TRUE, center = FALSE)

posdf$pos_coef_sc = scales::rescale(posdf$pos_coef, to = c(0, 10))

wordcloud(words = posdf$pos_words, freq = posdf$pos_coef,
          #scale = c(4, .3),
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

neg_words = row.names(mylasso.coef.1se_e)[which(mylasso.coef.1se_e < 1)][-1]
neg_coef = mylasso.coef.1se_e[which(mylasso.coef.1se_e < 1)][-1]
negdf = data.frame(neg_words=neg_words, neg_coef=neg_coef)  
negdf[order(negdf$neg_coef, decreasing = FALSE), ][1:10,]

negdf$neg_coef_sc = scale(negdf$neg_coef, scale=TRUE, center = FALSE)

negdf$neg_coef_sc = scales::rescale(negdf$neg_coef, to = c(0, 10))

wordcloud(words = negdf$neg_words, freq = negdf$neg_coef, 
          random.order = TRUE, 
          colors = brewer.pal(8, "Dark2"))

pos_words[sample(1:length(pos_words), 10)]

##########

train = read.table("alldata.tsv",
                   stringsAsFactors = FALSE,
                   header = TRUE)

train$review = gsub('<.*?>', ' ', train$review)

idxs = grep('[0-9]{1}/10', train$review)

idxs = grep('7.{1-4}10', train$review)

idxs = grep('based on a true', train$review)

length(idxs)

train[idxs , ]$sentiment

mean(train[idxs , ]$sentiment)

##########

setwd('..')
j = 1
wd = paste( getwd( ), "/split_", j, sep = '')
setwd(wd)
test <- read.table("test.tsv", stringsAsFactors = FALSE, header = TRUE)



colnames(test)
which(test$id == 598)
which(test$id == 22235)

test[train$id == 598, ]
test[train$id == 22235, ]

test <- read.table("../alldata.tsv", stringsAsFactors = FALSE, header = TRUE)










