
test_ratings = ratings[ratings$UserID==1449, ]

test_ratings = test_ratings %>%
  arrange(desc(Rating))

dim(test_ratings)

test_mr = left_join( test_ratings, movies, by="MovieID" )

colnames(test_mr)

test_mr[1:100,c("Title","Rating")]
test_mr[1:100,c("MovieID")]

testm = as(test, "matrix")
colnames(testm)[which(!is.na(testm))][1:100]



recommender.IBCF <- Recommender(
  train, 
  method = "IBCF",
  parameter = list( normalize = 'center', method = 'Cosine', k = N )
)

p.IBCF <- predict( recommender.IBCF, test, type="ratings" )

am = as(p.IBCF, "matrix")
p.IBCF = as.numeric(am)

p.movids = colnames(am)[which(!is.na(p.IBCF))]

p.movids = strtoi( gsub('m', '', p.movids))
p.ratings = p.IBCF[!is.na(p.IBCF)]

predictions = data.frame(MovieID=p.movids, 
                         Predicted_rating=p.ratings)

predictions = predictions %>%
  arrange(desc(Predicted_rating))

predictions[1:10,]

predictions = left_join(predictions[1:10,], movies, by="MovieID" )

predictions[1:10,]


top_n_movies = head( order( p.IBCF, decreasing = TRUE, na.last = TRUE), 10 )

p.IBCF[top_n_movies]

movies[ top_n_movies, ]$MovieID

p.movids2 = colnames(am)[which(!is.na(p.IBCF))]
length(p.movids2)
