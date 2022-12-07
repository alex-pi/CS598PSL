
ibcf_recom = function(user_ratings, movies, ratings) {
  print("Started IBCF")
  ratings_agg = rbind(ratings[,1:3], user_ratings)
  
  i = paste0('u', ratings_agg$UserID)
  j = paste0('m', ratings_agg$MovieID)
  x = ratings_agg$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  
  train = Rmat[2:501, ]
  test = Rmat[1, ]
  
  num_rates = dim(Rmat)[2]
  
  ####
  
  recommender.IBCF <- Recommender(train, method = "IBCF",
                                  parameter = list(normalize = 'center', 
                                                   method = 'Cosine', 
                                                   k = 30))
  
  p.IBCF <- predict(recommender.IBCF, test, type="ratings")
  p.IBCF <- as.numeric(as(p.IBCF, "matrix"))
  
  p.movids = colnames(train)[which(!is.na(p.IBCF))]
  p.movids = as.integer(sub('m', '', p.movids))
  p.ratings = p.IBCF[!is.na(p.IBCF)]
  
  predictions = data.frame(MovieID=p.movids, 
                           Title=movies[p.movids,]$Title,
                           Rating=p.ratings)
  predictions = predictions %>%
    arrange(desc(Rating))
  
  predictions$Rank = 1:dim(predictions)[1]
  
  print(predictions)
  print("Finished IBCF")
  return(predictions)
}

#user_ratings = data.frame(UserID=c(0,0,0), 
#                          MovieID=c(2,10,40), 
#                          Rating=c(3,5,3))

#ibcf_recom(user_ratings, movies, ratings)
