
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

  p.IBCF = predict(recommender.IBCF, test, type="ratings")
  am = as(p.IBCF, "matrix")
  p.IBCF = as.numeric(am)
  
  p.movids = colnames(am)[which(!is.na(p.IBCF))]
  p.movids = strtoi( gsub('m', '', p.movids))
  p.ratings = p.IBCF[!is.na(p.IBCF)]
  
  movie_preds = movies %>% filter( MovieID %in% p.movids )
  
  predictions = data.frame(MovieID=movie_preds$MovieID, 
                           Title=movie_preds$Title,
                           Rating=p.ratings)
  predictions = predictions %>%
    arrange(desc(Rating))

  #print(predictions)
  print("Finished IBCF")
  return(predictions)
}

#user_ratings = data.frame(UserID=c(0,0,0,0,0,0,0,0,0,0), 
#                          MovieID=c(47,13,32,10,45,22,2,44,31,1), 
#                          Rating=c(4,1,5,3,3,3,3,2,4,5))

#user_ratings = data.frame(UserID=c(0), 
#                        MovieID=c(10), 
#                        Rating=c(1))

#ppp=ibcf_recom(user_ratings, movies, ratings)
