
ibcf_recom = function(user_ratings, movies, ratings) {
  print("Started IBCF")
  if(dim(user_ratings)[1] == 0)
    return(NULL)
  # Add ratings from to UI to the ratings matrix.
  ratings_agg = rbind(ratings[,1:3], user_ratings[,1:3])
  
  # Create a sparse matrix with users as rows and movies as columns.
  # i,j is the rating of user i for movie j.
  i = paste0('u', ratings_agg$UserID)
  j = paste0('m', ratings_agg$MovieID)
  x = ratings_agg$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  
  # Due to memory issues we limit IBCF to ratings of 500 users.
  train = Rmat[2:501, ]
  # First user in the matrix corresponds to the ratings given in the UI.
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
  
  valid_indices = which( !is.na( p.IBCF ))
  
  p.movids = colnames(am)[valid_indices]
  p.movids = strtoi( gsub('m', '', p.movids))
  p.ratings = p.IBCF[valid_indices]
  
  predictions = data.frame(MovieID=p.movids, 
                           Predicted_rating=p.ratings)
  
  predictions = predictions %>%
    arrange(desc(Predicted_rating))
  
  predictions = left_join(predictions, movies, by="MovieID" )

  #print("predictions from IBCF")
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
