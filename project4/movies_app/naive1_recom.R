

naive1_recom = function(user_ratings, movies) {
  print("Started Naive.")
  movies_ratings = left_join(movies, ratings, by = "MovieID")
  movies_avg_ratings = movies_ratings %>% 
    group_by(MovieID) %>% 
    summarise_at(vars(Rating), list(AvgRating = mean))
  
  movies_avg_ratings = left_join(movies_avg_ratings, 
                                 movies[,c("MovieID", "Genres")], by = "MovieID")
  movies_avg_ratings = movies_avg_ratings %>% 
    separate_rows(Genres, sep='\\|') %>% 
    arrange(Genres, desc(AvgRating))
  top_movies_per_genre = movies_avg_ratings %>% 
    group_by(Genres) %>% 
    slice_max(order_by = AvgRating, n = 5)
  
  top_movies_per_genre = left_join(top_movies_per_genre, 
                                   movies[,c("MovieID", "Title", "Year")], 
                                   by = "MovieID")
  
  #user_ratings = data.frame(MovieID=c(2,10), Rating=c(2,4))
  
  ratings_user1 = left_join(user_ratings, movies, by = "MovieID")
  ratings_user1 = ratings_user1 %>% 
    separate_rows(Genres, sep='\\|') %>% 
    group_by(Genres) %>% 
    summarise_at(vars(Rating), list(AvgRating = mean)) %>% 
    slice_max(order_by = AvgRating, n = 1)
  
  top_genre_user1 = ratings_user1$Genres
  
  print("Here1")
  
  predictions = top_movies_per_genre %>% 
    filter(Genres %in% top_genre_user1) %>% 
    mutate(Rating = round(AvgRating, digits=0)) %>% 
    arrange(desc(AvgRating))
  
  predictions$Rank = 1:dim(predictions)[1]
  
  print(predictions)
  
  print("Finished Naive.")
  predictions
}

#naive1_recom(data.frame(MovieID=c(2,10), Rating=c(2,4)))
