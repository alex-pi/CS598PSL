
path_resolver = (function(isRemote=TRUE) {
  small_image_url = "https://liangfgithub.github.io/MovieImages/"
  small_image_path = "MovieImages/"
  movies_data_url = "https://liangfgithub.github.io/MovieData/"
  movies_data_path = "MovieData/"

  if(isRemote) {
    remote = list(
      img = function(x) paste0(small_image_url, x, '.jpg?raw=true'),
      mov = paste0(movies_data_url, 'movies.dat?raw=true'),
      rat = paste0(movies_data_url, 'ratings.dat?raw=true'),
      usr = paste0(movies_data_url, 'users.dat?raw=true')
    )    
    return(remote)
  }
  
  local = list(
    # To load images locally they need to be under www/MovieImages folder.
    # img = function(x) paste0(small_image_path, x, '.jpg'),
    img = function(x) paste0(small_image_url, x, '.jpg?raw=true'),
    mov = paste0(movies_data_path, 'movies.dat'),
    # Load ratings from a zip
    rat = unz(paste0(movies_data_path, 'ratings.zip'), 'ratings.dat'),
    usr = paste0(movies_data_path, 'users.dat')
  )
  
  return(local)
})(FALSE) # FALSE means data is found locally. 

get_movies_data = function() {
  # read in data
  
  movies = readLines(path_resolver$mov)
  movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
  movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
  movies = data.frame(movies, stringsAsFactors = FALSE)
  colnames(movies) = c('MovieID', 'Title', 'Genres')
  movies$MovieID = as.integer(movies$MovieID)
  movies$Title = iconv(movies$Title, "latin1", "UTF-8")
  
  print(getwd())
  #small_image_url = "https://liangfgithub.github.io/MovieImages/"
  movies$image_url = sapply(movies$MovieID, path_resolver$img)
  
  # extract year
  movies$Year = as.numeric(unlist(
    lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))  

  return(movies)
}

get_ratings_data = function() {
  # use colClasses = 'NULL' to skip columns
  ratings = read.csv(path_resolver$rat, 
                     sep = ':',
                     colClasses = c('integer', 'NULL'), 
                     header = FALSE)
  colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
  
  return(ratings)
}

get_users_data = function() {
  users = read.csv(path_resolver$usr,
                   sep = ':', header = FALSE)
  users = users[, -c(2,4,6,8)] # skip columns
  colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')  
  
  return(users)
}

get_genres = function(movies) {
  genres_col = movies$Genres
  distinct_genres = c()
  for (i in 1:length(genres_col)) {
    single_genre_set = genres_col[i]
    split_genres = unlist(strsplit(single_genre_set, "|", fixed=TRUE))
    distinct_genres = union(split_genres, distinct_genres)
  } 
  
  return(sort(distinct_genres))
}

read_genres = function() {
  read.csv("MovieData/genres.dat")$x
}

get_top_by_genre = function(movies) {
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
    slice_max(order_by = AvgRating, n = 10)
  
  top_movies_per_genre = left_join(top_movies_per_genre, 
                                   movies[,c("MovieID", "Title", "Year")], 
                                   by = "MovieID")
}

read_top_genres = function() {
  read.csv("MovieData/top_genre.dat")
}

movies = get_movies_data()
ratings = get_ratings_data()
users = get_users_data()
top.genre = read_top_genres()#get_top_by_genre(movies)


