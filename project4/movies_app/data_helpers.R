
myurl = "https://liangfgithub.github.io/MovieData/"

get_movies_data = function() {
  # read in data
  
  movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
  movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
  movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
  movies = data.frame(movies, stringsAsFactors = FALSE)
  colnames(movies) = c('MovieID', 'Title', 'Genres')
  movies$MovieID = as.integer(movies$MovieID)
  movies$Title = iconv(movies$Title, "latin1", "UTF-8")
  
  small_image_url = "https://liangfgithub.github.io/MovieImages/"
  movies$image_url = sapply(movies$MovieID, 
                            function(x) paste0(small_image_url, x, '.jpg?raw=true'))
  
  # extract year
  movies$Year = as.numeric(unlist(
    lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))  
  
  return(movies)
}

get_ratings_data = function() {
  # use colClasses = 'NULL' to skip columns
  ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                     sep = ':',
                     colClasses = c('integer', 'NULL'), 
                     header = FALSE)
  colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
  
  return(ratings)
}

get_users_data = function() {
  users = read.csv(paste0(myurl, 'users.dat?raw=true'),
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
  
  return(distinct_genres)
}


