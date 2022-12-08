library(ShinyRatingInput)
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)
library(hash)
library(tidyverse)
library(Matrix)
library(proxy)
source('data_helpers.R')
#source('naive1_recom.R')
source('ibcf_recom.R')

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat$UserID = 0
  dat = dat[Rating > 0]
}

render_results = function(recom_result, num_rows = 2, num_movies_row = 5) {
  ### TODO, fix case < 10
  #print(recom_result)
  num_recom = ifelse(is.null(recom_result), 0, dim(recom_result)[1])
  num_movies = dim(movies)[1]
  sprintf("\n# Recomendations: %d", dim(recom_result)[1]) %>% cat()
  
  # Complete recoms with top movie picks
  if(num_recom < (num_rows * num_movies_row)) {
    num_missing = (num_rows * num_movies_row) - num_recom
    sprintf("\n# Extra recoms: %d", num_missing) %>% cat()
    idx_miss = sample(1:num_movies, num_missing)
    recom_result = bind_rows(recom_result, movies[idx_miss, ])
    #print(recom_result)
  }
  
  lapply(1:num_rows, function(i) {
    list(fluidRow(lapply(1:num_movies_row, function(j) {
      idx = (i - 1) * num_movies_row + j
      movie = movies[movies$MovieID == recom_result[idx, ]$MovieID,]
      #print(movie)
      box(width = 2, status = "success", solidHeader = TRUE, 
          title = paste0("Rank ", idx),
          
          div(style = "text-align:center", 
              a(img(src = movie$image_url, 
                    height = 150))
          ),
          div(style="text-align:center; font-size: 100%", 
              strong(movie$Title)
          )
          
      )        
    }))) # columns
  }) # rows  
}

shinyServer(function(input, output, session) {

  # show the list of genres
  output$genres <- renderUI({
    genres = get_genres(movies)
    selectInput("genre_sel", "Choose a genre:", genres)    
  })
  
  df2 <- eventReactive(input$btng, {
    withBusyIndicatorServer("btng", {
      
      preds = top.genre[top.genre$Genres == input$genre_sel,]
      #preds[1,]$Title
    })
  })
  
  output$resultsg <- renderUI({
    recom_result <- df2()
    #print(recom_result$Title)
    
    render_results(recom_result)
  }) 
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    s.movies = movies
    # Start with a random set of movies
    #s.movies = sample(1:dim(movies)[1], num_rows*num_movies)
    #s.movies = movies[s.movies, ]
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        idx = (i - 1) * num_movies + j
        list(box(width = 2,
                 div(style = "text-align:center", 
                     img(src = s.movies$image_url[idx], 
                         height = 150)),
                 div(style = "text-align:center", 
                     strong(s.movies$Title[idx])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", 
                     ratingInput(paste0("select_", 
                                        s.movies$MovieID[idx]), 
                                 label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      print(user_ratings)
      
      #preds = naive1_recom(user_ratings, movies)
      preds = ibcf_recom(user_ratings, movies, ratings)
      recom_results <- data.table(Rank = preds$Rank, 
                                  MovieID = preds$MovieID, 
                                  Title = preds$Title, 
                                  Predicted_rating = preds$Rating
                                  )
      
    }) # still busy
    
  }) # clicked on button
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    render_results(recom_result)
  }) # renderUI function
  
}) # server function