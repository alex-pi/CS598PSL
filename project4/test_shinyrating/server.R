

#the corresponding server.R
shinyServer(function(input, output, session) {
  output$movieRating <- renderText({
    paste("The movie was rated ",input$movieRating)
  })
})