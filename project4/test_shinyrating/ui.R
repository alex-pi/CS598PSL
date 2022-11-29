#in ui.R
library(ShinyRatingInput)

shinyUI(bootstrapPage(
  ratingInput("movieRating", label="Rate this movie...", dataStop=10, dataFractions=2),
  htmlOutput("movieRating")
))