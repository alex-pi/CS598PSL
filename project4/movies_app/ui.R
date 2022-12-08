## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(    sidebarMenu(
            menuItem("Recommend by Genre", tabName = "genre", icon = icon("film")),
            menuItem("Recommend by Rating", tabName = "rating", icon = icon("star"))
          )),

          dashboardBody(includeCSS("css/movies.css"),
            tabItems(
              tabItem(tabName = "genre",
                      fluidRow(
                        box(width = 12, title = "Step 1: Select a movie genre", 
                            status = "info", solidHeader = TRUE, collapsible = FALSE,
                            div(class = "genreslist",
                                uiOutput('genres')
                            )
                        )
                      ),
                      fluidRow(
                        useShinyjs(),
                        box(
                          width = 12, status = "info", solidHeader = TRUE,
                          title = "Step 2: Discover movies you might like",
                          br(),
                          withBusyIndicatorUI(
                            actionButton("btng", "Click here to get your recommendations", class = "btn-warning")
                          ),
                          br(),
                          tableOutput("resultsg")
                          #textOutput("resultsg")
                        )
                      )
              ),              
              tabItem(tabName = "rating",
                fluidRow(
                    box(width = 12, title = "Step 1: Rate as many movies as possible", 
                        status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('ratings')
                        )
                    )
                  ),
                fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover movies you might like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                      ),
                      br(),
                      tableOutput("results")
                    )
                 )
               )
            )
          )
    )
) 