library(shiny)
library(DT)

shinyUI(
     fluidPage(
          dateInput("fulldate", "Game Date", value = Sys.Date() - 1),
          uiOutput("gamedrop"), 
          plotOutput("plot", width = 700, height = 800)
     )
)