# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  ntext <- eventReactive(input$searchButton, {
    input$searchText
    #fb_page <- getPage(page=input$searchText, token=fb_oauth)
  })
  
  output$nText <- renderText({
    ntext()
  })
  
})


