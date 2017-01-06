shiny::shinyServer(function(input, output) {
  ntext <- shiny::eventReactive(input$searchButton, {
    input$searchText
    fb_page <- Rfacebook::getPage(page=input$searchText, token=fb_oauth)
    #fb_page <- Rfacebook::getNewsfeed(token = fb_oauth, n=10)
  })
  
  output$nText <- shiny::renderText({
    ntext()
  })
  
  output$sample <- shiny::renderText({
    fb_page <- Rfacebook::getPage(page="indaysara", token=fb_oauth)
  })
  
})