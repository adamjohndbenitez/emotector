shiny::shinyServer(function(input, output) {
  
  ntext <- shiny::eventReactive(input$searchButton, {
    fb_page <- Rfacebook::getPage(page = input$searchText, token = fb_oauth)
  })
  
  output$nText <- shiny::renderPrint({
    ntext()
  })
  
})