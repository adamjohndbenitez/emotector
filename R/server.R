shiny::shinyServer(function(input, output) {
  
  base::load(file = "fb_oauth")
  
  ntext <- shiny::eventReactive(input$searchButton, {
    fb_page <- Rfacebook::getPage(page = input$searchText, token = fb_oauth)
    post <- Rfacebook::getPost(post=fb_page$id[1], n=2000, token=fb_oauth)
    print(fb_page$message[10])
  })
  
  output$nText <- shiny::renderPrint({
    ntext()
  })
  
})