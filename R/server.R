shiny::shinyServer(function(input, output, session) {
  base::load(file = "fb_oauth")
  
  shiny::updateDateRangeInput(session = session, inputId = "dateRangeId", start = Sys.Date() - 10, end = Sys.Date() + 10)
  
  shiny::observeEvent(input$searchButton, {
    listOfPosts <- Rfacebook::getPage(page = input$searchText, token = fb_oauth, n = as.numeric(input$numberOfPosts), since = input$dateRangeId[1], until = input$dateRangeId[2], verbose = FALSE)

    output$postListUIId <- shiny::renderUI({
      shiny::selectInput(inputId = "postListId", label = "Select Post", choices = 1:length(listOfPosts$message))
    })
    
    output$viewPostId <- shiny::renderText({
      listOfPosts$message[as.numeric(input$postListId)]
    })
    
    output$viewPostUIId <- shiny::renderUI({
      shinydashboard::box(title = "Post", width = 12, solidHeader = TRUE, status = "primary",
          shiny::textOutput(outputId = "viewPostId")
      )
    })
  })
})