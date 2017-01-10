shiny::shinyServer(function(input, output, session) {
  base::load(file = "fb_oauth")

  shiny::updateDateRangeInput(session = session, inputId = "dateRangeId", start = base::Sys.Date() - 10, end = base::Sys.Date())

  shiny::observeEvent(input$searchButton, {
    base::tryCatch(expr = {
      listOfPosts <- Rfacebook::getPage(page = input$searchText, token = fb_oauth, n = base::as.numeric(input$numberOfPosts), since = input$dateRangeId[1], until = input$dateRangeId[2], verbose = FALSE)

      output$postListUIId <- shiny::renderUI({
        shiny::selectInput(inputId = "postListId", label = "Select Post", choices = 1:base::length(listOfPosts$message))
      })
  
      output$viewPostId <- shiny::renderText({
        listOfPosts$message[as.numeric(input$postListId)]
      })
      
      shiny::showNotification(ui = "We're good to go...", duration = 5, closeButton = FALSE, type = "message", session = shiny::getDefaultReactiveDomain())
    }, error = function(e) {
      shiny::showNotification(ui = "The Facebook page or group ID you’re using is not correct or invalid. Click link below", action = 
        shiny::tags$a(href = "https://smashballoon.com/custom-facebook-feed/id/", "Ensure valid facebook page ID."), duration = NULL, closeButton = TRUE, type = "error", session = shiny::getDefaultReactiveDomain())
    }, warning = function(w) {
      shiny::showNotification(ui = "Waring message.", duration = 5, closeButton = FALSE, type = "warning", session = shiny::getDefaultReactiveDomain())
    }, finally = {
    })

    output$viewPostUIId <- shiny::renderUI({
      shinydashboard::box(title = "Post", width = 12, solidHeader = TRUE, status = "primary",
        shiny::textOutput(outputId = "viewPostId")
      )
    })
  })

  shiny::observeEvent(input$submitManualPostId, {
    output$viewPostUIId <- shiny::renderUI({
      shinydashboard::box(title = "Post", width = 12, solidHeader = TRUE, status = "primary",
        shiny::textOutput(outputId = "viewPostId")
      )
    })

    output$viewPostId <- shiny::renderText({
      input$manualPostTextAreaId
    })
  })
})