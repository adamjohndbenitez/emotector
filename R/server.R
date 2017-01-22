shiny::shinyServer(function(input, output, session) {
  # base::load(file = "fb_oauth")

  shiny::updateDateRangeInput(session = session, inputId = "dateRangeId", start = base::Sys.Date() - 10, end = base::Sys.Date())

  shiny::observeEvent(input$searchButton, {
    # fb_oauth <- "EAACEdEose0cBADQoZB0CnWWRnYlG1MdXGEZAOXeWc6yKW494e6GypZCWYHwxp1ycjey5gS45xTinaHBzZBXNxY9YGl7amnV5Qnp7h8h4rD7kY3z12Hcz2s1IBgTiPeKyo2AXh1hQHz1kZBxTBROws6eydmfMuTiNQ2SaDMgSZCSFB1k7qVb39ZB8Dam45bCh48ZD"
    base::load(file = "fb_oauth") 
    
    base::tryCatch(expr = {
      listOfPosts <- Rfacebook::getPage(page = input$searchText, token = fb_oauth, n = base::as.numeric(input$numberOfPosts), since = input$dateRangeId[1], until = input$dateRangeId[2])

      output$postListUIId <- shiny::renderUI({
        shiny::selectInput(inputId = "postListId", label = "Select Post", choices = 1:base::length(listOfPosts$message))
      })
  
      output$viewPostId <- shiny::renderText({
        listOfPosts$message[as.numeric(input$postListId)]
      })
      
      shiny::showNotification(ui = "We're good...", duration = 5, closeButton = FALSE, type = "message", session = shiny::getDefaultReactiveDomain())
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

    tokenize <- tokenizers::tokenize_words(x = input$manualPostTextAreaId, lowercase = TRUE, stopwords = NULL, simplify = FALSE)

    joyData <- openxlsx::readWorkbook(xlsxFile = "emotions-final.xlsx", sheet = "Joy", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    countJoyNeutral <- 0
    
    countJoyNeutral <- 0
    countJoyHigh <- 0
    
    countJoyHigher <- 0
    
    countJoyHighest <- 0
    
    if (length(tokenize[[1]]) == 0) {
      shiny::showNotification(ui = "No post to analyze. Please fill Post Box above.", action = NULL, duration = 5, closeButton = TRUE, type = "error", session = shiny::getDefaultReactiveDomain())
    } else {
      for (i in 1:length(tokenize[[1]])) {#looping of words
        for (j in 1:nrow(joyData)) {
          if (is.na(joyData[j, 6])) { #trapping for NA values in excel
          } else if (tokenize[[1]][i] == joyData[j, 6]) { #look for intensifier
            for (k in 1:nrow(joyData)) {
              if (is.na(joyData[k, 3])) {
              } else if (tokenize[[1]][i+1] == joyData[k, 3]) {
                countJoyHighest <- countJoyHighest + 1
                countJoyNeutral <- countJoyNeutral - 1
                break()
              } # look up words in neutral column then add intensifier
    
              if (is.na(joyData[k, 4])) {
              } else if (tokenize[[1]][i+1] == joyData[k, 4]) {
                countJoyHighest <- countJoyHighest + 1
                countJoyHigh <- countJoyHigh - 1
                break()
              }
    
              if (is.na(joyData[k, 5])) {
              } else if (tokenize[[1]][i+1] == joyData[k, 5]) {
                countJoyHighest <- countJoyHighest + 1
                countJoyHigher <- countJoyHigher - 1
                break()
              }
            }
            break()
          }
          
          if (is.na(joyData[j, 3])) {
          } else if (tokenize[[1]][i] == joyData[j, 3]) {
            countJoyNeutral <- countJoyNeutral + 1
            break()
          }
          
          if (is.na(joyData[j, 4])) {
          } else if (tokenize[[1]][i] == joyData[j, 4]) {
            countJoyHigh <- countJoyHigh + 1
            break()
          }
          
          if (is.na(joyData[j, 5])) {
          } else if (tokenize[[1]][i] == joyData[j, 5]) {
            countJoyHigher <- countJoyHigher + 1
            break()
          }
        }
      }  
    }

    
    
    print(countJoyNeutral)
    print(countJoyHigh)
    print(countJoyHigher)
    print(countJoyHighest)
    
    output$joyBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = 
        base::sum(countJoyNeutral, countJoyHigh, countJoyHigher, countJoyHighest), subtitle = "Total joy words", icon = 
          shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1)
    })
    output$joyHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest Joy", value = countJoyHighest, subtitle = "Total highest joy words", icon = 
          shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1)
    })
    output$joyHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher Joy", value = countJoyHigher, subtitle = "Total higher joy words", icon = 
          shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1)
    })
    output$joyHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High Joy", value = countJoyHigh, subtitle = "Total high joy words", icon = 
          shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1)
    })
    output$joyNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral Joy", value = countJoyNeutral, subtitle = "Total neutral joy words", icon = 
          shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1, fill = TRUE)
    })
    output$joyLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low Joy", value = 0, subtitle = "Total low joy words", icon = 
          shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1, fill = TRUE)
    })
    output$joyLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest Joy", value = 0, subtitle = "Total lowest joy words", icon = 
        shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1, fill = TRUE)
    })
  })
  
  
})