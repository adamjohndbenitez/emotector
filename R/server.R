require(magrittr)

shiny::shinyServer(function(input, output, session) {
  shiny::updateDateRangeInput(session = session, inputId = "dateRangeId", start = base::Sys.Date() - 10, end = base::Sys.Date())
  
  output$dygraph <- dygraphs::renderDygraph({
    dygraphs::dygraph(nhtemp, main = "New Haven Temperatures", ylab = "Temp (F)") %>%
      dygraphs::dyOptions(drawGrid = input$showgrid)
  })
  
  output$plot <- renderPlot({
    input$newplot
    nr.prof <-
      c(prof.pilots = 16, lawyers = 11, farmers = 10, salesmen = 9, physicians = 9,
        mechanics = 6, policemen = 6, managers = 6, engineers = 5, teachers = 4,
        housewives = 3, students = 3, armed.forces = 1)
    graphics::barplot(nr.prof)
  })
    
  shiny::observeEvent(input$searchButton, {
    # fb_oauth <- "EAACEdEose0cBADQoZB0CnWWRnYlG1MdXGEZAOXeWc6yKW494e6GypZCWYHwxp1ycjey5gS45xTinaHBzZBXNxY9YGl7amnV5Qnp7h8h4rD7kY3z12Hcz2s1IBgTiPeKyo2AXh1hQHz1kZBxTBROws6eydmfMuTiNQ2SaDMgSZCSFB1k7qVb39ZB8Dam45bCh48ZD"
    base::load(file = "fb_oauth") 
    
    base::tryCatch(expr = {
      listOfPosts <- Rfacebook::getPage(page = input$searchText, token = fb_oauth, n = base::as.numeric(input$numberOfPosts), since = input$dateRangeId[1], until = input$dateRangeId[2], feed = FALSE, reactions = TRUE, verbose = TRUE)
      progress <- shiny::Progress$new(session, min=1, max=15)
      on.exit(progress$close())

      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...')

      for (i in 1:15) {
        progress$set(value = i)
        Sys.sleep(0.25)
      }
      output$postListUIId <- shiny::renderUI({
        shiny::selectInput(inputId = "postListId", label = "Select Post #", choices = 1:base::length(listOfPosts$message))
      })
      
      output$viewPostId <- shiny::renderText({
        listofComments <- Rfacebook::getPost(post = listOfPosts$id[as.numeric(input$postListId)], token = fb_oauth, n = base::as.numeric(input$numberOfComments), comments = TRUE, likes = TRUE)
        
          output$viewCommentsId <- shiny::renderTable(expr = {
            if(length(listofComments$comments$message) != 0) {
              listofComments$comments$message
            }
          }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs", width = "auto", rownames = TRUE, colnames = TRUE, na = NA)
          
        c("#", input$postListId, ":", listOfPosts$message[as.numeric(input$postListId)])
      })
      
      # print(listofComments)
      
      output$viewPostUIId <- shiny::renderUI({
        shinydashboard::box(title = "Post", width = 12, solidHeader = TRUE, status = "primary", background = NULL, footer = 
          shiny::downloadLink(outputId = "downloadCsvFileId", label = shiny::tagList(
            shiny::icon(name = "download", class = "fa-1x", lib = "font-awesome"), "Download Posts")
          ),
          shiny::textOutput(outputId = "viewPostId")
        )
      })
      
      output$viewCommentsUIId <- shiny::renderUI({
        shinydashboard::box(title = "Comments", width = 12, solidHeader = TRUE, status = "primary", 
          shiny::tableOutput(outputId = "viewCommentsId")
        )
      })
      
      output$downloadCsvFileId <- shiny::downloadHandler(
        filename = function() { paste("Facebook_Posts_", Sys.Date(), ".csv", sep = "") },
        content = function(file) {
          write.csv(x = listOfPosts, file = file)
        }
      )

      shiny::showNotification(ui = "We're good...", duration = 5, closeButton = FALSE, type = "message", session = shiny::getDefaultReactiveDomain())
     }, error = function(e) {
       shiny::showNotification(ui = "The Facebook page or group ID you’re using is not correct or invalid. Click link below", action = 
        shiny::tagList(
          shiny::tags$a(href = "https://smashballoon.com/custom-facebook-feed/id/", "Ensure valid facebook page ID."),
          shiny::tags$a(href = "http://findmyfbid.com/", "Find your Facebook page ID.")
        ), duration = 10, closeButton = TRUE, type = "error", session = shiny::getDefaultReactiveDomain())
     }, warning = function(w) {
       shiny::showNotification(ui = "Waring message.", duration = 5, closeButton = FALSE, type = "warning", session = shiny::getDefaultReactiveDomain())
     }, finally = {
       
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

    joyData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Joy", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    sadnessData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Sadness", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    angerData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Anger", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    disgustData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Disgust", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    fearData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Fear", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    contrastingConjunctions <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Constrasting Conjunctions", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    
    tokenizeSentences <- tokenizers::tokenize_sentences(x = input$manualPostTextAreaId, lowercase = FALSE, strip_punctuation = FALSE, simplify = FALSE)
    
    #----------START FINAL-TALLY-OF-JOY----------
    finalCountJoyLowest <- 0
    finalCountJoyLow <- 0
    finalCountJoyNeutral <- 0
    finalCountJoyHigh <- 0
    finalCountJoyHigher <- 0
    finalCountJoyHighest <- 0
    finalWeightJoyLowest <- 0
    finalWeightJoyLow <- 0
    finalWeightJoyNeutral <- 0
    finalWeightJoyHigh <- 0
    finalWeightJoyHigher <- 0
    finalWeightJoyHighest <- 0
    #----------END FINAL-TALLY-OF-JOY----------
    #----------START FINAL-TALLY-OF-SADNESS----------
    finalCountSadnessLowest <- 0
    finalCountSadnessLow <- 0
    finalCountSadnessNeutral <- 0
    finalCountSadnessHigh <- 0
    finalCountSadnessHigher <- 0
    finalCountSadnessHighest <- 0
    finalWeightSadnessLowest <- 0
    finalWeightSadnessLow <- 0
    finalWeightSadnessNeutral <- 0
    finalWeightSadnessHigh <- 0
    finalWeightSadnessHigher <- 0
    finalWeightSadnessHighest <- 0
    #----------END FINAL-TALLY-OF-SADNESS----------
    #----------START FINAL-TALLY-OF-ANGER----------
    finalCountAngerLowest <- 0
    finalCountAngerLow <- 0
    finalCountAngerNeutral <- 0
    finalCountAngerHigh <- 0
    finalCountAngerHigher <- 0
    finalCountAngerHighest <- 0
    finalWeightAngerLowest <- 0
    finalWeightAngerLow <- 0
    finalWeightAngerNeutral <- 0
    finalWeightAngerHigh <- 0
    finalWeightAngerHigher <- 0
    finalWeightAngerHighest <- 0
    #----------END FINAL-TALLY-OF-ANGER----------
    #----------START FINAL-TALLY-OF-DISGUST----------
    finalCountDisgustLowest <- 0
    finalCountDisgustLow <- 0
    finalCountDisgustNeutral <- 0
    finalCountDisgustHigh <- 0
    finalCountDisgustHigher <- 0
    finalCountDisgustHighest <- 0
    finalWeightDisgustLowest <- 0
    finalWeightDisgustLow <- 0
    finalWeightDisgustNeutral <- 0
    finalWeightDisgustHigh <- 0
    finalWeightDisgustHigher <- 0
    finalWeightDisgustHighest <- 0
    #----------END FINAL-TALLY-OF-DISGUST----------
    #----------START FINAL-TALLY-OF-FEAR----------
    finalCountFearLowest <- 0
    finalCountFearLow <- 0
    finalCountFearNeutral <- 0
    finalCountFearHigh <- 0
    finalCountFearHigher <- 0
    finalCountFearHighest <- 0
    finalWeightFearLowest <- 0
    finalWeightFearLow <- 0
    finalWeightFearNeutral <- 0
    finalWeightFearHigh <- 0
    finalWeightFearHigher <- 0
    finalWeightFearHighest <- 0
    #----------END FINAL-TALLY-OF-FEAR----------
    sumWeightsJoy <- 0
    sumCountJoy <- 0
    sumWeightsSadness <- 0
    sumCountSadness <- 0
    sumWeightsAnger <- 0
    sumCountAnger <- 0
    sumWeightsDisgust <- 0
    sumCountDisgust <- 0
    sumWeightsFear <- 0
    sumCountFear <- 0
    
    if (length(tokenizeSentences[[1]]) == 0) {
      shiny::showNotification(ui = "No post to analyze. Please fill Post Box above.", action = NULL, duration = 5, closeButton = TRUE, type = "error", session = shiny::getDefaultReactiveDomain())
    } else {
      for (i in 1:length(tokenizeSentences[[1]])) {
        tokenizeWords <- tokenizers::tokenize_words(x = tokenizeSentences[[1]][i], lowercase = TRUE, stopwords = NULL, simplify = FALSE)
        #----------START TEMP-TALLY-OF-JOY----------
        tempCountJoyLowest <- 0
        tempCountJoyLow <- 0
        tempCountJoyNeutral <- 0
        tempCountJoyHigh <- 0
        tempCountJoyHigher <- 0
        tempCountJoyHighest <- 0
        tempWeightJoyLowest <- 0
        tempWeightJoyLow <- 0
        tempWeightJoyNeutral <- 0
        tempWeightJoyHigh <- 0
        tempWeightJoyHigher <- 0
        tempWeightJoyHighest <- 0
        #----------START TEMP-TALLY-OF-JOY----------
        #----------START TEMP-TALLY-OF-SADNESS----------
        tempCountSadnessLowest <- 0
        tempCountSadnessLow <- 0
        tempCountSadnessNeutral <- 0
        tempCountSadnessHigh <- 0
        tempCountSadnessHigher <- 0
        tempCountSadnessHighest <- 0
        tempWeightSadnessLowest <- 0
        tempWeightSadnessLow <- 0
        tempWeightSadnessNeutral <- 0
        tempWeightSadnessHigh <- 0
        tempWeightSadnessHigher <- 0
        tempWeightSadnessHighest <- 0
        #----------START TEMP-TALLY-OF-SADNESS----------
        #----------START TEMP-TALLY-OF-ANGER----------
        tempCountAngerLowest <- 0
        tempCountAngerLow <- 0
        tempCountAngerNeutral <- 0
        tempCountAngerHigh <- 0
        tempCountAngerHigher <- 0
        tempCountAngerHighest <- 0
        tempWeightAngerLowest <- 0
        tempWeightAngerLow <- 0
        tempWeightAngerNeutral <- 0
        tempWeightAngerHigh <- 0
        tempWeightAngerHigher <- 0
        tempWeightAngerHighest <- 0
        #----------START TEMP-TALLY-OF-ANGER----------
        #----------START TEMP-TALLY-OF-DISGUST----------
        tempCountDisgustLowest <- 0
        tempCountDisgustLow <- 0
        tempCountDisgustNeutral <- 0
        tempCountDisgustHigh <- 0
        tempCountDisgustHigher <- 0
        tempCountDisgustHighest <- 0
        tempWeightDisgustLowest <- 0
        tempWeightDisgustLow <- 0
        tempWeightDisgustNeutral <- 0
        tempWeightDisgustHigh <- 0
        tempWeightDisgustHigher <- 0
        tempWeightDisgustHighest <- 0
        #----------START TEMP-TALLY-OF-DISGUST----------
        #----------START TEMP-TALLY-OF-FEAR----------
        tempCountFearLowest <- 0
        tempCountFearLow <- 0
        tempCountFearNeutral <- 0
        tempCountFearHigh <- 0
        tempCountFearHigher <- 0
        tempCountFearHighest <- 0
        tempWeightFearLowest <- 0
        tempWeightFearLow <- 0
        tempWeightFearNeutral <- 0
        tempWeightFearHigh <- 0
        tempWeightFearHigher <- 0
        tempWeightFearHighest <- 0
        #----------START TEMP-TALLY-OF-FEAR----------
        if (length(tokenizeWords[[1]]) == 0) {
          shiny::showNotification(ui = "No post to analyze. Please fill Post Box above.", action = NULL, duration = 5, closeButton = TRUE, type = "error", session = shiny::getDefaultReactiveDomain())
        } else {
          for (j in 1:length(tokenizeWords[[1]])) { #looping of words
            for (k in 1:nrow(contrastingConjunctions)) { #check if there are contrasting conjunctions.
              if (tokenizeWords[[1]][j] == contrastingConjunctions[k, 1]) {
                #----------START TEMP-TALLY-OF-JOY----------
                tempCountJoyLowest <- 0
                tempCountJoyLow <- 0
                tempCountJoyNeutral <- 0
                tempCountJoyHigh <- 0
                tempCountJoyHigher <- 0
                tempCountJoyHighest <- 0
                tempWeightJoyLowest <- 0
                tempWeightJoyLow <- 0
                tempWeightJoyNeutral <- 0
                tempWeightJoyHigh <- 0
                tempWeightJoyHigher <- 0
                tempWeightJoyHighest <- 0
                #----------START TEMP-TALLY-OF-JOY----------
                #----------START TEMP-TALLY-OF-SADNESS----------
                tempCountSadnessLowest <- 0
                tempCountSadnessLow <- 0
                tempCountSadnessNeutral <- 0
                tempCountSadnessHigh <- 0
                tempCountSadnessHigher <- 0
                tempCountSadnessHighest <- 0
                tempWeightSadnessLowest <- 0
                tempWeightSadnessLow <- 0
                tempWeightSadnessNeutral <- 0
                tempWeightSadnessHigh <- 0
                tempWeightSadnessHigher <- 0
                tempWeightSadnessHighest <- 0
                #----------START TEMP-TALLY-OF-SADNESS----------
                #----------START TEMP-TALLY-OF-ANGER----------
                tempCountAngerLowest <- 0
                tempCountAngerLow <- 0
                tempCountAngerNeutral <- 0
                tempCountAngerHigh <- 0
                tempCountAngerHigher <- 0
                tempCountAngerHighest <- 0
                tempWeightAngerLowest <- 0
                tempWeightAngerLow <- 0
                tempWeightAngerNeutral <- 0
                tempWeightAngerHigh <- 0
                tempWeightAngerHigher <- 0
                tempWeightAngerHighest <- 0
                #----------START TEMP-TALLY-OF-ANGER----------
                #----------START TEMP-TALLY-OF-DISGUST----------
                tempCountDisgustLowest <- 0
                tempCountDisgustLow <- 0
                tempCountDisgustNeutral <- 0
                tempCountDisgustHigh <- 0
                tempCountDisgustHigher <- 0
                tempCountDisgustHighest <- 0
                tempWeightDisgustLowest <- 0
                tempWeightDisgustLow <- 0
                tempWeightDisgustNeutral <- 0
                tempWeightDisgustHigh <- 0
                tempWeightDisgustHigher <- 0
                tempWeightDisgustHighest <- 0
                #----------START TEMP-TALLY-OF-DISGUST----------
                #----------START TEMP-TALLY-OF-FEAR----------
                tempCountDisgustLowest <- 0
                tempCountDisgustLow <- 0
                tempCountDisgustNeutral <- 0
                tempCountDisgustHigh <- 0
                tempCountDisgustHigher <- 0
                tempCountDisgustHighest <- 0
                tempWeightDisgustLowest <- 0
                tempWeightDisgustLow <- 0
                tempWeightDisgustNeutral <- 0
                tempWeightDisgustHigh <- 0
                tempWeightDisgustHigher <- 0
                tempWeightDisgustHighest <- 0
                #----------START TEMP-TALLY-OF-FEAR----------
              }
            }
            
            #----------START JOY-FUZZY-SETS----------
            for (l in 1:nrow(joyData)) {
              if (is.na(joyData[l, 1])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == joyData[l, 1]) { #look for negative contractions and connotations.
                for (n in 1:nrow(joyData)) {
                  if (is.na(joyData[n, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[n, 3]) {
                    tempCountJoyLowest <- tempCountJoyLowest + 1
                    tempCountJoyNeutral <- tempCountJoyNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(joyData[n, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[n, 4]) {
                    tempCountJoyLowest <- tempCountJoyLowest + 1
                    tempCountJoyHigh <- tempCountJoyHigh - 1
                    break()
                  }
        
                  if (is.na(joyData[n, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[n, 5]) {
                    tempCountJoyLowest <- tempCountJoyLowest + 1
                    tempCountJoyHigher <- tempCountJoyHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(joyData[l, 2])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == joyData[l, 2]) { #look for medium words.
                for (m in 1:nrow(joyData)) {
                  if (is.na(joyData[m, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[m, 3]) {
                    tempCountJoyLow <- tempCountJoyLow + 1
                    tempCountJoyNeutral <- tempCountJoyNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(joyData[m, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[m, 4]) {
                    tempCountJoyLow <- tempCountJoyLow + 1
                    tempCountJoyHigh <- tempCountJoyHigh - 1
                    break()
                  }
        
                  if (is.na(joyData[m, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[m, 5]) {
                    tempCountJoyLow <- tempCountJoyLow + 1
                    tempCountJoyHigher <- tempCountJoyHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(joyData[l, 6])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == joyData[l, 6]) { #look for intensifier
                for (o in 1:nrow(joyData)) {
                  if (is.na(joyData[o, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[o, 3]) {
                    tempCountJoyHighest <- tempCountJoyHighest + 1
                    tempCountJoyNeutral <- tempCountJoyNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(joyData[o, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[o, 4]) {
                    tempCountJoyHighest <- tempCountJoyHighest + 1
                    tempCountJoyHigh <- tempCountJoyHigh - 1
                    break()
                  }
        
                  if (is.na(joyData[o, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[o, 5]) {
                    tempCountJoyHighest <- tempCountJoyHighest + 1
                    tempCountJoyHigher <- tempCountJoyHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(joyData[l, 3])) {
              } else if (tokenizeWords[[1]][j] == joyData[l, 3]) {
                tempCountJoyNeutral <- tempCountJoyNeutral + 1
                break()
              }
              
              if (is.na(joyData[l, 4])) {
              } else if (tokenizeWords[[1]][j] == joyData[l, 4]) {
                tempCountJoyHigh <- tempCountJoyHigh + 1
                break()
              }
              
              if (is.na(joyData[l, 5])) {
              } else if (tokenizeWords[[1]][j] == joyData[l, 5]) {
                tempCountJoyHigher <- tempCountJoyHigher + 1
                break()
              }
            }
            #----------END JOY-FUZZY-SETS----------
            #----------START SADNESS-FUZZY-SETS----------
            for (l in 1:nrow(sadnessData)) {
              if (is.na(sadnessData[l, 1])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == sadnessData[l, 1]) { #look for negative contractions and connotations.
                for (n in 1:nrow(sadnessData)) {
                  if (is.na(sadnessData[n, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == sadnessData[n, 3]) {
                    tempCountSadnessLowest <- tempCountSadnessLowest + 1
                    tempCountSadnessNeutral <- tempCountSadnessNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(sadnessData[n, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == sadnessData[n, 4]) {
                    tempCountSadnessLowest <- tempCountSadnessLowest + 1
                    tempCountSadnessHigh <- tempCountSadnessHigh - 1
                    break()
                  }
        
                  if (is.na(sadnessData[n, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == sadnessData[n, 5]) {
                    tempCountSadnessLowest <- tempCountSadnessLowest + 1
                    tempCountSadnessHigher <- tempCountSadnessHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(sadnessData[l, 2])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == sadnessData[l, 2]) { #look for medium words.
                for (m in 1:nrow(sadnessData)) {
                  if (is.na(sadnessData[m, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == sadnessData[m, 3]) {
                    tempCountSadnessLow <- tempCountSadnessLow + 1
                    tempCountSadnessNeutral <- tempCountSadnessNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(sadnessData[m, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == sadnessData[m, 4]) {
                    tempCountSadnessLow <- tempCountSadnessLow + 1
                    tempCountSadnessHigh <- tempCountSadnessHigh - 1
                    break()
                  }
        
                  if (is.na(sadnessData[m, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == sadnessData[m, 5]) {
                    tempCountSadnessLow <- tempCountSadnessLow + 1
                    tempCountSadnessHigher <- tempCountSadnessHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(sadnessData[l, 6])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == sadnessData[l, 6]) { #look for intensifier
                for (o in 1:nrow(sadnessData)) {
                  if (is.na(sadnessData[o, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == sadnessData[o, 3]) {
                    tempCountSadnessHighest <- tempCountSadnessHighest + 1
                    tempCountSadnessNeutral <- tempCountSadnessNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(sadnessData[o, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == sadnessData[o, 4]) {
                    tempCountSadnessHighest <- tempCountSadnessHighest + 1
                    tempCountSadnessHigh <- tempCountSadnessHigh - 1
                    break()
                  }
        
                  if (is.na(sadnessData[o, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == sadnessData[o, 5]) {
                    tempCountSadnessHighest <- tempCountSadnessHighest + 1
                    tempCountSadnessHigher <- tempCountSadnessHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(sadnessData[l, 3])) {
              } else if (tokenizeWords[[1]][j] == sadnessData[l, 3]) {
                tempCountSadnessNeutral <- tempCountSadnessNeutral + 1
                break()
              }
              
              if (is.na(sadnessData[l, 4])) {
              } else if (tokenizeWords[[1]][j] == sadnessData[l, 4]) {
                tempCountSadnessHigh <- tempCountSadnessHigh + 1
                break()
              }
              
              if (is.na(sadnessData[l, 5])) {
              } else if (tokenizeWords[[1]][j] == sadnessData[l, 5]) {
                tempCountSadnessHigher <- tempCountSadnessHigher + 1
                break()
              }
            }
            #----------END SADNESS-FUZZY-SETS----------
            #----------START ANGER-FUZZY-SETS----------
            for (l in 1:nrow(angerData)) {
              if (is.na(angerData[l, 1])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == angerData[l, 1]) { #look for negative contractions and connotations.
                for (n in 1:nrow(angerData)) {
                  if (is.na(angerData[n, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == angerData[n, 3]) {
                    tempCountAngerLowest <- tempCountAngerLowest + 1
                    tempCountAngerNeutral <- tempCountAngerNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(angerData[n, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == angerData[n, 4]) {
                    tempCountAngerLowest <- tempCountAngerLowest + 1
                    tempCountAngerHigh <- tempCountAngerHigh - 1
                    break()
                  }
        
                  if (is.na(angerData[n, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == angerData[n, 5]) {
                    tempCountAngerLowest <- tempCountAngerLowest + 1
                    tempCountAngerHigher <- tempCountAngerHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(angerData[l, 2])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == angerData[l, 2]) { #look for medium words.
                for (m in 1:nrow(angerData)) {
                  if (is.na(angerData[m, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == angerData[m, 3]) {
                    tempCountAngerLow <- tempCountAngerLow + 1
                    tempCountAngerNeutral <- tempCountAngerNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(angerData[m, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == angerData[m, 4]) {
                    tempCountAngerLow <- tempCountAngerLow + 1
                    tempCountAngerHigh <- tempCountAngerHigh - 1
                    break()
                  }
        
                  if (is.na(angerData[m, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == angerData[m, 5]) {
                    tempCountAngerLow <- tempCountAngerLow + 1
                    tempCountAngerHigher <- tempCountAngerHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(angerData[l, 6])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == angerData[l, 6]) { #look for intensifier
                for (o in 1:nrow(angerData)) {
                  if (is.na(angerData[o, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == angerData[o, 3]) {
                    tempCountAngerHighest <- tempCountAngerHighest + 1
                    tempCountAngerNeutral <- tempCountAngerNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(angerData[o, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == angerData[o, 4]) {
                    tempCountAngerHighest <- tempCountAngerHighest + 1
                    tempCountAngerHigh <- tempCountAngerHigh - 1
                    break()
                  }
        
                  if (is.na(angerData[o, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == angerData[o, 5]) {
                    tempCountAngerHighest <- tempCountAngerHighest + 1
                    tempCountAngerHigher <- tempCountAngerHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(angerData[l, 3])) {
              } else if (tokenizeWords[[1]][j] == angerData[l, 3]) {
                tempCountAngerNeutral <- tempCountAngerNeutral + 1
                break()
              }
              
              if (is.na(angerData[l, 4])) {
              } else if (tokenizeWords[[1]][j] == angerData[l, 4]) {
                tempCountAngerHigh <- tempCountAngerHigh + 1
                break()
              }
              
              if (is.na(angerData[l, 5])) {
              } else if (tokenizeWords[[1]][j] == angerData[l, 5]) {
                tempCountAngerHigher <- tempCountAngerHigher + 1
                break()
              }
            }
            #----------END ANGER-FUZZY-SETS----------
            #----------START DISGUST-FUZZY-SETS----------
            for (l in 1:nrow(disgustData)) {
              if (is.na(disgustData[l, 1])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == disgustData[l, 1]) { #look for negative contractions and connotations.
                for (n in 1:nrow(disgustData)) {
                  if (is.na(disgustData[n, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == disgustData[n, 3]) {
                    tempCountDisgustLowest <- tempCountDisgustLowest + 1
                    tempCountDisgustNeutral <- tempCountDisgustNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(disgustData[n, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == disgustData[n, 4]) {
                    tempCountDisgustLowest <- tempCountDisgustLowest + 1
                    tempCountDisgustHigh <- tempCountDisgustHigh - 1
                    break()
                  }
        
                  if (is.na(disgustData[n, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == disgustData[n, 5]) {
                    tempCountDisgustLowest <- tempCountDisgustLowest + 1
                    tempCountDisgustHigher <- tempCountDisgustHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(disgustData[l, 2])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == disgustData[l, 2]) { #look for medium words.
                for (m in 1:nrow(disgustData)) {
                  if (is.na(disgustData[m, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == disgustData[m, 3]) {
                    tempCountDisgustLow <- tempCountDisgustLow + 1
                    tempCountDisgustNeutral <- tempCountDisgustNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(disgustData[m, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == disgustData[m, 4]) {
                    tempCountDisgustLow <- tempCountDisgustLow + 1
                    tempCountDisgustHigh <- tempCountDisgustHigh - 1
                    break()
                  }
        
                  if (is.na(disgustData[m, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == disgustData[m, 5]) {
                    tempCountDisgustLow <- tempCountDisgustLow + 1
                    tempCountDisgustHigher <- tempCountDisgustHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(disgustData[l, 6])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == disgustData[l, 6]) { #look for intensifier
                for (o in 1:nrow(disgustData)) {
                  if (is.na(disgustData[o, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == disgustData[o, 3]) {
                    tempCountDisgustHighest <- tempCountDisgustHighest + 1
                    tempCountDisgustNeutral <- tempCountDisgustNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(disgustData[o, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == disgustData[o, 4]) {
                    tempCountDisgustHighest <- tempCountDisgustHighest + 1
                    tempCountDisgustHigh <- tempCountDisgustHigh - 1
                    break()
                  }
        
                  if (is.na(disgustData[o, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == disgustData[o, 5]) {
                    tempCountDisgustHighest <- tempCountDisgustHighest + 1
                    tempCountDisgustHigher <- tempCountDisgustHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(disgustData[l, 3])) {
              } else if (tokenizeWords[[1]][j] == disgustData[l, 3]) {
                tempCountDisgustNeutral <- tempCountDisgustNeutral + 1
                break()
              }
              
              if (is.na(disgustData[l, 4])) {
              } else if (tokenizeWords[[1]][j] == disgustData[l, 4]) {
                tempCountDisgustHigh <- tempCountDisgustHigh + 1
                break()
              }
              
              if (is.na(disgustData[l, 5])) {
              } else if (tokenizeWords[[1]][j] == sadnessData[l, 5]) {
                tempCountDisgustHigher <- tempCountDisgustHigher + 1
                break()
              }
            }
            #----------END DISGUST-FUZZY-SETS----------
            #----------START FEAR-FUZZY-SETS----------
            for (l in 1:nrow(fearData)) {
              if (is.na(fearData[l, 1])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == fearData[l, 1]) { #look for negative contractions and connotations.
                for (n in 1:nrow(fearData)) {
                  if (is.na(fearData[n, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == fearData[n, 3]) {
                    tempCountFearLowest <- tempCountFearLowest + 1
                    tempCountFearNeutral <- tempCountFearNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(fearData[n, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == fearData[n, 4]) {
                    tempCountFearLowest <- tempCountFearLowest + 1
                    tempCountFearHigh <- tempCountFearHigh - 1
                    break()
                  }
        
                  if (is.na(fearData[n, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == fearData[n, 5]) {
                    tempCountFearLowest <- tempCountFearLowest + 1
                    tempCountFearHigher <- tempCountFearHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(fearData[l, 2])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == fearData[l, 2]) { #look for medium words.
                for (m in 1:nrow(fearData)) {
                  if (is.na(fearData[m, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == fearData[m, 3]) {
                    tempCountFearLow <- tempCountFearLow + 1
                    tempCountFearNeutral <- tempCountFearNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(fearData[m, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == fearData[m, 4]) {
                    tempCountFearLow <- tempCountFearLow + 1
                    tempCountFearHigh <- tempCountFearHigh - 1
                    break()
                  }
        
                  if (is.na(fearData[m, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == fearData[m, 5]) {
                    tempCountFearLow <- tempCountFearLow + 1
                    tempCountFearHigher <- tempCountFearHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(fearData[l, 6])) { #trapping for NA values in excel
              } else if (tokenizeWords[[1]][j] == fearData[l, 6]) { #look for intensifier
                for (o in 1:nrow(fearData)) {
                  if (is.na(fearData[o, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == fearData[o, 3]) {
                    tempCountFearHighest <- tempCountFearHighest + 1
                    tempCountFearNeutral <- tempCountFearNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(fearData[o, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == fearData[o, 4]) {
                    tempCountFearHighest <- tempCountFearHighest + 1
                    tempCountFearHigh <- tempCountFearHigh - 1
                    break()
                  }
        
                  if (is.na(fearData[o, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == fearData[o, 5]) {
                    tempCountFearHighest <- tempCountFearHighest + 1
                    tempCountFearHigher <- tempCountFearHigher - 1
                    break()
                  }
                }
                break()
              }
              
              if (is.na(fearData[l, 3])) {
              } else if (tokenizeWords[[1]][j] == fearData[l, 3]) {
                tempCountFearNeutral <- tempCountFearNeutral + 1
                break()
              }
              
              if (is.na(fearData[l, 4])) {
              } else if (tokenizeWords[[1]][j] == fearData[l, 4]) {
                tempCountFearHigh <- tempCountFearHigh + 1
                break()
              }
              
              if (is.na(fearData[l, 5])) {
              } else if (tokenizeWords[[1]][j] == fearData[l, 5]) {
                tempCountFearHigher <- tempCountFearHigher + 1
                break()
              }
            }
            #----------END FEAR-FUZZY-SETS----------
          }  
        }
        
        #----------START STORE TEMP-TALLY-OF-JOY TO FINAL-TALLY-OF-JOY----------
        finalCountJoyLowest <- finalCountJoyLowest + tempCountJoyLowest
        finalWeightJoyLowest <- finalCountJoyLowest * 0
        finalCountJoyLow <- finalCountJoyLow + tempCountJoyLow
        finalWeightJoyLow <- finalCountJoyLow * 0.2
        finalCountJoyNeutral <- finalCountJoyNeutral + tempCountJoyNeutral
        finalWeightJoyNeutral <- finalCountJoyNeutral * 0.4
        finalCountJoyHigh <- finalCountJoyHigh + tempCountJoyHigh
        finalWeightJoyHigh <- finalCountJoyHigh * 0.6
        finalCountJoyHigher <- finalCountJoyHigher + tempCountJoyHigher
        finalWeightJoyHigher <- finalCountJoyHigher * 0.8
        finalCountJoyHighest <- finalCountJoyHighest + tempCountJoyHighest
        finalWeightJoyHighest <- finalCountJoyHighest * 1
        #----------END STORE TEMP-TALLY-OF-JOY TO FINAL-TALLY-OF-JOY----------
        #----------START STORE TEMP-TALLY-OF-SADNESS TO FINAL-TALLY-OF-SADNESS----------
        finalCountSadnessLowest <- finalCountSadnessLowest + tempCountSadnessLowest
        finalWeightSadnessLowest <- finalCountSadnessLowest * 0
        finalCountSadnessLow <- finalCountSadnessLow + tempCountSadnessLow
        finalWeightSadnessLow <- finalCountSadnessLow * 0.2
        finalCountSadnessNeutral <- finalCountSadnessNeutral + tempCountSadnessNeutral
        finalWeightSadnessNeutral <- finalCountSadnessNeutral * 0.4
        finalCountSadnessHigh <- finalCountSadnessHigh + tempCountSadnessHigh
        finalWeightSadnessHigh <- finalCountSadnessHigh * 0.6
        finalCountSadnessHigher <- finalCountSadnessHigher + tempCountSadnessHigher
        finalWeightSadnessHigher <- finalCountSadnessHigher * 0.8
        finalCountSadnessHighest <- finalCountSadnessHighest + tempCountSadnessHighest
        finalWeightSadnessHighest <- finalCountSadnessHighest * 1
        #----------START STORE TEMP-TALLY-OF-SADNESS TO FINAL-TALLY-OF-SADNESS----------
        #----------START STORE TEMP-TALLY-OF-ANGER TO FINAL-TALLY-OF-ANGER----------
        finalCountAngerLowest <- finalCountAngerLowest + tempCountAngerLowest
        finalWeightAngerLowest <- finalCountAngerLowest * 0
        finalCountAngerLow <- finalCountAngerLow + tempCountAngerLow
        finalWeightAngerLow <- finalCountAngerLow * 0.2
        finalCountAngerNeutral <- finalCountAngerNeutral + tempCountAngerNeutral
        finalWeightAngerNeutral <- finalCountAngerNeutral * 0.4
        finalCountAngerHigh <- finalCountAngerHigh + tempCountAngerHigh
        finalWeightAngerHigh <- finalCountAngerHigh * 0.6
        finalCountAngerHigher <- finalCountAngerHigher + tempCountAngerHigher
        finalWeightAngerHigher <- finalCountAngerHigher * 0.8
        finalCountAngerHighest <- finalCountAngerHighest + tempCountAngerHighest
        finalWeightAngerHighest <- finalCountAngerHighest * 1
        #----------END STORE TEMP-TALLY-OF-ANGER TO FINAL-TALLY-OF-ANGER----------
        #----------START STORE TEMP-TALLY-OF-DISGUST TO FINAL-TALLY-OF-DISGUST----------
        finalCountDisgustLowest <- finalCountDisgustLowest + tempCountDisgustLowest
        finalWeightDisgustLowest <- finalCountDisgustLowest * 0
        finalCountDisgustLow <- finalCountDisgustLow + tempCountDisgustLow
        finalWeightDisgustLow <- finalCountDisgustLow * 0.2
        finalCountDisgustNeutral <- finalCountDisgustNeutral + tempCountDisgustNeutral
        finalWeightDisgustNeutral <- finalCountDisgustNeutral * 0.4
        finalCountDisgustHigh <- finalCountDisgustHigh + tempCountDisgustHigh
        finalWeightDisgustHigh <- finalCountDisgustHigh * 0.6
        finalCountDisgustHigher <- finalCountDisgustHigher + tempCountDisgustHigher
        finalWeightDisgustHigher <- finalCountDisgustHigher * 0.8
        finalCountDisgustHighest <- finalCountDisgustHighest + tempCountDisgustHighest
        finalWeightDisgustHighest <- finalCountDisgustHighest * 1
        #----------END STORE TEMP-TALLY-OF-DISGUST TO FINAL-TALLY-OF-DISGUST----------
        #----------START STORE TEMP-TALLY-OF-FEAR TO FINAL-TALLY-OF-FEAR----------
        finalCountFearLowest <- finalCountFearLowest + tempCountFearLowest
        finalWeightFearLowest <- finalCountFearLowest * 0
        finalCountFearLow <- finalCountFearLow + tempCountFearLow
        finalWeightFearLow <- finalCountFearLow * 0.2
        finalCountFearNeutral <- finalCountFearNeutral + tempCountFearNeutral
        finalWeightFearNeutral <- finalCountFearNeutral * 0.4
        finalCountFearHigh <- finalCountFearHigh + tempCountFearHigh
        finalWeightFearHigh <- finalCountFearHigh * 0.6
        finalCountFearHigher <- finalCountFearHigher + tempCountFearHigher
        finalWeightFearHigher <- finalCountFearHigher * 0.8
        finalCountFearHighest <- finalCountFearHighest + tempCountFearHighest
        finalWeightFearHighest <- finalCountFearHighest * 1
        #----------END STORE TEMP-TALLY-OF-FEAR TO FINAL-TALLY-OF-FEAR----------
      }
    }
    
    sumWeightsJoy <- base::sum(finalWeightJoyLowest, finalWeightJoyLow, finalWeightJoyNeutral, finalWeightJoyHigh, finalWeightJoyHigher, finalWeightJoyHighest)
    sumCountJoy <- base::sum(finalCountJoyLowest, finalCountJoyLow, finalCountJoyNeutral, finalCountJoyHigh, finalCountJoyHigher, finalCountJoyHighest)
    sumWeightsSadness <- base::sum(finalWeightSadnessLowest, finalWeightSadnessLow, finalWeightSadnessNeutral, finalWeightSadnessHigh, finalWeightSadnessHigher, finalWeightSadnessHighest)
    sumCountSadness <- base::sum(finalCountSadnessLowest, finalCountSadnessLow, finalCountSadnessNeutral, finalCountSadnessHigh, finalCountSadnessHigher, finalCountSadnessHighest)
    sumWeightsAnger <- base::sum(finalWeightAngerLowest, finalWeightAngerLow, finalWeightAngerNeutral, finalWeightAngerHigh, finalWeightAngerHigher, finalWeightAngerHighest)
    sumCountAnger <- base::sum(finalCountAngerLowest, finalCountAngerLow, finalCountAngerNeutral, finalCountAngerHigh, finalCountAngerHigher, finalCountAngerHighest)
    sumWeightsDisgust <- base::sum(finalWeightDisgustLowest, finalWeightDisgustLow, finalWeightDisgustNeutral, finalWeightDisgustHigh, finalWeightDisgustHigher, finalWeightDisgustHighest)
    sumCountDisgust <- base::sum(finalCountDisgustLowest, finalCountDisgustLow, finalCountDisgustNeutral, finalCountDisgustHigh, finalCountDisgustHigher, finalCountDisgustHighest)
    sumWeightsFear <- base::sum(finalWeightFearLowest, finalWeightFearLow, finalWeightFearNeutral, finalWeightFearHigh, finalWeightFearHigher, finalWeightFearHighest)
    sumCountFear <- base::sum(finalCountFearLowest, finalCountFearLow, finalCountFearNeutral, finalCountFearHigh, finalCountFearHigher, finalCountFearHighest)
      
    #----------START JOY----------
    
    output$joyBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeightsJoy, 
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCountJoy, " - total word/s found" 
        ), icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1
      )
    })
    output$joyHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest Joy",
        value = shiny::tagList(
          finalWeightJoyHighest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoyHighest, " - word/s found"),
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1
      )
    })
    output$joyHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher Joy",
        value = shiny::tagList(
          finalWeightJoyHigher,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoyHigher, " - word/s found"),
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1
      )
    })
    output$joyHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High Joy",
        value = shiny::tagList(
          finalWeightJoyHigh,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoyHigh, " - word/s found"),
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1
      )
    })
    output$joyNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral Joy",
        value = shiny::tagList(
          finalWeightJoyNeutral,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoyNeutral, " - word/s found"),
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1, fill = TRUE
      )
    })
    output$joyLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low Joy", 
        value = shiny::tagList(
          finalWeightJoyLow,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoyLow, " - word/s found"), 
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1, fill = TRUE)
    })
    output$joyLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest Joy", 
        value = shiny::tagList(
          finalWeightJoyLowest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoyLowest, " - word/s found"), 
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1, fill = TRUE)
    })
    
    #----------END JOY----------
    
    #----------START SADNESS----------
    
    output$sadnessBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeightsSadness,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCountSadness, " - total word/s found"
        ), icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest Sadness",
        value = shiny::tagList(
          finalWeightSadnessHighest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadnessHighest, " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher Sadness",
        value = shiny::tagList(
          finalWeightSadnessHigher,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadnessHigher, " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High Sadness",
        value = shiny::tagList(
          finalWeightSadnessHigh,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadnessHigh, " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral Sadness",
        value = shiny::tagList(
          finalWeightSadnessNeutral,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadnessNeutral, " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE
      )
    })
    output$sadnessLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low Sadness",
        value = shiny::tagList(
          finalWeightSadnessLow,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadnessLow, " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE)
    })
    output$sadnessLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest Sadness",
        value = shiny::tagList(
          finalWeightSadnessLowest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadnessLowest, " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE)
    })
    
    #----------END SADNESS----------
    
    #----------START ANGER----------
    
    output$angerBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeightsAnger,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCountAnger, " - total word/s found"
        ), icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest anger",
        value = shiny::tagList(
          finalWeightAngerHighest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoyHighest, " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher anger",
        value = shiny::tagList(
          finalWeightAngerHigher,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoyHigher, " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High anger",
        value = shiny::tagList(
          finalWeightAngerHigh,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoyHigh, " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral anger",
        value = shiny::tagList(
          finalWeightAngerNeutral,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountAngerNeutral, " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE
      )
    })
    output$angerLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low anger",
        value = shiny::tagList(
          finalWeightAngerLow,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountAngerLow, " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE)
    })
    output$angerLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest anger",
        value = shiny::tagList(
          finalWeightAngerLowest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountAngerLowest, " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE)
    })
    
    #----------END ANGER----------
    
    #----------START DISGUST----------
    
    output$disgustBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeightsDisgust,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCountDisgust, " - total word/s found",
          0
        ), icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest disgust",
        value = shiny::tagList(
          finalWeightDisgustHighest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgustHighest, " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher disgust",
        value = shiny::tagList(
          finalWeightFearHigher,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgustHigher, " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High disgust",
        value = shiny::tagList(
          finalWeightFearHigh,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgustHigh, " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral disgust",
        value = shiny::tagList(
          finalWeightDisgustNeutral,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgustNeutral, " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE
      )
    })  
    output$disgustLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low disgust",
        value = shiny::tagList(
          finalWeightDisgustLow,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgustLow, " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE)
    })
    output$disgustLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest disgust",
        value = shiny::tagList(
          finalWeightDisgustLowest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgustLowest, " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE)
    })
    
    #----------END DISGUST----------
    
    #----------START FEAR----------
    
    output$fearBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeightsFear,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCountFear, " - total word/s found"
        ), icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest fear",
        value = shiny::tagList(
          finalWeightFearHighest,
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFearHighest, " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest fear",
        value = shiny::tagList(
          finalWeightFearHigher,
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFearHigher, " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High fear",
        value = shiny::tagList(
          finalWeightFearHigh,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFearHigh, " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral fear",
        value = shiny::tagList(
          finalWeightFearNeutral,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFearNeutral, " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE
      )
    })
    output$fearLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low fear",
        value = shiny::tagList(
          finalWeightFearLow,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFearLow, " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE)
    })
    output$fearLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest fear",
        value = shiny::tagList(
          finalWeightFearLowest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFearLowest, " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE)
    })
    
    #----------END FEAR----------
  })
  
})