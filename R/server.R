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

    #----------START JOY----------
    
    joyData <- openxlsx::readWorkbook(xlsxFile = "emotions-final.xlsx", sheet = "Joy", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    contrastingConjunctions <- openxlsx::readWorkbook(xlsxFile = "emotions-final.xlsx", sheet = "Constrasting Conjunctions", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    
    tokenizeSentences <- tokenizers::tokenize_sentences(x = input$manualPostTextAreaId, lowercase = FALSE, strip_punctuation = FALSE, simplify = FALSE)
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
    if (length(tokenizeSentences[[1]]) == 0) {
      shiny::showNotification(ui = "No post to analyze. Please fill Post Box above.", action = NULL, duration = 5, closeButton = TRUE, type = "error", session = shiny::getDefaultReactiveDomain())
    } else {
      for (i in 1:length(tokenizeSentences[[1]])) {
        tokenizeWords <- tokenizers::tokenize_words(x = tokenizeSentences[[1]][i], lowercase = TRUE, stopwords = NULL, simplify = FALSE)
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
        if (length(tokenizeWords[[1]]) == 0) {
          shiny::showNotification(ui = "No post to analyze. Please fill Post Box above.", action = NULL, duration = 5, closeButton = TRUE, type = "error", session = shiny::getDefaultReactiveDomain())
        } else {
          for (j in 1:length(tokenizeWords[[1]])) { #looping of words
            
            for (k in 1:nrow(contrastingConjunctions)) { #check if there are contrasting conjunctions.
              if (tokenizeWords[[1]][j] == contrastingConjunctions[k, 1]) {
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
              }
            }
            
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
                for (n in 1:nrow(joyData)) {
                  if (is.na(joyData[n, 3])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[n, 3]) {
                    tempCountJoyLow <- tempCountJoyLow + 1
                    tempCountJoyNeutral <- tempCountJoyNeutral - 1
                    break()
                  } # look up words in neutral column then add intensifier
        
                  if (is.na(joyData[n, 4])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[n, 4]) {
                    tempCountJoyLow <- tempCountJoyLow + 1
                    tempCountJoyHigh <- tempCountJoyHigh - 1
                    break()
                  }
        
                  if (is.na(joyData[n, 5])) {
                  } else if (tokenizeWords[[1]][j+1] == joyData[n, 5]) {
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
          }  
        }
        
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
      }
    }
    
    output$joyBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          base::sum(finalWeightJoyLowest, finalWeightJoyLow, finalWeightJoyNeutral, finalWeightJoyHigh, finalWeightJoyHigher, finalWeightJoyHighest), 
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          base::sum(finalCountJoyLowest, finalCountJoyLow, finalCountJoyNeutral, finalCountJoyHigh, finalCountJoyHigher, finalCountJoyHighest), " - total word/s found" 
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
          # base::sum(finalWeightJoyLowest, finalWeightJoyLow, finalWeightJoyNeutral, finalWeightJoyHigh, finalWeightJoyHigher, finalWeightJoyHighest), 
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          # base::sum(finalCountJoyLowest, finalCountJoyLow, finalCountJoyNeutral, finalCountJoyHigh, finalCountJoyHigher, finalCountJoyHighest), " - total word/s found" 
          0
        ), icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest Sadness",
        value = shiny::tagList(
          # finalWeightJoyHighest,
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHighest, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher Sadness",
        value = shiny::tagList(
          # finalWeightJoyHigher,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHigher, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High Sadness",
        value = shiny::tagList(
          # finalWeightJoyHigh,
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHigh, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral Sadness",
        value = shiny::tagList(
          finalWeightJoyNeutral,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyNeutral, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE
      )
    })
    output$sadnessLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low Sadness",
        value = shiny::tagList(
          # finalWeightJoyLow,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyLow, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE)
    })
    output$sadnessLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest Sadness",
        value = shiny::tagList(
          # finalWeightJoyLowest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyLowest, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE)
    })
    
    #----------END SADNESS----------
    
    #----------START FEAR----------
    
    output$fearBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          # base::sum(finalWeightJoyLowest, finalWeightJoyLow, finalWeightJoyNeutral, finalWeightJoyHigh, finalWeightJoyHigher, finalWeightJoyHighest), 
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          # base::sum(finalCountJoyLowest, finalCountJoyLow, finalCountJoyNeutral, finalCountJoyHigh, finalCountJoyHigher, finalCountJoyHighest), " - total word/s found" 
          0
        ), icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest fear",
        value = shiny::tagList(
          # finalWeightJoyHighest,
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHighest, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher fear",
        value = shiny::tagList(
          # finalWeightJoyHigher,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHigher, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High fear",
        value = shiny::tagList(
          # finalWeightJoyHigh,
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHigh, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral fear",
        value = shiny::tagList(
          finalWeightJoyNeutral,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyNeutral, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE
      )
    })
    output$fearLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low fear",
        value = shiny::tagList(
          # finalWeightJoyLow,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyLow, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE)
    })
    output$fearLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest fear",
        value = shiny::tagList(
          # finalWeightJoyLowest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyLowest, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE)
    })
    
    #----------END FEAR----------
    
    #----------START DISGUST----------
    
    output$disgustBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          # base::sum(finalWeightJoyLowest, finalWeightJoyLow, finalWeightJoyNeutral, finalWeightJoyHigh, finalWeightJoyHigher, finalWeightJoyHighest), 
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          # base::sum(finalCountJoyLowest, finalCountJoyLow, finalCountJoyNeutral, finalCountJoyHigh, finalCountJoyHigher, finalCountJoyHighest), " - total word/s found" 
          0
        ), icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest disgust",
        value = shiny::tagList(
          # finalWeightJoyHighest,
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHighest, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher disgust",
        value = shiny::tagList(
          # finalWeightJoyHigher,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHigher, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High disgust",
        value = shiny::tagList(
          # finalWeightJoyHigh,
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHigh, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral disgust",
        value = shiny::tagList(
          finalWeightJoyNeutral,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyNeutral, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE
      )
    })
    output$disgustLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low disgust",
        value = shiny::tagList(
          # finalWeightJoyLow,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyLow, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE)
    })
    output$disgustLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest disgust",
        value = shiny::tagList(
          # finalWeightJoyLowest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyLowest, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE)
    })
    
    #----------END DISGUST----------
    
    #----------START ANGER----------
    
    output$angerBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          # base::sum(finalWeightJoyLowest, finalWeightJoyLow, finalWeightJoyNeutral, finalWeightJoyHigh, finalWeightJoyHigher, finalWeightJoyHighest), 
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          # base::sum(finalCountJoyLowest, finalCountJoyLow, finalCountJoyNeutral, finalCountJoyHigh, finalCountJoyHigher, finalCountJoyHighest), " - total word/s found" 
          0
        ), icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest anger",
        value = shiny::tagList(
          # finalWeightJoyHighest,
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHighest, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher anger",
        value = shiny::tagList(
          # finalWeightJoyHigher,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHigher, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High anger",
        value = shiny::tagList(
          # finalWeightJoyHigh,
          0,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyHigh, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral anger",
        value = shiny::tagList(
          finalWeightJoyNeutral,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyNeutral, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE
      )
    })
    output$angerLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low anger",
        value = shiny::tagList(
          # finalWeightJoyLow,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyLow, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE)
    })
    output$angerLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest anger",
        value = shiny::tagList(
          # finalWeightJoyLowest,
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        # ), subtitle = shiny::tagList(finalCountJoyLowest, " - word/s found"),
        ), subtitle = 0,
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE)
    })
    
    #----------END ANGER----------
  })
  
})