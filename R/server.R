require(magrittr)

source("joyFuzzyRules.R")
source("sadnessFuzzyRules.R")
source("angerFuzzyRules.R")
source("disgustFuzzyRules.R")
source("fearFuzzyRules.R")
source("Calculations.R")

shiny::shinyServer(function(input, output, session) {
  shiny::updateDateRangeInput(session = session, inputId = "dateRangeId", start = base::Sys.Date() - 7, end = base::Sys.Date())
  
  joyData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Joy", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
  sadnessData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Sadness", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
  angerData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Anger", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
  disgustData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Disgust", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
  fearData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Fear", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
  contrastingConjunctions <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Constrasting Conjunctions", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
  
  shiny::observeEvent(input$searchButton, {
    # fb_oauth <- "EAACEdEose0cBADQoZB0CnWWRnYlG1MdXGEZAOXeWc6yKW494e6GypZCWYHwxp1ycjey5gS45xTinaHBzZBXNxY9YGl7amnV5Qnp7h8h4rD7kY3z12Hcz2s1IBgTiPeKyo2AXh1hQHz1kZBxTBROws6eydmfMuTiNQ2SaDMgSZCSFB1k7qVb39ZB8Dam45bCh48ZD"
    base::load(file = "fb_oauth")
    
    base::tryCatch(expr = {
      listOfPosts <- Rfacebook::getPage(page = input$searchText, token = fb_oauth, n = base::as.numeric(input$numberOfPosts), since = input$dateRangeId[1], until = input$dateRangeId[2], feed = FALSE, reactions = TRUE, verbose = TRUE)
      
      
      progress <- shiny::Progress$new(session, min=1, max=15)
      on.exit(progress$close())
      
      progress$set(message = "Getting Facebook Post", detail = "This may take a while...")
      
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
          listofComments$comments$message
        }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs", width = "auto", rownames = TRUE, colnames = TRUE, na = NA)
        
        c("#", input$postListId, ":", listOfPosts$message[as.numeric(input$postListId)])
      })
      
      output$downloadCSV <- shiny::renderUI({
        shiny::downloadLink(outputId = "downloadCsvFileId", label = shiny::tagList(
          shiny::icon(name = "download", class = "fa-1x", lib = "font-awesome"), "Download Posts")
        )
      })
      
      output$viewPostUIId <- shiny::renderUI({
        shinydashboard::box(title = "Post", width = 12, solidHeader = TRUE, status = "primary", background = NULL,
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
    analyzePostAndItsComments <- c()
    if ((input$searchText != "") & (input$manualPostTextAreaId == "" | input$manualPostTextAreaId != "")) {
      base::load(file = "fb_oauth")
      
      base::tryCatch(expr = {
        listOfPostsForAnalysis <- Rfacebook::getPage(page = input$searchText, token = fb_oauth, n = base::as.numeric(input$numberOfPosts), since = input$dateRangeId[1], until = input$dateRangeId[2], feed = FALSE, reactions = TRUE, verbose = TRUE)
        for (i in 1:length(listOfPostsForAnalysis$message)) {
          if (validUTF8(listOfPostsForAnalysis$message[i])) {
            analyzePostAndItsComments <- append(x = analyzePostAndItsComments, values = listOfPostsForAnalysis$message[i])
          }
        }

        for (y in 1:length(listOfPostsForAnalysis$message)) {
          listofCommentsForAnalysis <- Rfacebook::getPost(post = listOfPostsForAnalysis$id[y], token = fb_oauth, n = base::as.numeric(input$numberOfComments), comments = TRUE, likes = TRUE)
          for (z in 1:length(listofCommentsForAnalysis$comments$message)) {
            analyzePostAndItsComments <- append(x = analyzePostAndItsComments, values = listofCommentsForAnalysis$comments$message[z])
          }
        }
        
        shiny::showNotification(ui = "Finished Analyzing...", duration = 5, closeButton = FALSE, type = "message", session = shiny::getDefaultReactiveDomain())
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
      
    } else {
      analyzePostAndItsComments <- input$manualPostTextAreaId
      output$viewPostUIId <- shiny::renderUI({
        shinydashboard::box(title = "Post", width = 12, solidHeader = TRUE, status = "primary",
          shiny::textOutput(outputId = "viewPostId")
        )
      })
  
      output$viewPostId <- shiny::renderText({
        input$manualPostTextAreaId
      })
    }
    
    # -----------------START-EMOTIONAL-ANALYSIS------------------
        
    joyData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Joy", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    sadnessData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Sadness", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    angerData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Anger", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    disgustData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Disgust", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    fearData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Fear", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    contrastingConjunctions <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Constrasting Conjunctions", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

    progress <- shiny::Progress$new(session, min = 1, max = length(analyzePostAndItsComments))
    on.exit(progress$close())

    progress$set(message = "Calculation in progress", detail = "This may take a while...")

    for (i in 1:length(analyzePostAndItsComments)) {

      # tokenizeSentences <- tokenizers::tokenize_sentences(x = stringi::stri_enc_toutf8(analyzePostAndItsComments[w]), lowercase = FALSE, strip_punctuation = FALSE, simplify = FALSE)
      tokenizeLines <- tokenizers::tokenize_lines(x = analyzePostAndItsComments[i], simplify = TRUE)

      finalCountJoy <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
      finalWeightJoy <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
      finalCountSadness <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
      finalWeightSadness <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
      finalCountAnger <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
      finalWeightAnger <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
      finalCountDisgust <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
      finalWeightDisgust <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
      finalCountFear <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
      finalWeightFear <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
      sumWeights <- list(Joy = 0, Sadness = 0, Anger = 0, Disgust = 0, Fear = 0)
      sumCounts <- list(Joy = 0, Sadness = 0, Anger = 0, Disgust = 0, Fear = 0)

      for (j in 1:length(tokenizeLines)) {
        tokenizeWords <- tokenizers::tokenize_words(x = tokenizeLines[j], lowercase = TRUE, stopwords = NULL, simplify = TRUE)
        
        tempCountJoy <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
        tempWeightJoy <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
        tempCountSadness <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
        tempWeightSadness <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
        tempCountAnger <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
        tempWeightAnger <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
        tempCountDisgust <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
        tempWeightDisgust <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
        tempCountFear <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
        tempWeightFear <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
        
        for (k in 1:length(tokenizeWords)) {
          # for (l in 1:nrow(contrastingConjunctions)) { #check if there are contrasting conjunctions.
          # 
          #   if (identical(tokenizeWords[k], contrastingConjunctions[l, 1])) {
          #     tempCountJoy <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
          #     tempWeightJoy <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
          #     tempCountSadness <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
          #     tempWeightSadness <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
          #     tempCountAnger <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
          #     tempWeightAnger <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
          #     tempCountDisgust <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
          #     tempWeightDisgust <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
          #     tempCountFear <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
          #     tempWeightFear <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
          #   }
          # }
          
          #----------START JOY-FUZZY-SETS----------
          joy.FuzzyRules(joyData, tokenizeWords[k], tokenizeWords[k+1])
          #----------END JOY-FUZZY-SETS----------
          #----------START SADNESS-FUZZY-SETS----------
          # sadness.FuzzyRules(sadnessData, tokenizeWords[k], tokenizeWords[k+1])
          #----------END SADNESS-FUZZY-SETS----------
          #----------START ANGER-FUZZY-SETS----------
          # anger.FuzzyRules(angerData, tokenizeWords[k], tokenizeWords[k+1])
          #----------END ANGER-FUZZY-SETS----------
          #----------START DISGUST-FUZZY-SETS----------
          # disgust.FuzzyRules(disgustData, tokenizeWords[k], tokenizeWords[k+1])
          #----------END DISGUST-FUZZY-SETS----------
          #----------START FEAR-FUZZY-SETS----------
          # fear.FuzzyRules(fearData, tokenizeWords[k], tokenizeWords[k+1])
          #----------END FEAR-FUZZY-SETS----------
          
        }
        tally.emotions()

      }
      total.emotions()
    #   if (length(tokenizeSentences[[1]]) == 0) {
    #     shiny::showNotification(ui = "No post to analyze. Please fill Post Box above.", action = NULL, duration = 5, closeButton = TRUE, type = "error", session = shiny::getDefaultReactiveDomain())
    #   } else {
    #     for (i in 1:length(tokenizeSentences[[1]])) {
    #       progress$set(value = i)
    #       Sys.sleep(0.25)
    #       tokenizeWords <- tokenizers::tokenize_words(x = analyzePostAndItsComments[i], lowercase = TRUE, stopwords = NULL, simplify = FALSE)
    # 
    #       tempCountJoy <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #       tempWeightJoy <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #       tempCountSadness <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #       tempWeightSadness <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #       tempCountAnger <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #       tempWeightAnger <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #       tempCountDisgust <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #       tempWeightDisgust <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #       tempCountFear <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #       tempWeightFear <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    # 
    #       if (length(tokenizeWords[[1]]) == 0) {
    #         shiny::showNotification(ui = "No post to analyze. Please fill Post Box above.", action = NULL, duration = 5, closeButton = TRUE, type = "error", session = shiny::getDefaultReactiveDomain())
    #       } else {
    #         print(contrastingConjunctions)
    #         for (j in 1:length(tokenizeWords[[1]])) { #looping of words
    #           for (k in 1:nrow(contrastingConjunctions)) { #check if there are contrasting conjunctions.
    # 
    #             if (is.na(contrastingConjunctions[k, 1])) {
    #             } else if (tokenizeWords[[1]][j] == contrastingConjunctions[k, 1]) {
    #               tempCountJoy <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #               tempWeightJoy <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #               tempCountSadness <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #               tempWeightSadness <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #               tempCountAnger <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #               tempWeightAnger <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #               tempCountDisgust <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #               tempWeightDisgust <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #               tempCountFear <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #               tempWeightFear <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    #             }
    #           }
    # 
    #           #----------START JOY-FUZZY-SETS----------
    #           joy.FuzzyRules(joyData, tokenizeWords[[1]][j], tokenizeWords[[1]][j+1])
    #           #----------END JOY-FUZZY-SETS----------
    #           #----------START SADNESS-FUZZY-SETS----------
    #           sadness.FuzzyRules(sadnessData, tokenizeWords[[1]][j], tokenizeWords[[1]][j+1])
    #           #----------END SADNESS-FUZZY-SETS----------
    #           #----------START ANGER-FUZZY-SETS----------
    #           anger.FuzzyRules(angerData, tokenizeWords[[1]][j], tokenizeWords[[1]][j+1])
    #           #----------END ANGER-FUZZY-SETS----------
    #           #----------START DISGUST-FUZZY-SETS----------
    #           disgust.FuzzyRules(disgustData, tokenizeWords[[1]][j], tokenizeWords[[1]][j+1])
    #           #----------END DISGUST-FUZZY-SETS----------
    #           #----------START FEAR-FUZZY-SETS----------
    #           fear.FuzzyRules(fearData, tokenizeWords[[1]][j], tokenizeWords[[1]][j+1])
    #           #----------END FEAR-FUZZY-SETS----------
    #         }
    #       }
    # 
    #       tally.emotions()
    #     }
    #   }
    # 
    #   total.emotions()
    }
    print("successfully.. analyzed...")
    
    # -----------------END-EMOTIONAL-ANALYSIS------------------
      
    #----------START JOY----------
    
    output$joyBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeights[["Joy"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCounts[["Joy"]], " - total word/s found"
        ), icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1
      )
    })
    output$joyHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest Joy",
        value = shiny::tagList(
          finalWeightJoy[["Highest"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoy[["Highest"]], " - word/s found"),
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1
      )
    })
    output$joyHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher Joy",
        value = shiny::tagList(
          finalWeightJoy[["Higher"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoy[["Higher"]], " - word/s found"),
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1
      )
    })
    output$joyHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High Joy",
        value = shiny::tagList(
          finalWeightJoy[["High"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoy[["High"]], " - word/s found"),
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1
      )
    })
    output$joyNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral Joy",
        value = shiny::tagList(
          finalWeightJoy[["Neutral"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoy[["Neutral"]], " - word/s found"),
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1, fill = TRUE
      )
    })
    output$joyLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low Joy",
        value = shiny::tagList(
          finalWeightJoy[["Low"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoy[["Low"]], " - word/s found"),
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1, fill = TRUE)
    })
    output$joyLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest Joy",
        value = shiny::tagList(
          finalWeightJoy[["Lowest"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountJoy[["Lowest"]], " - word/s found"),
        icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1, fill = TRUE)
    })
    
    #----------END JOY----------
    
    #----------START SADNESS----------
    
    # output$sadnessBoxId <- shinydashboard::renderValueBox({
    #   shinydashboard::valueBox(value = shiny::tagList(
    #       sumWeights[["Sadness"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(
    #       sumCounts[["Sadness"]], " - total word/s found"
    #     ), icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
    #   )
    # })
    # output$sadnessHighestBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Highest Sadness",
    #     value = shiny::tagList(
    #       finalWeightSadness[["Highest"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountSadness[["Highest"]], " - word/s found"),
    #     icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
    #   )
    # })
    # output$sadnessHigherBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Higher Sadness",
    #     value = shiny::tagList(
    #       finalWeightSadness[["Higher"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountSadness[["Higher"]], " - word/s found"),
    #     icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
    #   )
    # })
    # output$sadnessHighBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "High Sadness",
    #     value = shiny::tagList(
    #       finalWeightSadness[["High"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountSadness[["High"]], " - word/s found"),
    #     icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
    #   )
    # })
    # output$sadnessNeutralBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Neutral Sadness",
    #     value = shiny::tagList(
    #       finalWeightSadness[["Neutral"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountSadness[["Neutral"]], " - word/s found"),
    #     icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE
    #   )
    # })
    # output$sadnessLowBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Low Sadness",
    #     value = shiny::tagList(
    #       finalWeightSadness[["Low"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountSadness[["Low"]], " - word/s found"),
    #     icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE)
    # })
    # output$sadnessLowestBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Lowest Sadness",
    #     value = shiny::tagList(
    #       finalWeightSadness[["lowest"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountSadness[["Lowest"]], " - word/s found"),
    #     icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE)
    # })
    
    #----------END SADNESS----------
    
    #----------START ANGER----------
    
    # output$angerBoxId <- shinydashboard::renderValueBox({
    #   shinydashboard::valueBox(value = shiny::tagList(
    #       sumWeights[["Anger"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(
    #       sumCounts[["Anger"]], " - total word/s found"
    #     ), icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
    #   )
    # })
    # output$angerHighestBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Highest anger",
    #     value = shiny::tagList(
    #       finalWeightAnger[["Highest"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountAnger[["Highest"]], " - word/s found"),
    #     icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
    #   )
    # })
    # output$angerHigherBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Higher anger",
    #     value = shiny::tagList(
    #       finalWeightAnger[["Higher"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountAnger[["Higher"]], " - word/s found"),
    #     icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
    #   )
    # })
    # output$angerHighBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "High anger",
    #     value = shiny::tagList(
    #       finalWeightAnger[["High"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountAnger[["High"]], " - word/s found"),
    #     icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
    #   )
    # })
    # output$angerNeutralBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Neutral anger",
    #     value = shiny::tagList(
    #       finalWeightAnger[["Neutral"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountAnger[["Neutral"]], " - word/s found"),
    #     icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE
    #   )
    # })
    # output$angerLowBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Low anger",
    #     value = shiny::tagList(
    #       finalWeightAnger[["Low"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountAnger[["Low"]], " - word/s found"),
    #     icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE)
    # })
    # output$angerLowestBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Lowest anger",
    #     value = shiny::tagList(
    #       finalWeightAnger[["Lowest"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountAnger[["Lowest"]], " - word/s found"),
    #     icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE)
    # })
    
    #----------END ANGER----------
    
    #----------START DISGUST----------
    
    # output$disgustBoxId <- shinydashboard::renderValueBox({
    #   shinydashboard::valueBox(value = shiny::tagList(
    #       sumWeights[["Disgust"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(
    #       sumCounts[["Disgust"]], " - total word/s found"
    #     ), icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
    #   )
    # })
    # output$disgustHighestBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Highest disgust",
    #     value = shiny::tagList(
    #       finalWeightDisgust[["Highest"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountDisgust[["Highest"]], " - word/s found"),
    #     icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
    #   )
    # })
    # output$disgustHigherBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Higher disgust",
    #     value = shiny::tagList(
    #       finalWeightDisgust[["Higher"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountDisgust[["Higher"]], " - word/s found"),
    #     icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
    #   )
    # })
    # output$disgustHighBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "High disgust",
    #     value = shiny::tagList(
    #       finalWeightDisgust[["High"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountDisgust[["High"]], " - word/s found"),
    #     icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
    #   )
    # })
    # output$disgustNeutralBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Neutral disgust",
    #     value = shiny::tagList(
    #       finalWeightDisgust[["Neutral"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountDisgust[["Neutral"]], " - word/s found"),
    #     icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE
    #   )
    # })
    # output$disgustLowBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Low disgust",
    #     value = shiny::tagList(
    #       finalWeightDisgust[["Low"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountDisgust[["Low"]], " - word/s found"),
    #     icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE)
    # })
    # output$disgustLowestBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Lowest disgust",
    #     value = shiny::tagList(
    #       finalWeightDisgust[["Lowest"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountDisgust[["Lowest"]], " - word/s found"),
    #     icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE)
    # })
    
    #----------END DISGUST----------
    
    #----------START FEAR----------
    
    # output$fearBoxId <- shinydashboard::renderValueBox({
    #   shinydashboard::valueBox(value = shiny::tagList(
    #       sumWeights[["Fear"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(
    #       sumCounts[["Fear"]], " - total word/s found"
    #     ), icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
    #   )
    # })
    # output$fearHighestBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Highest fear",
    #     value = shiny::tagList(
    #       finalWeightFear[["Highest"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountFear[["Highest"]], " - word/s found"),
    #     icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
    #   )
    # })
    # output$fearHigherBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Highest fear",
    #     value = shiny::tagList(
    #       finalWeightFear[["Higher"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountFear[["Higher"]], " - word/s found"),
    #     icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
    #   )
    # })
    # output$fearHighBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "High fear",
    #     value = shiny::tagList(
    #       finalWeightFear[["High"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountFear[["High"]], " - word/s found"),
    #     icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
    #   )
    # })
    # output$fearNeutralBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Neutral fear",
    #     value = shiny::tagList(
    #       finalWeightFear[["Neutral"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountFear[["Neutral"]], " - word/s found"),
    #     icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE
    #   )
    # })
    # output$fearLowBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Low fear",
    #     value = shiny::tagList(
    #       finalWeightFear[["Low"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountFear[["Low"]], " - word/s found"),
    #     icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE)
    # })
    # output$fearLowestBoxId <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(title = "Lowest fear",
    #     value = shiny::tagList(
    #       finalWeightFear[["Lowest"]],
    #       shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
    #     ), subtitle = shiny::tagList(finalCountFear[["Lowest"]], " - word/s found"),
    #     icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE)
    # })
    
    #----------END FEAR----------
    
    #----------START-OF-GRAPHS/CHARTS----------
    
    # output$plot <- renderPlot({
    #   emotions.counts <- c(Joy = sumWeights[["Joy"]],
    #     Sadness = sumWeights[["Sadness"]], Anger = sumWeights[["Anger"]],
    #     Disgust = sumWeights[["Disgust"]], Fear = sumWeights[["Fear"]])
    #   graphics::barplot(height = emotions.counts, col = c("orange", "blue", "red", "green", "purple"), border = c("orange", "blue", "red", "green", "purple"))
    # })
    # 
    # output$plot1 <- renderPlot({
    #   countsOfAll <- c(
    #     finalCountJoy[["Lowest"]], finalCountSadness[["Lowest"]], finalCountAnger[["Lowest"]], finalCountDisgust[["Lowest"]], finalCountFear[["Lowest"]],
    #     finalCountJoy[["Low"]], finalCountSadness[["Low"]], finalCountAnger[["Low"]], finalCountDisgust[["Low"]], finalCountFear[["Low"]],
    #     finalCountJoy[["Neutral"]], finalCountSadness[["Neutral"]], finalCountAnger[["Neutral"]], finalCountDisgust[["Neutral"]], finalCountFear[["Neutral"]],
    #     finalCountJoy[["High"]], finalCountSadness[["High"]], finalCountAnger[["High"]], finalCountDisgust[["High"]], finalCountFear[["High"]],
    #     finalCountJoy[["Higher"]], finalCountSadness[["Higher"]], finalCountAnger[["Higher"]], finalCountDisgust[["Higher"]], finalCountFear[["Higher"]],
    #     finalCountJoy[["Highest"]], finalCountSadness[["Highest"]], finalCountAnger[["Highest"]], finalCountDisgust[["Highest"]], finalCountFear[["Highest"]]
    #     )
    #   # countsOfAll <- c(1:30)
    #   countsMatrix <- matrix(countsOfAll, ncol = 5, byrow = TRUE)
    #   rownames(countsMatrix) <- c("Lowest", "Low", "Neutral", "High", "Higher", "Highest")
    #   colnames(countsMatrix) <- c("Joy", "Sadness", "Anger", "Digust", "Fear")
    #   tableDataEmotions <- as.table(countsMatrix)
    #   graphics::barplot(height = tableDataEmotions, width = 2, main = "Emotions vs Degrees",
    #     xlab = "Emotions", ylab = "Degrees", col = grey.colors(length(rownames(tableDataEmotions))), legend.text = rownames(tableDataEmotions))
    # })
    
    #----------END-OF-GRAPHS/CHARTS----------
    
  })
  
})