require(magrittr)

source("joyFuzzyRules.R")
source("sadnessFuzzyRules.R")
source("angerFuzzyRules.R")
source("disgustFuzzyRules.R")
source("fearFuzzyRules.R")
source("emojisFuzzyRules.R")
source("emoticonsFuzzyRules.R")
source("Calculations.R")

shiny::shinyServer(function(input, output, session) {
  shiny::updateDateRangeInput(session = session, inputId = "dateRangeId", start = base::Sys.Date() - 7, end = base::Sys.Date())
  
  shiny::observeEvent(eventExpr = input$searchButton, handlerExpr = {
    # fb_oauth <- "EAACEdEose0cBADQoZB0CnWWRnYlG1MdXGEZAOXeWc6yKW494e6GypZCWYHwxp1ycjey5gS45xTinaHBzZBXNxY9YGl7amnV5Qnp7h8h4rD7kY3z12Hcz2s1IBgTiPeKyo2AXh1hQHz1kZBxTBROws6eydmfMuTiNQ2SaDMgSZCSFB1k7qVb39ZB8Dam45bCh48ZD"
    base::load(file = "fb_oauth")
    
    base::tryCatch(expr = {
      listOfPosts <- Rfacebook::getPage(page = input$searchText, token = fb_oauth, n = base::as.numeric(input$numberOfPosts), since = input$dateRangeId[1], until = input$dateRangeId[2], feed = FALSE, reactions = TRUE, verbose = TRUE)
      
      output$postListUIId <- shiny::renderUI({
        shiny::selectInput(inputId = "postListId", label = "Select Post #", choices = base::length(listOfPosts$message):1) # sorted from latest to oldest
      })
      
      output$viewPostId <- shiny::renderText({
        listofComments <- Rfacebook::getPost(post = listOfPosts$id[as.numeric(input$postListId)], token = fb_oauth, n = base::as.numeric(input$numberOfComments), comments = TRUE, likes = TRUE)
        
        output$viewCommentsId <- shiny::renderTable(expr = {
          withProgress(message = "Getting Facebook Post...", detail = "This may take a while...", value = 0, {
            for (i in 1:15) {
              incProgress(1/15)
              Sys.sleep(0.25)
            }
          })
          
          listofComments$comments$message
        }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs", width = "auto", rownames = TRUE, colnames = TRUE, na = NA)
        
        c("#", input$postListId, ":", listOfPosts$message[as.numeric(input$postListId)])
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
      
      output$downloadCSVUIId <- shiny::renderUI({
        shiny::downloadLink(outputId = "downloadCsvFileId", label = shiny::tagList(
          shiny::icon(name = "download", class = "fa-1x", lib = "font-awesome"), "Download Posts")
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
  
  # autoInvalidate <- reactiveTimer(2000)
  
  shiny::observeEvent(eventExpr = input$submitAnalyzePostId, handlerExpr = {
    
    # Emotions Tab #
    output$showEmotionsBoxes <- shiny::renderUI({
      shinydashboard::tabBox(id = "tabBoxId", selected = "All", title = "Emotions", width = 12, height = NULL, side = "left",
         shiny::tabPanel(title = "All Emotions", value = "All", icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"),
           shiny::fluidRow(
             shinydashboard::valueBoxOutput(outputId = "allJoyBoxId", width = 3),
             shinydashboard::valueBoxOutput(outputId = "allSadnessBoxId", width = 3),
             shinydashboard::valueBoxOutput(outputId = "allAngerBoxId", width = 3),
             shinydashboard::valueBoxOutput(outputId = "allDisgustBoxId", width = 3),
             shinydashboard::valueBoxOutput(outputId = "allFearBoxId", width = 3)
           )
         ),
         shiny::tabPanel(title = "Joy", value = "Joy", icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"),
           shiny::fluidRow(
             shinydashboard::valueBoxOutput(outputId = "joyBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "joyHighestBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "joyHigherBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "joyHighBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "joyNeutralBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "joyLowBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "joyLowestBoxId", width = 3)
           )
         ),
         shiny::tabPanel(title = "Sadness", value = "Sadness", icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"),
           shiny::fluidRow(
             shinydashboard::valueBoxOutput(outputId = "sadnessBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "sadnessHighestBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "sadnessHigherBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "sadnessHighBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "sadnessNeutralBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "sadnessLowBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "sadnessLowestBoxId", width = 3)
           )
         ),
         shiny::tabPanel(title = "Anger", value = "Anger", icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"),
           shiny::fluidRow(
             shinydashboard::valueBoxOutput(outputId = "angerBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "angerHighestBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "angerHigherBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "angerHighBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "angerNeutralBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "angerLowBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "angerLowestBoxId", width = 3)
           )
         ),
         shiny::tabPanel(title = "Disgust", value = "Disgust", icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"),
           shiny::fluidRow(
             shinydashboard::valueBoxOutput(outputId = "disgustBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "disgustHighestBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "disgustHigherBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "disgustHighBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "disgustNeutralBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "disgustLowBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "disgustLowestBoxId", width = 3)
           )
         ),
         shiny::tabPanel(title = "Fear", value = "Fear", icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"),
           shiny::fluidRow(
             shinydashboard::valueBoxOutput(outputId = "fearBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "fearHighestBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "fearHigherBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "fearHighBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "fearNeutralBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "fearLowBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "fearLowestBoxId", width = 3)
           )
         ),
         shiny::tabPanel(title = "Emojis Reaction", value = "Emojis", icon = shiny::icon(name = "facebook-square", class = "fa-1x", lib = "font-awesome"),
           shiny::fluidRow(
             shinydashboard::valueBoxOutput(outputId = "loveBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "hahaBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "sadBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "angryBoxId", width = 3)
           )
         ),
         shiny::tabPanel(title = "Emoticons", value = "Emoticons", icon = shiny::icon(name = "circle", class = "fa-1x", lib = "font-awesome"),
           shiny::fluidRow(
             shinydashboard::valueBoxOutput(outputId = "JoyEmoticonsBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "SadnessEmoticonsBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "AngerEmoticonsBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "DisgustEmoticonsBoxId", width = 3),
             shinydashboard::infoBoxOutput(outputId = "FearEmoticonsBoxId", width = 3)
           )
         )
      )
    })
    
    output$showSummaryWeightedEmotion <- shiny::renderUI({
      shinydashboard::box(title = "Summary of Weighted Emotions", width = 12, solidHeader = FALSE, status = "primary", background = NULL, shiny::plotOutput("plot"))
    })
    
    output$showDegreeOfEmotion <- shiny::renderUI({
      shinydashboard::box(title = "Degree of Emotions (Count-based Stacked Barplot)", width = 12, solidHeader = FALSE, status = "primary", background = NULL, shiny::plotOutput("plot1"))
    })
    
    output$showWordCloud <- shiny::renderUI({
      shinydashboard::box(title = "Word Cloud", width = 12, solidHeader = FALSE, status = "primary", background = NULL, shiny::plotOutput("plot2"))
    })
    ########################################################
    output$myImage <- renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='insideout.gif')
      
      # Generate the PNG
      png(outfile, width=400, height=300)
      hist(rnorm(input$obs), main="Generated in renderImage()")
      dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           width = 400,
           height = 300,
           alt = "This is alternate text")
    }, deleteFile = TRUE)
    ########################################################
    
    # observe(x = {
    #   # autoInvalidate()
    #   invalidateLater(1000, session)
    # })
    
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
    finalEmojis <- list(Love = 0, Haha = 0, Sad = 0, Angry = 0)
    finalWeightLove <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    finalWeightHaha <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    finalWeightSad <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    finalWeightAngry <- list(Lowest = 0, Low = 0, Neutral = 0, High = 0, Higher = 0, Highest = 0)
    finalCountEmoticons <- list(Joy = 0, Sadness = 0, Anger = 0, Disgust = 0, Fear = 0)
    finalWeightEmoticons <- list(Joy = 0, Sadness = 0, Anger = 0, Disgust = 0, Fear = 0)
    
    analyzePostAndItsComments <- c()
    detectedWordsGathered <- c()
    emojisLoveCounts <- c()
    emojisHahaCounts <- c()
    emojisSadCounts <- c()
    emojisAngryCounts <- c()
    
    joyData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Joy", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    sadnessData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Sadness", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    angerData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Anger", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    disgustData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Disgust", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    fearData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Fear", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    contrastingConjunctions <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emotion.xlsx", sheet = "Constrasting Conjunctions", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
    
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
        for (h in 1:length(listOfPostsForAnalysis$message)) {
          emojisLoveCounts <- append(x = emojisLoveCounts, values = listOfPostsForAnalysis$love_count[h])
          emojisHahaCounts <- append(x = emojisHahaCounts, values = listOfPostsForAnalysis$haha_count[h])
          emojisSadCounts <- append(x = emojisSadCounts, values = listOfPostsForAnalysis$sad_count[h])
          emojisAngryCounts <- append(x = emojisAngryCounts, values = listOfPostsForAnalysis$angry_count[h])
        }
        
        shiny::showNotification(ui = "Analyzing...", duration = 5, closeButton = FALSE, type = "message", session = shiny::getDefaultReactiveDomain())
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
    
    if (!identical(length(analyzePostAndItsComments), 0)) {
      # -----------------START-EMOTIONAL-ANALYSIS------------------
          
      unicodeRegex <- "<U\\+[a-zA-Z0-9]*>"
      unicodeRegex2 <- "<U\\+[a-zA-Z0-9]*><U\\+[a-zA-Z0-9]*>"
      smileyRegex <- "([0-9A-Za-z'\\&\\-\\.\\/\\(\\)=:;]+)|((?::|;|=)(?:-)?(?:\\)|D|P))"
      PostsNativeEncoded <- enc2native(analyzePostAndItsComments)
      AllEmoticons <- c()
      for (i in 1:length(PostsNativeEncoded)) {
        everyWord <- tokenizers::tokenize_regex(x = PostsNativeEncoded[i], pattern = "\\s+", simplify = TRUE)
        for (j in 1:length(everyWord)) {
          isEdTag <- grepl(pattern = "<ed>", x = everyWord[j])
          isUnicode <- grepl(pattern = unicodeRegex, x = everyWord[j])
          isUnicode2 <- grepl(pattern = unicodeRegex2, x = everyWord[j])
          isSmiley <- grepl(pattern = smileyRegex, x = everyWord[j])
          if (isTRUE(isEdTag)) {
            everyUnicode <- strsplit(x = everyWord[j], split = "<ed>")
            for (k in everyUnicode) {
              AllEmoticons <- append(x = AllEmoticons, values = k)
            }
          }
          if (isTRUE(isUnicode) | isTRUE(isUnicode2)) {
            getEmoticons <- regexpr(pattern = unicodeRegex, text = everyWord[j])
            emoticonsLists <- regmatches(x = everyWord[j], m = getEmoticons)
            AllEmoticons <- append(x = AllEmoticons, values = emoticonsLists)
            getEmoticons2 <- regexpr(pattern = unicodeRegex2, text = everyWord[j])
            emoticonsLists2 <- regmatches(x = everyWord[j], m = getEmoticons2)
            AllEmoticons <- append(x = AllEmoticons, values = emoticonsLists2)
          }
          if (isTRUE(isSmiley)) {
            getSmileys <- regexpr(pattern = smileyRegex, text = everyWord[j])
            emoticonsLists3 <- regmatches(x = everyWord[j], m = getSmileys)
            AllEmoticons <- append(x = AllEmoticons, values = emoticonsLists3)
          }
        }
      }
      
      emoticonsData <- openxlsx::readWorkbook(xlsxFile = "final-list-of-emoticons.xlsx", sheet = "emoticons", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
      print(AllEmoticons)
      emoticons.FuzzyRules(emoticonsData, AllEmoticons)
      
      emojis.FuzzyRules(emojisLoveCounts, emojisHahaCounts, emojisSadCounts, emojisAngryCounts)
      
      progress <- shiny::Progress$new()
      on.exit(expr = progress$close())
      progress$set(message = "Post and Comments: ", value = 0)
      totalPostAndComments <- length(analyzePostAndItsComments)
      
      for (i in 1:length(analyzePostAndItsComments)) {
        tokenizeLines <- tokenizers::tokenize_lines(x = analyzePostAndItsComments[i], simplify = TRUE)
  
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
        
        progress$inc(1/totalPostAndComments, detail = paste(i, " of ", length(analyzePostAndItsComments)))
        Sys.sleep(0.1)
        
        for (j in 1:length(tokenizeLines)) {
          tokenizeWords <- tokenizers::tokenize_words(x = tokenizeLines[j], lowercase = TRUE, stopwords = NULL, simplify = TRUE)
          
          for (k in 1:length(tokenizeWords)) {
            
            for (l in 1:nrow(contrastingConjunctions)) {
              if (identical(tokenizeWords[k], contrastingConjunctions[l, 1])) {
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
              }
            }
            
            #----------START JOY-FUZZY-SETS----------
            joy.FuzzyRules(joyData, tokenizeWords[k], tokenizeWords[k+1])
            #----------END JOY-FUZZY-SETS----------
            #----------START SADNESS-FUZZY-SETS----------
            sadness.FuzzyRules(sadnessData, tokenizeWords[k], tokenizeWords[k+1])
            #----------END SADNESS-FUZZY-SETS----------
            #----------START ANGER-FUZZY-SETS----------
            anger.FuzzyRules(angerData, tokenizeWords[k], tokenizeWords[k+1])
            #----------END ANGER-FUZZY-SETS----------
            #----------START DISGUST-FUZZY-SETS----------
            disgust.FuzzyRules(disgustData, tokenizeWords[k], tokenizeWords[k+1])
            #----------END DISGUST-FUZZY-SETS----------
            #----------START FEAR-FUZZY-SETS----------
            fear.FuzzyRules(fearData, tokenizeWords[k], tokenizeWords[k+1])
            #----------END FEAR-FUZZY-SETS----------
            
          }
          tally.emotions()
  
        }
        total.emotions()
      }
      shiny::showNotification(ui = "Done...", duration = 5, closeButton = FALSE, type = "message", session = shiny::getDefaultReactiveDomain())
      # -----------------END-EMOTIONAL-ANALYSIS------------------
    } else {
      shiny::showNotification(ui = "There are no inputs..", action = "You can override post, by providing manual input. Just ensure you search input is blank.", duration = 10, closeButton = TRUE, type = "warning", session = shiny::getDefaultReactiveDomain())
    }
    
    # -----------------START-ALL-EMOTIONS------------------
    
    output$allJoyBoxId <- shinydashboard::renderValueBox({
      # Re-execute this reactive expression after 2000 milliseconds
      
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeights[["Joy"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCounts[["Joy"]], " - total word/s found"
        ), icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"), color = "yellow", width = 1
      )
    })
    output$allSadnessBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeights[["Sadness"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCounts[["Sadness"]], " - total word/s found"
        ), icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$allAngerBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeights[["Anger"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCounts[["Anger"]], " - total word/s found"
        ), icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$allDisgustBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeights[["Disgust"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCounts[["Disgust"]], " - total word/s found"
        ), icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$allFearBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeights[["Fear"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCounts[["Fear"]], " - total word/s found"
        ), icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    
    # -----------------END-ALL-EMOTIONS------------------
    
    # -----------------START-EMOTICONS------------------
    
    output$JoyEmoticonsBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          finalWeightEmoticons[["Joy"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          finalCountEmoticons[["Joy"]], " - total emoticon/s found"
        ), icon = shiny::icon(name = "circle", class = "fa-1x", lib = "font-awesome"), color = "aqua", width = 1
      )
    })
    output$SadnessEmoticonsBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          finalWeightEmoticons[["Sadness"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          finalCountEmoticons[["Sadness"]], " - total emoticon/s found"
        ), icon = shiny::icon(name = "circle-o", class = "fa-1x", lib = "font-awesome"), color = "light-blue", width = 1
      )
    })
    output$AngerEmoticonsBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          finalWeightEmoticons[["Anger"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          finalCountEmoticons[["Anger"]], " - total emoticon/s found"
        ), icon = shiny::icon(name = "circle-o", class = "fa-1x", lib = "font-awesome"), color = "orange", width = 1
      )
    })
    output$DisgustEmoticonsBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          finalWeightEmoticons[["Disgust"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          finalCountEmoticons[["Disgust"]], " - total emoticon/s found"
        ), icon = shiny::icon(name = "circle-thin", class = "fa-1x", lib = "font-awesome"), color = "olive", width = 1
      )
    })
    output$FearEmoticonsBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          finalWeightEmoticons[["Fear"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          finalCountEmoticons[["Fear"]], " - total emoticon/s found"
        ), icon = shiny::icon(name = "circle-thin", class = "fa-1x", lib = "font-awesome"), color = "lime", width = 1
      )
    })
    
    # -----------------END-EMOTICONS------------------
    
    # -----------------START-EMOJIS------------------
    
    output$loveBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          finalEmojis[["Love"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          "Love Weights"
        ), icon = shiny::icon(name = "gratipay", class = "fa-1x", lib = "font-awesome"), color = "fuchsia", width = 1
      )
    })
    output$hahaBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          finalEmojis[["Haha"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          "Haha Weights"
        ), icon = shiny::icon(name = "sign-language", class = "fa-1x", lib = "font-awesome"), color = "teal", width = 1
      )
    })
    output$sadBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          finalEmojis[["Sad"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          "Sad Weights"
        ), icon = shiny::icon(name = "meh-o", class = "fa-1x", lib = "font-awesome"), color = "maroon", width = 1
      )
    })
    output$angryBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          finalEmojis[["Angry"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          "Angry Weights"
        ), icon = shiny::icon(name = "eye", class = "fa-1x", lib = "font-awesome"), color = "navy", width = 1
      )
    })
    
    # -----------------END-EMOJIS------------------
      
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
    
    output$sadnessBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeights[["Sadness"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCounts[["Sadness"]], " - total word/s found"
        ), icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest Sadness",
        value = shiny::tagList(
          finalWeightSadness[["Highest"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadness[["Highest"]], " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher Sadness",
        value = shiny::tagList(
          finalWeightSadness[["Higher"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadness[["Higher"]], " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High Sadness",
        value = shiny::tagList(
          finalWeightSadness[["High"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadness[["High"]], " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1
      )
    })
    output$sadnessNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral Sadness",
        value = shiny::tagList(
          finalWeightSadness[["Neutral"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadness[["Neutral"]], " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE
      )
    })
    output$sadnessLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low Sadness",
        value = shiny::tagList(
          finalWeightSadness[["Low"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadness[["Low"]], " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE)
    })
    output$sadnessLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest Sadness",
        value = shiny::tagList(
          finalWeightSadness[["lowest"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountSadness[["Lowest"]], " - word/s found"),
        icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"), color = "blue", width = 1, fill = TRUE)
    })
    
    #----------END SADNESS----------
    
    #----------START ANGER----------
    
    output$angerBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeights[["Anger"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCounts[["Anger"]], " - total word/s found"
        ), icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest anger",
        value = shiny::tagList(
          finalWeightAnger[["Highest"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountAnger[["Highest"]], " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher anger",
        value = shiny::tagList(
          finalWeightAnger[["Higher"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountAnger[["Higher"]], " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High anger",
        value = shiny::tagList(
          finalWeightAnger[["High"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountAnger[["High"]], " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1
      )
    })
    output$angerNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral anger",
        value = shiny::tagList(
          finalWeightAnger[["Neutral"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountAnger[["Neutral"]], " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE
      )
    })
    output$angerLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low anger",
        value = shiny::tagList(
          finalWeightAnger[["Low"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountAnger[["Low"]], " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE)
    })
    output$angerLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest anger",
        value = shiny::tagList(
          finalWeightAnger[["Lowest"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountAnger[["Lowest"]], " - word/s found"),
        icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"), color = "red", width = 1, fill = TRUE)
    })
    
    #----------END ANGER----------
    
    #----------START DISGUST----------
    
    output$disgustBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeights[["Disgust"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCounts[["Disgust"]], " - total word/s found"
        ), icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest disgust",
        value = shiny::tagList(
          finalWeightDisgust[["Highest"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgust[["Highest"]], " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Higher disgust",
        value = shiny::tagList(
          finalWeightDisgust[["Higher"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgust[["Higher"]], " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High disgust",
        value = shiny::tagList(
          finalWeightDisgust[["High"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgust[["High"]], " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1
      )
    })
    output$disgustNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral disgust",
        value = shiny::tagList(
          finalWeightDisgust[["Neutral"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgust[["Neutral"]], " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE
      )
    })
    output$disgustLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low disgust",
        value = shiny::tagList(
          finalWeightDisgust[["Low"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgust[["Low"]], " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE)
    })
    output$disgustLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest disgust",
        value = shiny::tagList(
          finalWeightDisgust[["Lowest"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountDisgust[["Lowest"]], " - word/s found"),
        icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"), color = "green", width = 1, fill = TRUE)
    })
    
    #----------END DISGUST----------
    
    #----------START FEAR----------
    
    output$fearBoxId <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = shiny::tagList(
          sumWeights[["Fear"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(
          sumCounts[["Fear"]], " - total word/s found"
        ), icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearHighestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest fear",
        value = shiny::tagList(
          finalWeightFear[["Highest"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFear[["Highest"]], " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearHigherBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Highest fear",
        value = shiny::tagList(
          finalWeightFear[["Higher"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFear[["Higher"]], " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearHighBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "High fear",
        value = shiny::tagList(
          finalWeightFear[["High"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFear[["High"]], " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1
      )
    })
    output$fearNeutralBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Neutral fear",
        value = shiny::tagList(
          finalWeightFear[["Neutral"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFear[["Neutral"]], " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE
      )
    })
    output$fearLowBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Low fear",
        value = shiny::tagList(
          finalWeightFear[["Low"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFear[["Low"]], " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE)
    })
    output$fearLowestBoxId <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = "Lowest fear",
        value = shiny::tagList(
          finalWeightFear[["Lowest"]],
          shiny::icon(name = "balance-scale", class = "fa-1x", lib = "font-awesome")
        ), subtitle = shiny::tagList(finalCountFear[["Lowest"]], " - word/s found"),
        icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"), color = "purple", width = 1, fill = TRUE)
    })
    
    #----------END FEAR----------
    
    #----------START-OF-GRAPHS/CHARTS----------
    
    output$plot <- renderPlot({
      emotions.counts <- c(Joy = sumWeights[["Joy"]],
        Sadness = sumWeights[["Sadness"]], Anger = sumWeights[["Anger"]],
        Disgust = sumWeights[["Disgust"]], Fear = sumWeights[["Fear"]])
      graphics::barplot(height = emotions.counts, col = c("orange", "blue", "red", "green", "purple"), border = c("orange", "blue", "red", "green", "purple"))
    })

    output$plot1 <- renderPlot({
      countsOfAll <- c(
        finalCountJoy[["Lowest"]], finalCountSadness[["Lowest"]], finalCountAnger[["Lowest"]], finalCountDisgust[["Lowest"]], finalCountFear[["Lowest"]],
        finalCountJoy[["Low"]], finalCountSadness[["Low"]], finalCountAnger[["Low"]], finalCountDisgust[["Low"]], finalCountFear[["Low"]],
        finalCountJoy[["Neutral"]], finalCountSadness[["Neutral"]], finalCountAnger[["Neutral"]], finalCountDisgust[["Neutral"]], finalCountFear[["Neutral"]],
        finalCountJoy[["High"]], finalCountSadness[["High"]], finalCountAnger[["High"]], finalCountDisgust[["High"]], finalCountFear[["High"]],
        finalCountJoy[["Higher"]], finalCountSadness[["Higher"]], finalCountAnger[["Higher"]], finalCountDisgust[["Higher"]], finalCountFear[["Higher"]],
        finalCountJoy[["Highest"]], finalCountSadness[["Highest"]], finalCountAnger[["Highest"]], finalCountDisgust[["Highest"]], finalCountFear[["Highest"]]
        )
      # countsOfAll <- c(1:30)
      countsMatrix <- matrix(countsOfAll, ncol = 5, byrow = TRUE)
      rownames(countsMatrix) <- c("Lowest", "Low", "Neutral", "High", "Higher", "Highest")
      colnames(countsMatrix) <- c("Joy", "Sadness", "Anger", "Digust", "Fear")
      tableDataEmotions <- as.table(countsMatrix)
      graphics::barplot(height = tableDataEmotions, width = 2, main = "Emotions vs Degrees",
        xlab = "Emotions", ylab = "Degrees", col = grey.colors(length(rownames(tableDataEmotions))), legend.text = rownames(tableDataEmotions))
    })
    
    output$plot2 <- renderPlot({
      wordcloud::wordcloud(words = detectedWordsGathered)
    })
    
    #----------END-OF-GRAPHS/CHARTS----------
    
  })
  
})