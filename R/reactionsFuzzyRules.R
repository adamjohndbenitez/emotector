reactions.FuzzyRules <- function(analyzePostAndItsComments, env = parent.frame()) {
  laughingHahaRegex <- "\\b(?:a*(?:h+a+)+h*?)\\b"
  laughingHeheRegex <- "\\b(?:e*(?:h+e+)+h*?)\\b"
  laughingHihiRegex <- "\\b(?:i*(?:h+i+)+h*?)\\b"
  laughOutLoudRegex <- "\\b(?:l+o+)+l+\\b"
  suprisedRegex <- "\\b(?:w+o+)+w+\\b"
  cryingRegex <- "\\b(?:u*(?:h+u+)+h*?)\\b"
  angryRegex <- "\\b(?:g*(?:g+r+)+r*?)\\b"
  disgustRegex <- "\\b(?:e*(?:e+w+)+w*?)\\b"
  scaredRegex <- "\\b(?:w*(?:w+a+)+h*?)\\b"
  
  for (i in 1:length(analyzePostAndItsComments)) {
	  everyWord <- tokenizers::tokenize_words(x = analyzePostAndItsComments[i], lowercase = TRUE, stopwords = NULL, simplify = TRUE)
    # everyWord <- tokenizers::tokenize_regex(x = analyzePostAndItsComments[i], pattern = "\\s+", simplify = TRUE)
    
    for (j in 1:length(everyWord)) {
      isLaughingHahaRegex <- grepl(pattern = laughingHahaRegex, x = everyWord[j])
      isLaughingHeheRegex <- grepl(pattern = laughingHeheRegex, x = everyWord[j])
      isLaughingHihiRegex <- grepl(pattern = laughingHihiRegex, x = everyWord[j])
      isLaughOutLoudRegex <- grepl(pattern = laughOutLoudRegex, x = everyWord[j])
      isSuprisedRegex <- grepl(pattern = suprisedRegex, x = everyWord[j])
      isCryingRegex <- grepl(pattern = cryingRegex, x = everyWord[j])
      isAngryRegex <- grepl(pattern = angryRegex, x = everyWord[j])
      isDisgustRegex <- grepl(pattern = disgustRegex, x = everyWord[j])
      isScaredRegex <- grepl(pattern = scaredRegex, x = everyWord[j])
      
      if (isTRUE(isLaughingHahaRegex) & (!identical(everyWord[j], "ha"))) {
        getLaughingHahaRegex <- regexpr(pattern = laughingHahaRegex, text = everyWord[j])
        matchLaughingHahaLists <- regmatches(x = everyWord[j], m = getLaughingHahaRegex)
        env$finalCountReactions[["Joy"]] <- env$finalCountReactions[["Joy"]] + 1
        env$finalWeightReactions[["Joy"]] <- env$finalWeightReactions[["Joy"]] + 0.6
        env$finalWeightJoy[["High"]] <- env$finalWeightJoy[["High"]] + 0.6
        env$finalCountJoy[["High"]] <- env$finalCountJoy[["High"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = matchLaughingHahaLists)
      } else if (isTRUE(isLaughingHeheRegex) & (!identical(everyWord[j], "he"))) {
        getLaughingHeheRegex <- regexpr(pattern = laughingHeheRegex, text = everyWord[j])
        matchLaughingHeheLists <- regmatches(x = everyWord[j], m = getLaughingHeheRegex)
        env$finalCountReactions[["Joy"]] <- env$finalCountReactions[["Joy"]] + 1
        env$finalWeightReactions[["Joy"]] <- env$finalWeightReactions[["Joy"]] + 0.6
        env$finalWeightJoy[["High"]] <- env$finalWeightJoy[["High"]] + 0.6
        env$finalCountJoy[["High"]] <- env$finalCountJoy[["High"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = matchLaughingHeheLists)
      } else if (isTRUE(isLaughingHihiRegex) & (!identical(everyWord[j], "hi"))) {
        getLaughingHihiRegex <- regexpr(pattern = laughingHihiRegex, text = everyWord[j])
        matchLaughingHihiLists <- regmatches(x = everyWord[j], m = getLaughingHihiRegex)
        env$finalCountReactions[["Joy"]] <- env$finalCountReactions[["Joy"]] + 1
        env$finalWeightReactions[["Joy"]] <- env$finalWeightReactions[["Joy"]] + 0.6
        env$finalWeightJoy[["High"]] <- env$finalWeightJoy[["High"]] + 0.6
        env$finalCountJoy[["High"]] <- env$finalCountJoy[["High"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = matchLaughingHihiLists)
      } else if (isTRUE(isLaughOutLoudRegex)) {
        getlaughOutLoudRegex <- regexpr(pattern = laughOutLoudRegex, text = everyWord[j])
        matchLaughOutLoudLists <- regmatches(x = everyWord[j], m = getlaughOutLoudRegex) 
        env$finalCountReactions[["Joy"]] <- env$finalCountReactions[["Joy"]] + 1
        env$finalWeightReactions[["Joy"]] <- env$finalWeightReactions[["Joy"]] + 1
        env$finalWeightJoy[["Highest"]] <- env$finalWeightJoy[["Highest"]] + 1
        env$finalCountJoy[["Highest"]] <- env$finalCountJoy[["Highest"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = matchLaughOutLoudLists)
      } else if (isTRUE(isSuprisedRegex)) {
        getSuprisedRegex <- regexpr(pattern = suprisedRegex, text = everyWord[j])
        matchSuprisedLists <- regmatches(x = everyWord[j], m = getSuprisedRegex)
        env$finalCountReactions[["Joy"]] <- env$finalCountReactions[["Joy"]] + 1
        env$finalWeightReactions[["Joy"]] <- env$finalWeightReactions[["Joy"]] + 0.8
        env$finalWeightJoy[["Higher"]] <- env$finalWeightJoy[["Higher"]] + 0.8
        env$finalCountJoy[["Higher"]] <- env$finalCountJoy[["Higher"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = matchSuprisedLists)
      } else if (isTRUE(isCryingRegex)) {
        getCryingRegex <- regexpr(pattern = cryingRegex, text = everyWord[j])
        matchCryingLists <- regmatches(x = everyWord[j], m = getCryingRegex)
        env$finalCountReactions[["Sadness"]] <- env$finalCountReactions[["Sadness"]] + 1
        env$finalWeightReactions[["Sadness"]] <- env$finalWeightReactions[["Sadness"]] + 1
        env$finalWeightSadness[["Highest"]] <- env$finalWeightSadness[["Highest"]] + 1
        env$finalCountSadness[["Highest"]] <- env$finalCountSadness[["Highest"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = matchCryingLists)
      } else if (isTRUE(isAngryRegex)) {
        getAngryRegex <- regexpr(pattern = angryRegex, text = everyWord[j])
        matchAngryLists <- regmatches(x = everyWord[j], m = getAngryRegex)
        env$finalCountReactions[["Anger"]] <- env$finalCountReactions[["Anger"]] + 1
        env$finalWeightReactions[["Anger"]] <- env$finalWeightReactions[["Anger"]] + 1
        env$finalWeightAnger[["Highest"]] <- env$finalWeightAnger[["Highest"]] + 1
        env$finalCountAnger[["Highest"]] <- env$finalCountAnger[["Highest"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = matchAngryLists)
      } else if (isTRUE(isDisgustRegex)) {
        getDisgustRegex <- regexpr(pattern = disgustRegex, text = everyWord[j])
        matchDisgustLists <- regmatches(x = everyWord[j], m = getDisgustRegex)
        env$finalCountReactions[["Disgust"]] <- env$finalCountReactions[["Disgust"]] + 1
        env$finalWeightReactions[["Disgust"]] <- env$finalWeightReactions[["Disgust"]] + 1
        env$finalWeightDisgust[["Highest"]] <- env$finalWeightDisgust[["Highest"]] + 1
        env$finalCountDisgust[["Highest"]] <- env$finalCountDisgust[["Highest"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = matchDisgustLists)
      } else if (isTRUE(isScaredRegex)) {
        getScaredRegex <- regexpr(pattern = scaredRegex, text = everyWord[j])
        matchScaredLists <- regmatches(x = everyWord[j], m = getScaredRegex)
        env$finalCountReactions[["Fear"]] <- env$finalCountReactions[["Fear"]] + 1
        env$finalWeightReactions[["Fear"]] <- env$finalWeightReactions[["Fear"]] + 0.8
        env$finalWeightFear[["Highest"]] <- env$finalWeightFear[["Highest"]] + 0.8
        env$finalCountFear[["Highest"]] <- env$finalCountFear[["Highest"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = matchScaredLists)
      }
    }
  }
}