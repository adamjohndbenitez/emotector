reactions.FuzzyRules <- function(analyzePostAndItsComments, env = parent.frame()) {
  laughingRegex <- "\\b(?:a*A*e*E*i*I*(?:ha*|HA*|aA*|Aa*|he*|HE*|eE*|Ee*|hi*|HI*|iI*|Ii*)+H*h*?)\\b"
  laughOutLoudRegex <- "\\b((?:lo*|lO*|Lo*|LO*|oO*|Oo*)+l*L*?)\\b"
  suprisedRegex <- "\\b(?:w*W*(?:wo*|wO*|Wo*|WO*|oO*|Oo*)+w*W*?)\\b"
  cryingRegex <- "\\b(?:u*U*(?:hu*|hU*|Hu*|HU*|uU*|Uu*)+H*h*?)\\b"
  angryRegex <- "\\b(?:g*G*g*(?:rR*|Rr*)+R?r?)\\b"
  disgustRegex <- "\\b(?:e*E*e*(?:wW*|Ww*)+W?w?)\\b"
  scaredRegex <- "\\b(?:w*W*(?:wa*|wA*|Wa*|WA*|aA*|Aa*)+h*H*?)\\b"
  
  for (i in 1:length(analyzePostAndItsComments)) {
	# everyWord <- tokenizers::tokenize_words(x = analyzePostAndItsComments[i], lowercase = TRUE, stopwords = NULL, simplify = TRUE)
    everyWord <- tokenizers::tokenize_regex(x = analyzePostAndItsComments[i], pattern = "\\s+", simplify = TRUE)
    
    for (j in 1:length(everyWord)) {
      isLaughingRegex <- grepl(pattern = laughingRegex, x = everyWord[j])
      isLaughOutLoudRegex <- grepl(pattern = laughOutLoudRegex, x = everyWord[j])
      isSuprisedRegex <- grepl(pattern = suprisedRegex, x = everyWord[j])
      isCryingRegex <- grepl(pattern = cryingRegex, x = everyWord[j])
      isAngryRegex <- grepl(pattern = angryRegex, x = everyWord[j])
      isDisgustRegex <- grepl(pattern = disgustRegex, x = everyWord[j])
      isScaredRegex <- grepl(pattern = scaredRegex, x = everyWord[j])
      
      if (isTRUE(isLaughingRegex)) {
        getLaughingRegex <- regexpr(pattern = laughingRegex, text = everyWord[j])
        matchLaughingLists <- regmatches(x = everyWord[j], m = getLaughingRegex)
        env$finalCountReactions[["Joy"]] <- env$finalCountReactions[["Joy"]] + 1
        env$finalWeightReactions[["Joy"]] <- env$finalWeightReactions[["Joy"]] + 0.6
        env$finalWeightJoy[["High"]] <- env$finalWeightJoy[["High"]] + 0.6
        env$finalCountJoy[["High"]] <- env$finalCountJoy[["High"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = matchLaughingLists)
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