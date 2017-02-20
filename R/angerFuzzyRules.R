anger.FuzzyRules <- function(angerData, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(angerData)) {
    if (identical(tokenizeWords1, angerData[l, 1])) { #look for negative contractions and connotations.
      for (n in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[n, 3])) {
          env$tempCountAnger[["Lowest"]] <- env$tempCountAnger[["Lowest"]] + 1
          env$tempCountAnger[["Neutral"]] <- env$tempCountAnger[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        } # look up words in neutral column then add intensifier

        if (identical(tokenizeWords2, angerData[n, 4])) {
          env$tempCountAnger[["Lowest"]] <- env$tempCountAnger[["Lowest"]] + 1
          env$tempCountAnger[["High"]] <- env$tempCountAnger[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, angerData[n, 5])) {
          env$tempCountAnger[["Lowest"]] <- env$tempCountAnger[["Lowest"]] + 1
          # env$tempCountAnger[["Higher"]] <- env$tempCountAnger[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, angerData[l, 2])) { #look for medium words.
      for (m in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[m, 3])) {
          env$tempCountAnger[["Low"]] <- env$tempCountAnger[["Low"]] + 1
          env$tempCountAnger[["Neutral"]] <- env$tempCountAnger[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        } # look up words in neutral column then add intensifier

        if (identical(tokenizeWords2, angerData[m, 4])) {
          env$tempCountAnger[["Low"]] <- env$tempCountAnger[["Low"]] + 1
          env$tempCountAnger[["High"]] <- env$tempCountAnger[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, angerData[m, 5])) {
          env$tempCountAnger[["Low"]] <- env$tempCountAnger[["Low"]] + 1
          # env$tempCountAnger[["Higher"]] <- env$tempCountAnger[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, angerData[l, 6])) { #look for intensifier
      for (o in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[o, 3])) {
          env$tempCountAnger[["Highest"]] <- env$tempCountAnger[["Highest"]] + 1
          env$tempCountAnger[["Neutral"]] <- env$tempCountAnger[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        } # look up words in neutral column then add intensifier

        if (identical(tokenizeWords2, angerData[o, 4])) {
          env$tempCountAnger[["Highest"]] <- env$tempCountAnger[["Highest"]] + 1
          env$tempCountAnger[["High"]] <- env$tempCountAnger[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, angerData[o, 5])) {
          env$tempCountAnger[["Highest"]] <- env$tempCountAnger[["Highest"]] + 1
          # env$tempCountAnger[["Higher"]] <- env$tempCountAnger[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, angerData[l, 3])) {
      env$tempCountAnger[["Neutral"]] <- env$tempCountAnger[["Neutral"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
    
    if (identical(tokenizeWords1, angerData[l, 4])) {
      env$tempCountAnger[["High"]] <- env$tempCountAnger[["High"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
    
    if (identical(tokenizeWords1, angerData[l, 5])) {
      env$tempCountAnger[["Higher"]] <- env$tempCountAnger[["Higher"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
  }
}