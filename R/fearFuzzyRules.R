fear.FuzzyRules <- function(fearData, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(fearData)) {
    if (identical(tokenizeWords1, fearData[l, 1])) {
      for (n in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[n, 3])) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$tempCountFear[["Neutral"]] <- env$tempCountFear[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, fearData[n, 4])) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$tempCountFear[["High"]] <- env$tempCountFear[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, fearData[n, 5])) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$tempCountFear[["Higher"]] <- env$tempCountFear[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, fearData[l, 2])) {
      for (m in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[m, 3])) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$tempCountFear[["Neutral"]] <- env$tempCountFear[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, fearData[m, 4])) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$tempCountFear[["High"]] <- env$tempCountFear[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, fearData[m, 5])) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$tempCountFear[["Higher"]] <- env$tempCountFear[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, fearData[l, 6])) {
      for (o in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[o, 3])) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
          env$tempCountFear[["Neutral"]] <- env$tempCountFear[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        } # look up words in neutral column then add intensifier

        if (identical(tokenizeWords2, fearData[o, 4])) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
          env$tempCountFear[["High"]] <- env$tempCountFear[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, fearData[o, 5])) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
          env$tempCountFear[["Higher"]] <- env$tempCountFear[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, fearData[l, 3])) {
      env$tempCountFear[["Neutral"]] <- env$tempCountFear[["Neutral"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
    
    if (identical(tokenizeWords1, fearData[l, 4])) {
      env$tempCountFear[["High"]] <- env$tempCountFear[["High"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
    
    if (identical(tokenizeWords1, fearData[l, 5])) {
      env$tempCountFear[["Higher"]] <- env$tempCountFear[["Higher"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
  }
}