disgust.FuzzyRules <- function(disgustData, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(disgustData)) {
    if (identical(tokenizeWords1, disgustData[l, 1]) & !is.na(disgustData[l, 1])) {
      for (n in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[n, 3]) & !is.na(disgustData[n, 3])) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          env$tempCountDisgust[["Neutral"]] <- env$tempCountDisgust[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, disgustData[n, 4]) & !is.na(disgustData[n, 4])) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          env$tempCountDisgust[["High"]] <- env$tempCountDisgust[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, disgustData[n, 5]) & !is.na(disgustData[n, 5])) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          # env$tempCountDisgust[["Higher"]] <- env$tempCountDisgust[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, disgustData[l, 2]) & !is.na(disgustData[l, 2])) {
      for (m in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[m, 3]) & !is.na(disgustData[m, 3])) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          env$tempCountDisgust[["Neutral"]] <- env$tempCountDisgust[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, disgustData[m, 4]) & !is.na(disgustData[m, 4])) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          env$tempCountDisgust[["High"]] <- env$tempCountDisgust[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, disgustData[m, 5]) & !is.na(disgustData[m, 5])) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          # env$tempCountDisgust[["Higher"]] <- env$tempCountDisgust[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    isEnglishIntensifier <- FALSE
    if (identical(tokenizeWords1, disgustData[l, 6]) & !is.na(disgustData[l, 6])) { #look for intensifier
      for (o in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[o, 3]) & !is.na(disgustData[o, 3])) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$tempCountDisgust[["Neutral"]] <- env$tempCountDisgust[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }

        if (identical(tokenizeWords2, disgustData[o, 4]) & !is.na(disgustData[o, 4])) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$tempCountDisgust[["High"]] <- env$tempCountDisgust[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, disgustData[o, 5]) & !is.na(disgustData[o, 5])) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          # env$tempCountDisgust[["Higher"]] <- env$tempCountDisgust[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    isBisayaIntensifier <- FALSE
    if (identical(tokenizeWords1, disgustData[l, 3]) & !is.na(disgustData[l, 3])) {
      for (b in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[b, 6]) & !is.na(disgustData[b, 6])) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        }
      }
    }
    
    if (identical(tokenizeWords1, disgustData[l, 3]) & !is.na(disgustData[l, 3]) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
      env$tempCountDisgust[["Neutral"]] <- env$tempCountDisgust[["Neutral"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
    
    if (identical(tokenizeWords1, disgustData[l, 4]) & !is.na(disgustData[l, 4])) {
      env$tempCountDisgust[["High"]] <- env$tempCountDisgust[["High"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
    
    if (identical(tokenizeWords1, disgustData[l, 5]) & !is.na(disgustData[l, 5])) {
      env$tempCountDisgust[["Higher"]] <- env$tempCountDisgust[["Higher"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
  }
}