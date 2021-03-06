disgust.FuzzyRules <- function(disgustData, tokenizeWords0, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(disgustData)) {
    isEnglishNegatives <- FALSE
    if (identical(tokenizeWords1, disgustData[l, 1]) & !is.na(disgustData[l, 1])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (n in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[n, 3]) & !is.na(disgustData[n, 3])) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }

        if (identical(tokenizeWords2, disgustData[n, 4]) & !is.na(disgustData[n, 4])) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }

        if (identical(tokenizeWords2, disgustData[n, 5]) & !is.na(disgustData[n, 5])) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      break()
    }
    
    isEnglishLowDegree <- FALSE
    if (identical(tokenizeWords1, disgustData[l, 2]) & !is.na(disgustData[l, 2])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (m in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[m, 3]) & !is.na(disgustData[m, 3])) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }

        if (identical(tokenizeWords2, disgustData[m, 4]) & !is.na(disgustData[m, 4])) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }

        if (identical(tokenizeWords2, disgustData[m, 5]) & !is.na(disgustData[m, 5])) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      break()
    }
    
    isEnglishIntensifier <- FALSE
    if (identical(tokenizeWords1, disgustData[l, 6]) & !is.na(disgustData[l, 6])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (o in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[o, 3]) & !is.na(disgustData[o, 3])) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }

        if (identical(tokenizeWords2, disgustData[o, 4]) & !is.na(disgustData[o, 4])) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }

        if (identical(tokenizeWords2, disgustData[o, 5]) & !is.na(disgustData[o, 5])) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      break()
    }
    
    isBisayaIntensifier <- FALSE
    isBisayaNegatives <- FALSE
    isBisayaLowDegree <- FALSE
    if (identical(tokenizeWords1, disgustData[l, 3]) & !is.na(disgustData[l, 3])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[a, 1]) & !is.na(disgustData[a, 1])) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[b, 2]) & !is.na(disgustData[b, 2])) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[c, 6]) & !is.na(disgustData[c, 6])) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(disgustData)) {
        if (identical(tokenizeWords0, disgustData[d, 1]) & !is.na(disgustData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(disgustData)) {
        if (identical(tokenizeWords0, disgustData[e, 2]) & !is.na(disgustData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(disgustData)) {
        if (identical(tokenizeWords0, disgustData[f, 6]) & !is.na(disgustData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountDisgust[["Neutral"]] <- env$tempCountDisgust[["Neutral"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
    
    if (identical(tokenizeWords1, disgustData[l, 4]) & !is.na(disgustData[l, 4])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[a, 1]) & !is.na(disgustData[a, 1])) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[b, 2]) & !is.na(disgustData[b, 2])) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[c, 6]) & !is.na(disgustData[c, 6])) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(disgustData)) {
        if (identical(tokenizeWords0, disgustData[d, 1]) & !is.na(disgustData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(disgustData)) {
        if (identical(tokenizeWords0, disgustData[e, 2]) & !is.na(disgustData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(disgustData)) {
        if (identical(tokenizeWords0, disgustData[f, 6]) & !is.na(disgustData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountDisgust[["High"]] <- env$tempCountDisgust[["High"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
    
    if (identical(tokenizeWords1, disgustData[l, 5]) & !is.na(disgustData[l, 5])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[a, 1]) & !is.na(disgustData[a, 1])) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[b, 2]) & !is.na(disgustData[b, 2])) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(disgustData)) {
        if (identical(tokenizeWords2, disgustData[c, 6]) & !is.na(disgustData[c, 6])) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(disgustData)) {
        if (identical(tokenizeWords0, disgustData[d, 1]) & !is.na(disgustData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(disgustData)) {
        if (identical(tokenizeWords0, disgustData[e, 2]) & !is.na(disgustData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(disgustData)) {
        if (identical(tokenizeWords0, disgustData[f, 6]) & !is.na(disgustData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountDisgust[["Higher"]] <- env$tempCountDisgust[["Higher"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
  }
  
}