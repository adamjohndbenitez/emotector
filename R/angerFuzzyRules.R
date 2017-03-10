anger.FuzzyRules <- function(angerData, tokenizeWords0, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(angerData)) {
    isEnglishNegatives <- FALSE
    if (identical(tokenizeWords1, angerData[l, 1]) & !is.na(angerData[l, 1])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (n in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[n, 3]) & !is.na(angerData[n, 3])) {
          env$tempCountAnger[["Lowest"]] <- env$tempCountAnger[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }

        if (identical(tokenizeWords2, angerData[n, 4]) & !is.na(angerData[n, 4])) {
          env$tempCountAnger[["Lowest"]] <- env$tempCountAnger[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }

        if (identical(tokenizeWords2, angerData[n, 5]) & !is.na(angerData[n, 5])) {
          env$tempCountAnger[["Lowest"]] <- env$tempCountAnger[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      break()
    }
    
    isEnglishLowDegree <- FALSE
    if (identical(tokenizeWords1, angerData[l, 2]) & !is.na(angerData[l, 2])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (m in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[m, 3]) & !is.na(angerData[m, 3])) {
          env$tempCountAnger[["Low"]] <- env$tempCountAnger[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }

        if (identical(tokenizeWords2, angerData[m, 4]) & !is.na(angerData[m, 4])) {
          env$tempCountAnger[["Low"]] <- env$tempCountAnger[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }

        if (identical(tokenizeWords2, angerData[m, 5]) & !is.na(angerData[m, 5])) {
          env$tempCountAnger[["Low"]] <- env$tempCountAnger[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      break()
    }
    
    isEnglishIntensifier <- FALSE
    if (identical(tokenizeWords1, angerData[l, 6]) & !is.na(angerData[l, 6])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (o in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[o, 3]) & !is.na(angerData[o, 3])) {
          env$tempCountAnger[["Highest"]] <- env$tempCountAnger[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }

        if (identical(tokenizeWords2, angerData[o, 4]) & !is.na(angerData[o, 4])) {
          env$tempCountAnger[["Highest"]] <- env$tempCountAnger[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }

        if (identical(tokenizeWords2, angerData[o, 5]) & !is.na(angerData[o, 5])) {
          env$tempCountAnger[["Highest"]] <- env$tempCountAnger[["Highest"]] + 1
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
    if (identical(tokenizeWords1, angerData[l, 3]) & !is.na(angerData[l, 3])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[a, 1]) & !is.na(angerData[a, 1])) {
          env$tempCountAnger[["Lowest"]] <- env$tempCountAnger[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[b, 2]) & !is.na(angerData[b, 2])) {
          env$tempCountAnger[["Low"]] <- env$tempCountAnger[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[c, 6]) & !is.na(angerData[c, 6])) {
          env$tempCountAnger[["Highest"]] <- env$tempCountAnger[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(angerData)) {
        if (identical(tokenizeWords0, angerData[d, 1]) & !is.na(angerData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(angerData)) {
        if (identical(tokenizeWords0, angerData[e, 2]) & !is.na(angerData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(angerData)) {
        if (identical(tokenizeWords0, angerData[f, 6]) & !is.na(angerData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountAnger[["Neutral"]] <- env$tempCountAnger[["Neutral"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
    
    if (identical(tokenizeWords1, angerData[l, 4]) & !is.na(angerData[l, 4])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[a, 1]) & !is.na(angerData[a, 1])) {
          env$tempCountAnger[["Lowest"]] <- env$tempCountAnger[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[b, 2]) & !is.na(angerData[b, 2])) {
          env$tempCountAnger[["Low"]] <- env$tempCountAnger[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[c, 6]) & !is.na(angerData[c, 6])) {
          env$tempCountAnger[["Highest"]] <- env$tempCountAnger[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(angerData)) {
        if (identical(tokenizeWords0, angerData[d, 1]) & !is.na(angerData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(angerData)) {
        if (identical(tokenizeWords0, angerData[e, 2]) & !is.na(angerData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(angerData)) {
        if (identical(tokenizeWords0, angerData[f, 6]) & !is.na(angerData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountAnger[["High"]] <- env$tempCountAnger[["High"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
    
    if (identical(tokenizeWords1, angerData[l, 5]) & !is.na(angerData[l, 5])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[a, 1]) & !is.na(angerData[a, 1])) {
          env$tempCountAnger[["Lowest"]] <- env$tempCountAnger[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[b, 2]) & !is.na(angerData[b, 2])) {
          env$tempCountAnger[["Low"]] <- env$tempCountAnger[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(angerData)) {
        if (identical(tokenizeWords2, angerData[c, 6]) & !is.na(angerData[c, 6])) {
          env$tempCountAnger[["Highest"]] <- env$tempCountAnger[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(angerData)) {
        if (identical(tokenizeWords0, angerData[d, 1]) & !is.na(angerData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(angerData)) {
        if (identical(tokenizeWords0, angerData[e, 2]) & !is.na(angerData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(angerData)) {
        if (identical(tokenizeWords0, angerData[f, 6]) & !is.na(angerData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountAnger[["Higher"]] <- env$tempCountAnger[["Higher"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
  }
  
}