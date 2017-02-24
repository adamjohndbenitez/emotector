fear.FuzzyRules <- function(fearData, tokenizeWords0, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(fearData)) {
    isEnglishNegatives <- FALSE
    if (identical(tokenizeWords1, fearData[l, 1]) & !is.na(fearData[l, 1])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (n in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[n, 3]) & !is.na(fearData[n, 3])) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }

        if (identical(tokenizeWords2, fearData[n, 4]) & !is.na(fearData[n, 4])) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }

        if (identical(tokenizeWords2, fearData[n, 5]) & !is.na(fearData[n, 5])) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      break()
    }
    
    isEnglishLowDegree <- FALSE
    if (identical(tokenizeWords1, fearData[l, 2]) & !is.na(fearData[l, 2])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (m in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[m, 3]) & !is.na(fearData[m, 3])) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }

        if (identical(tokenizeWords2, fearData[m, 4]) & !is.na(fearData[m, 4])) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }

        if (identical(tokenizeWords2, fearData[m, 5]) & !is.na(fearData[m, 5])) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      break()
    }
    
    isEnglishIntensifier <- FALSE
    if (identical(tokenizeWords1, fearData[l, 6]) & !is.na(fearData[l, 6])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (o in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[o, 3]) & !is.na(fearData[o, 3])) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }

        if (identical(tokenizeWords2, fearData[o, 4]) & !is.na(fearData[o, 4])) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }

        if (identical(tokenizeWords2, fearData[o, 5]) & !is.na(fearData[o, 5])) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
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
    if (identical(tokenizeWords1, fearData[l, 3]) & !is.na(fearData[l, 3])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[a, 1]) & !is.na(fearData[a, 1])) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[b, 2]) & !is.na(fearData[b, 2])) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[b, 6]) & !is.na(fearData[b, 6])) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(fearData)) {
        if (identical(tokenizeWords0, fearData[d, 1]) & !is.na(fearData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(fearData)) {
        if (identical(tokenizeWords0, fearData[e, 2]) & !is.na(fearData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(fearData)) {
        if (identical(tokenizeWords0, fearData[f, 6]) & !is.na(fearData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountFear[["Neutral"]] <- env$tempCountFear[["Neutral"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
    
    if (identical(tokenizeWords1, fearData[l, 4]) & !is.na(fearData[l, 4])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[a, 1]) & !is.na(fearData[a, 1])) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[b, 2]) & !is.na(fearData[b, 2])) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[b, 6]) & !is.na(fearData[b, 6])) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(fearData)) {
        if (identical(tokenizeWords0, fearData[d, 1]) & !is.na(fearData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(fearData)) {
        if (identical(tokenizeWords0, fearData[e, 2]) & !is.na(fearData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(fearData)) {
        if (identical(tokenizeWords0, fearData[f, 6]) & !is.na(fearData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountFear[["High"]] <- env$tempCountFear[["High"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
    
    if (identical(tokenizeWords1, fearData[l, 5]) & !is.na(fearData[l, 5])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[a, 1]) & !is.na(fearData[a, 1])) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[b, 2]) & !is.na(fearData[b, 2])) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(fearData)) {
        if (identical(tokenizeWords2, fearData[b, 6]) & !is.na(fearData[b, 6])) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(fearData)) {
        if (identical(tokenizeWords0, fearData[d, 1]) & !is.na(fearData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(fearData)) {
        if (identical(tokenizeWords0, fearData[e, 2]) & !is.na(fearData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(fearData)) {
        if (identical(tokenizeWords0, fearData[f, 6]) & !is.na(fearData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountFear[["Higher"]] <- env$tempCountFear[["Higher"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
  }
  
}