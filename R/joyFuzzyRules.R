joy.FuzzyRules <- function(joyData, tokenizeWords0, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(joyData)) {
    isEnglishNegatives <- FALSE
    if (identical(tokenizeWords1, joyData[l, 1]) & !is.na(joyData[l, 1])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (n in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[n, 3]) & !is.na(joyData[n, 3])) {
          env$tempCountJoy[["Lowest"]] <- env$tempCountJoy[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }

        if (identical(tokenizeWords2, joyData[n, 4]) & !is.na(joyData[n, 4])) {
          env$tempCountJoy[["Lowest"]] <- env$tempCountJoy[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }

        if (identical(tokenizeWords2, joyData[n, 5]) & !is.na(joyData[n, 5])) {
          env$tempCountJoy[["Lowest"]] <- env$tempCountJoy[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      break()
    }
    
    isEnglishLowDegree <- FALSE
    if (identical(tokenizeWords1, joyData[l, 2]) & !is.na(joyData[l, 2])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (m in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[m, 3]) & !is.na(joyData[m, 3])) {
          env$tempCountJoy[["Low"]] <- env$tempCountJoy[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }

        if (identical(tokenizeWords2, joyData[m, 4]) & !is.na(joyData[m, 4])) {
          env$tempCountJoy[["Low"]] <- env$tempCountJoy[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }

        if (identical(tokenizeWords2, joyData[m, 5]) & !is.na(joyData[m, 5])) {
          env$tempCountJoy[["Low"]] <- env$tempCountJoy[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      break()
    }
    
    isEnglishIntensifier <- FALSE
    if (identical(tokenizeWords1, joyData[l, 6]) & !is.na(joyData[l, 6])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (o in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[o, 3]) & !is.na(joyData[o, 3])) {
          env$tempCountJoy[["Highest"]] <- env$tempCountJoy[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }

        if (identical(tokenizeWords2, joyData[o, 4]) & !is.na(joyData[o, 4])) {
          env$tempCountJoy[["Highest"]] <- env$tempCountJoy[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }

        if (identical(tokenizeWords2, joyData[o, 5]) & !is.na(joyData[o, 5])) {
          env$tempCountJoy[["Highest"]] <- env$tempCountJoy[["Highest"]] + 1
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
    if (identical(tokenizeWords1, joyData[l, 3]) & !is.na(joyData[l, 3])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[a, 1]) & !is.na(joyData[a, 1])) {
          env$tempCountJoy[["Lowest"]] <- env$tempCountJoy[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[b, 2]) & !is.na(joyData[b, 2])) {
          env$tempCountJoy[["Low"]] <- env$tempCountJoy[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[b, 6]) & !is.na(joyData[b, 6])) {
          env$tempCountJoy[["Highest"]] <- env$tempCountJoy[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(joyData)) {
        if (identical(tokenizeWords0, joyData[d, 1]) & !is.na(joyData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(joyData)) {
        if (identical(tokenizeWords0, joyData[e, 2]) & !is.na(joyData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(joyData)) {
        if (identical(tokenizeWords0, joyData[f, 6]) & !is.na(joyData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountJoy[["Neutral"]] <- env$tempCountJoy[["Neutral"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
    
    if (identical(tokenizeWords1, joyData[l, 4]) & !is.na(joyData[l, 4])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[a, 1]) & !is.na(joyData[a, 1])) {
          env$tempCountJoy[["Lowest"]] <- env$tempCountJoy[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[b, 2]) & !is.na(joyData[b, 2])) {
          env$tempCountJoy[["Low"]] <- env$tempCountJoy[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[b, 6]) & !is.na(joyData[b, 6])) {
          env$tempCountJoy[["Highest"]] <- env$tempCountJoy[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(joyData)) {
        if (identical(tokenizeWords0, joyData[d, 1]) & !is.na(joyData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(joyData)) {
        if (identical(tokenizeWords0, joyData[e, 2]) & !is.na(joyData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(joyData)) {
        if (identical(tokenizeWords0, joyData[f, 6]) & !is.na(joyData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountJoy[["High"]] <- env$tempCountJoy[["High"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
    
    if (identical(tokenizeWords1, joyData[l, 5]) & !is.na(joyData[l, 5])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[a, 1]) & !is.na(joyData[a, 1])) {
          env$tempCountJoy[["Lowest"]] <- env$tempCountJoy[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[b, 2]) & !is.na(joyData[b, 2])) {
          env$tempCountJoy[["Low"]] <- env$tempCountJoy[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[b, 6]) & !is.na(joyData[b, 6])) {
          env$tempCountJoy[["Highest"]] <- env$tempCountJoy[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(joyData)) {
        if (identical(tokenizeWords0, joyData[d, 1]) & !is.na(joyData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(joyData)) {
        if (identical(tokenizeWords0, joyData[e, 2]) & !is.na(joyData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(joyData)) {
        if (identical(tokenizeWords0, joyData[f, 6]) & !is.na(joyData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountJoy[["Higher"]] <- env$tempCountJoy[["Higher"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
  }
  
}