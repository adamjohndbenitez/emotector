sadness.FuzzyRules <- function(sadnessData, tokenizeWords0, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(sadnessData)) {
    isEnglishNegatives <- FALSE
    if (identical(tokenizeWords1, sadnessData[l, 1]) & !is.na(sadnessData[l, 1])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (n in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[n, 3]) & !is.na(sadnessData[n, 3])) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }

        if (identical(tokenizeWords2, sadnessData[n, 4]) & !is.na(sadnessData[n, 4])) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }

        if (identical(tokenizeWords2, sadnessData[n, 5]) & !is.na(sadnessData[n, 5])) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      break()
    }
    
    isEnglishLowDegree <- FALSE
    if (identical(tokenizeWords1, sadnessData[l, 2]) & !is.na(sadnessData[l, 2])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (m in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[m, 3]) & !is.na(sadnessData[m, 3])) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }

        if (identical(tokenizeWords2, sadnessData[m, 4]) & !is.na(sadnessData[m, 4])) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }

        if (identical(tokenizeWords2, sadnessData[m, 5]) & !is.na(sadnessData[m, 5])) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      break()
    }
    
    isEnglishIntensifier <- FALSE
    if (identical(tokenizeWords1, sadnessData[l, 6]) & !is.na(sadnessData[l, 6])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (o in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[o, 3]) & !is.na(sadnessData[o, 3])) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }

        if (identical(tokenizeWords2, sadnessData[o, 4]) & !is.na(sadnessData[o, 4])) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishIntensifier <- TRUE
          break()
        }

        if (identical(tokenizeWords2, sadnessData[o, 5]) & !is.na(sadnessData[o, 5])) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
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
    if (identical(tokenizeWords1, sadnessData[l, 3]) & !is.na(sadnessData[l, 3])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[a, 1]) & !is.na(sadnessData[a, 1])) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[b, 2]) & !is.na(sadnessData[b, 2])) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[c, 6]) & !is.na(sadnessData[c, 6])) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords0, sadnessData[d, 1]) & !is.na(sadnessData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords0, sadnessData[e, 2]) & !is.na(sadnessData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords0, sadnessData[f, 6]) & !is.na(sadnessData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountSadness[["Neutral"]] <- env$tempCountSadness[["Neutral"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
    
    if (identical(tokenizeWords1, sadnessData[l, 4]) & !is.na(sadnessData[l, 4])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[a, 1]) & !is.na(sadnessData[a, 1])) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[b, 2]) & !is.na(sadnessData[b, 2])) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[c, 6]) & !is.na(sadnessData[c, 6])) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords0, sadnessData[d, 1]) & !is.na(sadnessData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords0, sadnessData[e, 2]) & !is.na(sadnessData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords0, sadnessData[f, 6]) & !is.na(sadnessData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountSadness[["High"]] <- env$tempCountSadness[["High"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
    
    if (identical(tokenizeWords1, sadnessData[l, 5]) & !is.na(sadnessData[l, 5])) {
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      for (a in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[a, 1]) & !is.na(sadnessData[a, 1])) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaNegatives <- TRUE
          break()
        }
      }
      
      for (b in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[b, 2]) & !is.na(sadnessData[b, 2])) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      
      for (c in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[c, 6]) & !is.na(sadnessData[c, 6])) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          isBisayaIntensifier <- TRUE
          break()
        } 
      }
      
      for (d in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords0, sadnessData[d, 1]) & !is.na(sadnessData[d, 1])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishNegatives <- TRUE
          break()
        }
      }
      for (e in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords0, sadnessData[e, 2]) & !is.na(sadnessData[e, 2])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishLowDegree <- TRUE
          break()
        }
      }
      for (f in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords0, sadnessData[f, 6]) & !is.na(sadnessData[f, 6])) {
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords0)
          isEnglishIntensifier <- TRUE
          break()
        }
      }
      
      if ((!isBisayaNegatives & !isEnglishNegatives) & (!isBisayaLowDegree & !isEnglishLowDegree) & (!isBisayaIntensifier & !isEnglishIntensifier)) {
        env$tempCountSadness[["Higher"]] <- env$tempCountSadness[["Higher"]] + 1
        env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
        break()
      }
      break()
    }
  }
  
}