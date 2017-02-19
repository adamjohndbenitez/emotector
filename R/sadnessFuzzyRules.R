sadness.FuzzyRules <- function(sadnessData, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(sadnessData)) {
    if (identical(tokenizeWords1, sadnessData[l, 1])) {
      for (n in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[n, 3])) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$tempCountSadness[["Neutral"]] <- env$tempCountSadness[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, sadnessData[n, 4])) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$tempCountSadness[["High"]] <- env$tempCountSadness[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, sadnessData[n, 5])) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$tempCountSadness[["Higher"]] <- env$tempCountSadness[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, sadnessData[l, 2])) {
      for (m in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[m, 3])) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$tempCountSadness[["Neutral"]] <- env$tempCountSadness[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        } # look up words in neutral column then add intensifier

        if (identical(tokenizeWords2, sadnessData[m, 4])) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$tempCountSadness[["High"]] <- env$tempCountSadness[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, sadnessData[m, 5])) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$tempCountSadness[["Higher"]] <- env$tempCountSadness[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, sadnessData[l, 6])) {
      for (o in 1:nrow(sadnessData)) {
        if (identical(tokenizeWords2, sadnessData[o, 3])) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
          env$tempCountSadness[["Neutral"]] <- env$tempCountSadness[["Neutral"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        } # look up words in neutral column then add intensifier

        if (identical(tokenizeWords2, sadnessData[o, 4])) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
          env$tempCountSadness[["High"]] <- env$tempCountSadness[["High"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }

        if (identical(tokenizeWords2, sadnessData[o, 5])) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
          env$tempCountSadness[["Higher"]] <- env$tempCountSadness[["Higher"]] - 1
          env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords2)
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, sadnessData[l, 3])) {
      env$tempCountSadness[["Neutral"]] <- env$tempCountSadness[["Neutral"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
    
    if (identical(tokenizeWords1, sadnessData[l, 4])) {
      env$tempCountSadness[["High"]] <- env$tempCountSadness[["High"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
    
    if (identical(tokenizeWords1, sadnessData[l, 5])) {
      env$tempCountSadness[["Higher"]] <- env$tempCountSadness[["Higher"]] + 1
      env$detectedWordsGathered <- append(x = env$detectedWordsGathered, values = tokenizeWords1)
      break()
    }
  }
}