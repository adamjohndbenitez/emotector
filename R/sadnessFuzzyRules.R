sadness.FuzzyRules <- function(sadnessData, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(sadnessData)) {
    if (is.na(sadnessData[l, 1])) { #trapping for NA values in excel
    } else if (tokenizeWords1 == sadnessData[l, 1]) { #look for negative contractions and connotations.
      for (n in 1:nrow(sadnessData)) {
        if (is.na(sadnessData[n, 3])) {
        } else if (tokenizeWords2 == sadnessData[n, 3]) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$tempCountSadness[["Neutral"]] <- env$tempCountSadness[["Neutral"]] - 1
          break()
        } # look up words in neutral column then add intensifier

        if (is.na(sadnessData[n, 4])) {
        } else if (tokenizeWords2 == sadnessData[n, 4]) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$tempCountSadness[["High"]] <- env$tempCountSadness[["High"]] - 1
          break()
        }

        if (is.na(sadnessData[n, 5])) {
        } else if (tokenizeWords2 == sadnessData[n, 5]) {
          env$tempCountSadness[["Lowest"]] <- env$tempCountSadness[["Lowest"]] + 1
          env$tempCountSadness[["Higher"]] <- env$tempCountSadness[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (is.na(sadnessData[l, 2])) { #trapping for NA values in excel
    } else if (tokenizeWords1 == sadnessData[l, 2]) { #look for medium words.
      for (m in 1:nrow(sadnessData)) {
        if (is.na(sadnessData[m, 3])) {
        } else if (tokenizeWords2 == sadnessData[m, 3]) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$tempCountSadness[["Neutral"]] <- env$tempCountSadness[["Neutral"]] - 1
          break()
        } # look up words in neutral column then add intensifier

        if (is.na(sadnessData[m, 4])) {
        } else if (tokenizeWords2 == sadnessData[m, 4]) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$tempCountSadness[["High"]] <- env$tempCountSadness[["High"]] - 1
          break()
        }

        if (is.na(sadnessData[m, 5])) {
        } else if (tokenizeWords2 == sadnessData[m, 5]) {
          env$tempCountSadness[["Low"]] <- env$tempCountSadness[["Low"]] + 1
          env$tempCountSadness[["Higher"]] <- env$tempCountSadness[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (is.na(sadnessData[l, 6])) { #trapping for NA values in excel
    } else if (tokenizeWords1 == sadnessData[l, 6]) { #look for intensifier
      for (o in 1:nrow(sadnessData)) {
        if (is.na(sadnessData[o, 3])) {
        } else if (tokenizeWords2 == sadnessData[o, 3]) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
          env$tempCountSadness[["Neutral"]] <- env$tempCountSadness[["Neutral"]] - 1
          break()
        } # look up words in neutral column then add intensifier

        if (is.na(sadnessData[o, 4])) {
        } else if (tokenizeWords2 == sadnessData[o, 4]) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
          env$tempCountSadness[["High"]] <- env$tempCountSadness[["High"]] - 1
          break()
        }

        if (is.na(sadnessData[o, 5])) {
        } else if (tokenizeWords2 == sadnessData[o, 5]) {
          env$tempCountSadness[["Highest"]] <- env$tempCountSadness[["Highest"]] + 1
          env$tempCountSadness[["Higher"]] <- env$tempCountSadness[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (is.na(sadnessData[l, 3])) {
    } else if (tokenizeWords1 == sadnessData[l, 3]) {
      env$tempCountSadness[["Neutral"]] <- env$tempCountSadness[["Neutral"]] + 1
      break()
    }
    
    if (is.na(sadnessData[l, 4])) {
    } else if (tokenizeWords1 == sadnessData[l, 4]) {
      env$tempCountSadness[["High"]] <- env$tempCountSadness[["High"]] + 1
      break()
    }
    
    if (is.na(sadnessData[l, 5])) {
    } else if (tokenizeWords1 == sadnessData[l, 5]) {
      env$tempCountSadness[["Higher"]] <- env$tempCountSadness[["Higher"]] + 1
      break()
    }
  }
}