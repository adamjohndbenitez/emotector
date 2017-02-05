disgust.FuzzyRules <- function(disgustData, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(disgustData)) {
    if (is.na(disgustData[l, 1])) { #trapping for NA values in excel
    } else if (tokenizeWords1 == disgustData[l, 1]) { #look for negative contractions and connotations.
      for (n in 1:nrow(disgustData)) {
        if (is.na(disgustData[n, 3])) {
        } else if (tokenizeWords2 == disgustData[n, 3]) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          env$tempCountDisgust[["Neutral"]] <- env$tempCountDisgust[["Neutral"]] - 1
          break()
        } # look up words in neutral column then add intensifier

        if (is.na(disgustData[n, 4])) {
        } else if (tokenizeWords2 == disgustData[n, 4]) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          env$tempCountDisgust[["High"]] <- env$tempCountDisgust[["High"]] - 1
          break()
        }

        if (is.na(disgustData[n, 5])) {
        } else if (tokenizeWords2 == disgustData[n, 5]) {
          env$tempCountDisgust[["Lowest"]] <- env$tempCountDisgust[["Lowest"]] + 1
          env$tempCountDisgust[["Higher"]] <- env$tempCountDisgust[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (is.na(disgustData[l, 2])) { #trapping for NA values in excel
    } else if (tokenizeWords1 == disgustData[l, 2]) { #look for medium words.
      for (m in 1:nrow(disgustData)) {
        if (is.na(disgustData[m, 3])) {
        } else if (tokenizeWords2 == disgustData[m, 3]) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          env$tempCountDisgust[["Neutral"]] <- env$tempCountDisgust[["Neutral"]] - 1
          break()
        } # look up words in neutral column then add intensifier

        if (is.na(disgustData[m, 4])) {
        } else if (tokenizeWords2 == disgustData[m, 4]) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          env$tempCountDisgust[["High"]] <- env$tempCountDisgust[["High"]] - 1
          break()
        }

        if (is.na(disgustData[m, 5])) {
        } else if (tokenizeWords2 == disgustData[m, 5]) {
          env$tempCountDisgust[["Low"]] <- env$tempCountDisgust[["Low"]] + 1
          env$tempCountDisgust[["Higher"]] <- env$tempCountDisgust[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (is.na(disgustData[l, 6])) { #trapping for NA values in excel
    } else if (tokenizeWords1 == disgustData[l, 6]) { #look for intensifier
      for (o in 1:nrow(disgustData)) {
        if (is.na(disgustData[o, 3])) {
        } else if (tokenizeWords2 == disgustData[o, 3]) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$tempCountDisgust[["Neutral"]] <- env$tempCountDisgust[["Neutral"]] - 1
          break()
        } # look up words in neutral column then add intensifier

        if (is.na(disgustData[o, 4])) {
        } else if (tokenizeWords2 == disgustData[o, 4]) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$tempCountDisgust[["High"]] <- env$tempCountDisgust[["High"]] - 1
          break()
        }

        if (is.na(disgustData[o, 5])) {
        } else if (tokenizeWords2 == disgustData[o, 5]) {
          env$tempCountDisgust[["Highest"]] <- env$tempCountDisgust[["Highest"]] + 1
          env$tempCountDisgust[["Higher"]] <- env$tempCountDisgust[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (is.na(disgustData[l, 3])) {
    } else if (tokenizeWords1 == disgustData[l, 3]) {
      env$tempCountDisgust[["Neutral"]] <- env$tempCountDisgust[["Neutral"]] + 1
      break()
    }
    
    if (is.na(disgustData[l, 4])) {
    } else if (tokenizeWords1 == disgustData[l, 4]) {
      env$tempCountDisgust[["High"]] <- env$tempCountDisgust[["High"]] + 1
      break()
    }
    
    if (is.na(disgustData[l, 5])) {
    } else if (tokenizeWords1 == disgustData[l, 5]) {
      env$tempCountDisgust[["Higher"]] <- env$tempCountDisgust[["Higher"]] + 1
      break()
    }
  }
}