fear.FuzzyRules <- function(fearData, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(fearData)) {
    if (is.na(fearData[l, 1])) { #trapping for NA values in excel
    } else if (tokenizeWords1 == fearData[l, 1]) { #look for negative contractions and connotations.
      for (n in 1:nrow(fearData)) {
        if (is.na(fearData[n, 3])) {
        } else if (tokenizeWords2 == fearData[n, 3]) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$tempCountFear[["Neutral"]] <- env$tempCountFear[["Neutral"]] - 1
          break()
        } # look up words in neutral column then add intensifier

        if (is.na(fearData[n, 4])) {
        } else if (tokenizeWords2 == fearData[n, 4]) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$tempCountFear[["High"]] <- env$tempCountFear[["High"]] - 1
          break()
        }

        if (is.na(fearData[n, 5])) {
        } else if (tokenizeWords2 == fearData[n, 5]) {
          env$tempCountFear[["Lowest"]] <- env$tempCountFear[["Lowest"]] + 1
          env$tempCountFear[["Higher"]] <- env$tempCountFear[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (is.na(fearData[l, 2])) { #trapping for NA values in excel
    } else if (tokenizeWords1 == fearData[l, 2]) { #look for medium words.
      for (m in 1:nrow(fearData)) {
        if (is.na(fearData[m, 3])) {
        } else if (tokenizeWords2 == fearData[m, 3]) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$tempCountFear[["Neutral"]] <- env$tempCountFear[["Neutral"]] - 1
          break()
        } # look up words in neutral column then add intensifier

        if (is.na(fearData[m, 4])) {
        } else if (tokenizeWords2 == fearData[m, 4]) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$tempCountFear[["High"]] <- env$tempCountFear[["High"]] - 1
          break()
        }

        if (is.na(fearData[m, 5])) {
        } else if (tokenizeWords2 == fearData[m, 5]) {
          env$tempCountFear[["Low"]] <- env$tempCountFear[["Low"]] + 1
          env$tempCountFear[["Higher"]] <- env$tempCountFear[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (is.na(fearData[l, 6])) { #trapping for NA values in excel
    } else if (tokenizeWords1 == fearData[l, 6]) { #look for intensifier
      for (o in 1:nrow(fearData)) {
        if (is.na(fearData[o, 3])) {
        } else if (tokenizeWords2 == fearData[o, 3]) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
          env$tempCountFear[["Neutral"]] <- env$tempCountFear[["Neutral"]] - 1
          break()
        } # look up words in neutral column then add intensifier

        if (is.na(fearData[o, 4])) {
        } else if (tokenizeWords2 == fearData[o, 4]) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
          env$tempCountFear[["High"]] <- env$tempCountFear[["High"]] - 1
          break()
        }

        if (is.na(fearData[o, 5])) {
        } else if (tokenizeWords2 == fearData[o, 5]) {
          env$tempCountFear[["Highest"]] <- env$tempCountFear[["Highest"]] + 1
          env$tempCountFear[["Higher"]] <- env$tempCountFear[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (is.na(fearData[l, 3])) {
    } else if (tokenizeWords1 == fearData[l, 3]) {
      env$tempCountFear[["Neutral"]] <- env$tempCountFear[["Neutral"]] + 1
      break()
    }
    
    if (is.na(fearData[l, 4])) {
    } else if (tokenizeWords1 == fearData[l, 4]) {
      env$tempCountFear[["High"]] <- env$tempCountFear[["High"]] + 1
      break()
    }
    
    if (is.na(fearData[l, 5])) {
    } else if (tokenizeWords1 == fearData[l, 5]) {
      env$tempCountFear[["Higher"]] <- env$tempCountFear[["Higher"]] + 1
      break()
    }
  }
}