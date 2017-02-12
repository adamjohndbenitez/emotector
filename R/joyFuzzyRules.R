joy.FuzzyRules <- function(joyData, tokenizeWords1, tokenizeWords2, env = parent.frame()) {
  for (l in 1:nrow(joyData)) {
    if (identical(tokenizeWords1, joyData[l, 1])) {
      for (n in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[n, 3])) {
          env$tempCountJoy[["Lowest"]] <- env$tempCountJoy[["Lowest"]] + 1
          env$tempCountJoy[["Neutral"]] <- env$tempCountJoy[["Neutral"]] - 1
          break()
        }

        if (identical(tokenizeWords2, joyData[n, 4])) {
          env$tempCountJoy[["Lowest"]] <- env$tempCountJoy[["Lowest"]] + 1
          env$tempCountJoy[["High"]] <- env$tempCountJoy[["High"]] - 1
          break()
        }

        if (identical(tokenizeWords2, joyData[n, 5])) {
          env$tempCountJoy[["Lowest"]] <- env$tempCountJoy[["Lowest"]] + 1
          env$tempCountJoy[["Higher"]] <- env$tempCountJoy[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, joyData[l, 2])) {
      for (m in 1:nrow(joyData)) {
        if (is.na(joyData[m, 3])) {
        } else if (tokenizeWords2 == joyData[m, 3]) {
          env$tempCountJoy[["Low"]] <- env$tempCountJoy[["Low"]] + 1
          env$tempCountJoy[["Neutral"]] <- env$tempCountJoy[["Neutral"]] - 1
          break()
        }

        if (is.na(joyData[m, 4])) {
        } else if (tokenizeWords2 == joyData[m, 4]) {
          env$tempCountJoy[["Low"]] <- env$tempCountJoy[["Low"]] + 1
          env$tempCountJoy[["High"]] <- env$tempCountJoy[["High"]] - 1
          break()
        }

        if (is.na(joyData[m, 5])) {
        } else if (tokenizeWords2 == joyData[m, 5]) {
          env$tempCountJoy[["Low"]] <- env$tempCountJoy[["Low"]] + 1
          env$tempCountJoy[["Higher"]] <- env$tempCountJoy[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, joyData[l, 6])) {
      for (o in 1:nrow(joyData)) {
        if (identical(tokenizeWords2, joyData[o, 3])) {
          env$tempCountJoy[["Highest"]] <- env$tempCountJoy[["Highest"]] + 1
          env$tempCountJoy[["Neutral"]] <- env$tempCountJoy[["Neutral"]] - 1
          break()
        }

        if (identical(tokenizeWords2, joyData[o, 4])) {
          env$tempCountJoy[["Highest"]] <- env$tempCountJoy[["Highest"]] + 1
          env$tempCountJoy[["High"]] <- env$tempCountJoy[["High"]] - 1
          break()
        }

        if (identical(tokenizeWords2, joyData[o, 5])) {
          env$tempCountJoy[["Highest"]] <- env$tempCountJoy[["Highest"]] + 1
          env$tempCountJoy[["Higher"]] <- env$tempCountJoy[["Higher"]] - 1
          break()
        }
      }
      break()
    }
    
    if (identical(tokenizeWords1, joyData[l, 3])) {
      env$tempCountJoy[["Neutral"]] <- env$tempCountJoy[["Neutral"]] + 1
      break()
    }
    
    if (identical(tokenizeWords1, joyData[l, 4])) {
      env$tempCountJoy[["High"]] <- env$tempCountJoy[["High"]] + 1
      break()
    }
    
    if (identical(tokenizeWords1, joyData[l, 5])) {
      env$tempCountJoy[["Higher"]] <- env$tempCountJoy[["Higher"]] + 1
      break()
    }
  }
}