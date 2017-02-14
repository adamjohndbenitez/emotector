emojis.FuzzyRules <- function(hahaWeightData, sadWeightData, angryWeightData, env = parent.frame()) {
  for (r in 1:length(hahaWeightData)) {
    if (hahaWeightData[r] >= 0 & hahaWeightData[r] <= 20) {
      env$finalWeightJoy[["Lowest"]] <- env$finalWeightJoy[["Lowest"]] + 0
    } else if (hahaWeightData[r] >= 21 & hahaWeightData[r] <= 40) {
      env$finalWeightJoy[["Low"]] <- env$finalWeightJoy[["Low"]] + 0.2
    } else if (hahaWeightData[r] >= 41 & hahaWeightData[r] <= 60) {
      env$finalWeightJoy[["Neutral"]] <- env$finalWeightJoy[["Neutral"]] + 0.4
    } else if (hahaWeightData[r] >= 61 & hahaWeightData[r] <= 80) {
      env$finalWeightJoy[["High"]] <- env$finalWeightJoy[["High"]] + 0.6
    } else if (hahaWeightData[r] >= 81 & hahaWeightData[r] <= 100) {
      env$finalWeightJoy[["higher"]] <- env$finalWeightJoy[["Higher"]] + 0.8
    } else if (hahaWeightData[r] >= 101) {
      env$finalWeightJoy[["highest"]] <- env$finalWeightJoy[["Highest"]] + 1
    }
  }
  
  for (s in 1:length(sadWeightData)) {
    if (sadWeightData[s] >= 0 & sadWeightData[s] <= 20) {
      env$finalWeightJoy[["Lowest"]] <- env$finalWeightJoy[["Lowest"]] + 0
    } else if (sadWeightData[s] >= 21 & sadWeightData[s] <= 40) {
      env$finalWeightJoy[["Low"]] <- env$finalWeightJoy[["Low"]] + 0.2
    } else if (sadWeightData[s] >= 41 & sadWeightData[s] <= 60) {
      env$finalWeightJoy[["Neutral"]] <- env$finalWeightJoy[["Neutral"]] + 0.4
    } else if (sadWeightData[s] >= 61 & sadWeightData[s] <= 80) {
      env$finalWeightJoy[["High"]] <- env$finalWeightJoy[["High"]] + 0.6
    } else if (sadWeightData[s] >= 81 & sadWeightData[s] <= 100) {
      env$finalWeightJoy[["higher"]] <- env$finalWeightJoy[["Higher"]] + 0.8
    } else if (sadWeightData[s] >= 101) {
      env$finalWeightJoy[["highest"]] <- env$finalWeightJoy[["Highest"]] + 1
    }
  }
  
  for (t in 1:length(angryWeightData)) {
    if (angryWeightData[t] >= 0 & angryWeightData[t] <= 20) {
      env$finalWeightJoy[["Lowest"]] <- env$finalWeightJoy[["Lowest"]] + 0
    } else if (angryWeightData[t] >= 21 & angryWeightData[t] <= 40) {
      env$finalWeightJoy[["Low"]] <- env$finalWeightJoy[["Low"]] + 0.2
    } else if (angryWeightData[t] >= 41 & angryWeightData[t] <= 60) {
      env$finalWeightJoy[["Neutral"]] <- env$finalWeightJoy[["Neutral"]] + 0.4
    } else if (angryWeightData[t] >= 61 & angryWeightData[t] <= 80) {
      env$finalWeightJoy[["High"]] <- env$finalWeightJoy[["High"]] + 0.6
    } else if (angryWeightData[t] >= 81 & angryWeightData[t] <= 100) {
      env$finalWeightJoy[["higher"]] <- env$finalWeightJoy[["Higher"]] + 0.8
    } else if (angryWeightData[t] >= 101) {
      env$finalWeightJoy[["highest"]] <- env$finalWeightJoy[["Highest"]] + 1
    }
  }
}