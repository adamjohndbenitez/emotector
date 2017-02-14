emojis.FuzzyRules <- function(loveWeightData, hahaWeightData, sadWeightData, angryWeightData, env = parent.frame()) {
  for (r in 1:length(loveWeightData)) {
    if (loveWeightData[r] >= 0 & loveWeightData[r] <= 20) {
      env$finalWeightLove[["Lowest"]] <- env$finalWeightLove[["Lowest"]] + 0
      env$finalWeightJoy[["Lowest"]] <- env$finalWeightJoy[["Lowest"]] + 0
    } else if (loveWeightData[r] >= 21 & loveWeightData[r] <= 40) {
      env$finalWeightLove[["Low"]] <- env$finalWeightLove[["Low"]] + 0.2
      env$finalWeightJoy[["Low"]] <- env$finalWeightJoy[["Low"]] + 0.2
    } else if (loveWeightData[r] >= 41 & loveWeightData[r] <= 60) {
      env$finalWeightLove[["Neutral"]] <- env$finalWeightLove[["Neutral"]] + 0.4
      env$finalWeightJoy[["Neutral"]] <- env$finalWeightJoy[["Neutral"]] + 0.4
    } else if (loveWeightData[r] >= 61 & loveWeightData[r] <= 80) {
      env$finalWeightLove[["High"]] <- env$finalWeightLove[["High"]] + 0.6
      env$finalWeightJoy[["High"]] <- env$finalWeightJoy[["High"]] + 0.6
    } else if (loveWeightData[r] >= 81 & loveWeightData[r] <= 100) {
      env$finalWeightLove[["higher"]] <- env$finalWeightLove[["Higher"]] + 0.8
      env$finalWeightJoy[["higher"]] <- env$finalWeightJoy[["Higher"]] + 0.8
    } else if (loveWeightData[r] >= 101) {
      env$finalWeightLove[["highest"]] <- env$finalWeightLove[["Highest"]] + 1
      env$finalWeightJoy[["highest"]] <- env$finalWeightJoy[["Highest"]] + 1
    }
  }
  env$finalEmojis[["Love"]] <- base::sum(unlist(env$finalWeightLove))
  
  for (r in 1:length(hahaWeightData)) {
    if (hahaWeightData[r] >= 0 & hahaWeightData[r] <= 20) {
      env$finalWeightHaha[["Lowest"]] <- env$finalWeightHaha[["Lowest"]] + 0
      env$finalWeightJoy[["Lowest"]] <- env$finalWeightJoy[["Lowest"]] + 0
    } else if (hahaWeightData[r] >= 21 & hahaWeightData[r] <= 40) {
      env$finalWeightHaha[["Low"]] <- env$finalWeightHaha[["Low"]] + 0.2
      env$finalWeightJoy[["Low"]] <- env$finalWeightJoy[["Low"]] + 0.2
    } else if (hahaWeightData[r] >= 41 & hahaWeightData[r] <= 60) {
      env$finalWeightHaha[["Neutral"]] <- env$finalWeightHaha[["Neutral"]] + 0.4
      env$finalWeightJoy[["Neutral"]] <- env$finalWeightJoy[["Neutral"]] + 0.4
    } else if (hahaWeightData[r] >= 61 & hahaWeightData[r] <= 80) {
      env$finalWeightHaha[["High"]] <- env$finalWeightHaha[["High"]] + 0.6
      env$finalWeightJoy[["High"]] <- env$finalWeightJoy[["High"]] + 0.6
    } else if (hahaWeightData[r] >= 81 & hahaWeightData[r] <= 100) {
      env$finalWeightHaha[["higher"]] <- env$finalWeightHaha[["Higher"]] + 0.8
      env$finalWeightJoy[["higher"]] <- env$finalWeightJoy[["Higher"]] + 0.8
    } else if (hahaWeightData[r] >= 101) {
      env$finalWeightHaha[["highest"]] <- env$finalWeightHaha[["Highest"]] + 1
      env$finalWeightJoy[["highest"]] <- env$finalWeightJoy[["Highest"]] + 1
    }
  }
  env$finalEmojis[["Haha"]] <- base::sum(unlist(env$finalWeightHaha))
  
  for (s in 1:length(sadWeightData)) {
    if (sadWeightData[s] >= 0 & sadWeightData[s] <= 20) {
      env$finalWeightSad[["Lowest"]] <- env$finalWeightSad[["Lowest"]] + 0
      env$finalWeightSadness[["Lowest"]] <- env$finalWeightSadness[["Lowest"]] + 0
    } else if (sadWeightData[s] >= 21 & sadWeightData[s] <= 40) {
      env$finalWeightSad[["Low"]] <- env$finalWeightSad[["Low"]] + 0.2
      env$finalWeightSadness[["Low"]] <- env$finalWeightSadness[["Low"]] + 0.2
    } else if (sadWeightData[s] >= 41 & sadWeightData[s] <= 60) {
      env$finalWeightSad[["Neutral"]] <- env$finalWeightSad[["Neutral"]] + 0.4
      env$finalWeightSadness[["Neutral"]] <- env$finalWeightSadness[["Neutral"]] + 0.4
    } else if (sadWeightData[s] >= 61 & sadWeightData[s] <= 80) {
      env$finalWeightSad[["High"]] <- env$finalWeightSad[["High"]] + 0.6
      env$finalWeightSadness[["High"]] <- env$finalWeightSadness[["High"]] + 0.6
    } else if (sadWeightData[s] >= 81 & sadWeightData[s] <= 100) {
      env$finalWeightSad[["higher"]] <- env$finalWeightSad[["Higher"]] + 0.8
      env$finalWeightSadness[["higher"]] <- env$finalWeightSadness[["Higher"]] + 0.8
    } else if (sadWeightData[s] >= 101) {
      env$finalWeightSad[["highest"]] <- env$finalWeightSad[["Highest"]] + 1
      env$finalWeightSadness[["highest"]] <- env$finalWeightSadness[["Highest"]] + 1
    }
  }
  
  env$finalEmojis[["Sad"]] <- base::sum(unlist(env$finalWeightSad))
  
  for (t in 1:length(angryWeightData)) {
    if (angryWeightData[t] >= 0 & angryWeightData[t] <= 20) {
      env$finalWeightAngry[["Lowest"]] <- env$finalWeightAngry[["Lowest"]] + 0
      env$finalWeightAnger[["Lowest"]] <- env$finalWeightAnger[["Lowest"]] + 0
    } else if (angryWeightData[t] >= 21 & angryWeightData[t] <= 40) {
      env$finalWeightAngry[["Low"]] <- env$finalWeightAngry[["Low"]] + 0.2
      env$finalWeightAnger[["Low"]] <- env$finalWeightAnger[["Low"]] + 0.2
    } else if (angryWeightData[t] >= 41 & angryWeightData[t] <= 60) {
      env$finalWeightAngry[["Neutral"]] <- env$finalWeightAngry[["Neutral"]] + 0.4
      env$finalWeightAnger[["Neutral"]] <- env$finalWeightAnger[["Neutral"]] + 0.4
    } else if (angryWeightData[t] >= 61 & angryWeightData[t] <= 80) {
      env$finalWeightAngry[["High"]] <- env$finalWeightAngry[["High"]] + 0.6
      env$finalWeightAnger[["High"]] <- env$finalWeightAnger[["High"]] + 0.6
    } else if (angryWeightData[t] >= 81 & angryWeightData[t] <= 100) {
      env$finalWeightAngry[["higher"]] <- env$finalWeightAngry[["Higher"]] + 0.8
      env$finalWeightAnger[["higher"]] <- env$finalWeightAnger[["Higher"]] + 0.8
    } else if (angryWeightData[t] >= 101) {
      env$finalWeightAngry[["highest"]] <- env$finalWeightAngry[["Highest"]] + 1
      env$finalWeightAnger[["highest"]] <- env$finalWeightAnger[["Highest"]] + 1
    }
  }
  
  env$finalEmojis[["Angry"]] <- base::sum(unlist(env$finalWeightAngry))
}