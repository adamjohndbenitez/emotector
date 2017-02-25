tally.emotions <- function(env = parent.frame()) {
  #----------START STORE TEMP-TALLY-OF-JOY TO FINAL-TALLY-OF-JOY----------
  env$finalCountJoy[["Lowest"]] <- env$finalCountJoy[["Lowest"]] + env$tempCountJoy[["Lowest"]]
  env$onlyCountJoyWord[["Lowest"]] <- env$onlyCountJoyWord[["Lowest"]] + env$tempCountJoy[["Lowest"]]
  env$finalWeightJoy[["Lowest"]] <- env$finalCountJoy[["Lowest"]] * 0
  env$onlyWeightJoyWord[["Lowest"]] <- env$onlyCountJoyWord[["Lowest"]] * 0
  env$finalCountJoy[["Low"]] <- env$finalCountJoy[["Low"]] + env$tempCountJoy[["Low"]]
  env$onlyCountJoyWord[["Low"]] <- env$onlyCountJoyWord[["Low"]] + env$tempCountJoy[["Low"]]
  env$finalWeightJoy[["Low"]] <- env$finalCountJoy[["Low"]] * 0.2
  env$onlyWeightJoyWord[["Low"]] <- env$onlyCountJoyWord[["Low"]] * 0.2
  env$finalCountJoy[["Neutral"]] <- env$finalCountJoy[["Neutral"]] + env$tempCountJoy[["Neutral"]]
  env$onlyCountJoyWord[["Neutral"]] <- env$onlyCountJoyWord[["Neutral"]] + env$tempCountJoy[["Neutral"]]
  env$finalWeightJoy[["Neutral"]] <- env$finalCountJoy[["Neutral"]] * 0.4
  env$onlyWeightJoyWord[["Neutral"]] <- env$onlyCountJoyWord[["Neutral"]] * 0.4
  env$finalCountJoy[["High"]] <- env$finalCountJoy[["High"]] + env$tempCountJoy[["High"]]
  env$onlyCountJoyWord[["High"]] <- env$onlyCountJoyWord[["High"]] + env$tempCountJoy[["High"]]
  env$finalWeightJoy[["High"]] <- env$finalCountJoy[["High"]] * 0.6
  env$onlyWeightJoyWord[["High"]] <- env$onlyCountJoyWord[["High"]] * 0.6
  env$finalCountJoy[["Higher"]] <- env$finalCountJoy[["Higher"]] + env$tempCountJoy[["Higher"]]
  env$onlyCountJoyWord[["Higher"]] <- env$onlyCountJoyWord[["Higher"]] + env$tempCountJoy[["Higher"]]
  env$finalWeightJoy[["Higher"]] <- env$finalCountJoy[["Higher"]] * 0.8
  env$onlyWeightJoyWord[["Higher"]] <- env$onlyCountJoyWord[["Higher"]] * 0.8
  env$finalCountJoy[["Highest"]] <- env$finalCountJoy[["Highest"]] + env$tempCountJoy[["Highest"]]
  env$onlyCountJoyWord[["Highest"]] <- env$onlyCountJoyWord[["Highest"]] + env$tempCountJoy[["Highest"]]
  env$finalWeightJoy[["Highest"]] <- env$finalCountJoy[["Highest"]] * 1
  env$onlyWeightJoyWord[["Highest"]] <- env$onlyCountJoyWord[["Highest"]] * 1
  #----------END STORE TEMP-TALLY-OF-JOY TO FINAL-TALLY-OF-JOY----------
  #----------START STORE TEMP-TALLY-OF-SADNESS TO FINAL-TALLY-OF-SADNESS----------
  env$finalCountSadness[["Lowest"]] <- env$finalCountSadness[["Lowest"]] + env$tempCountSadness[["Lowest"]]
  env$onlyCountSadnessWord[["Lowest"]] <- env$onlyCountSadnessWord[["Lowest"]] + env$tempCountSadness[["Lowest"]]
  env$finalWeightSadness[["Lowest"]] <- env$finalCountSadness[["Lowest"]] * 0
  env$onlyWeightSadnessWord[["Lowest"]] <- env$onlyCountSadnessWord[["Lowest"]] * 0
  env$finalCountSadness[["Low"]] <- env$finalCountSadness[["Low"]] + env$tempCountSadness[["Low"]]
  env$onlyCountSadnessWord[["Low"]] <- env$onlyCountSadnessWord[["Low"]] + env$tempCountSadness[["Low"]]
  env$finalWeightSadness[["Low"]] <- env$finalCountSadness[["Low"]] * 0.2
  env$onlyWeightSadnessWord[["Low"]] <- env$onlyCountSadnessWord[["Low"]] * 0.2
  env$finalCountSadness[["Neutral"]] <- env$finalCountSadness[["Neutral"]] + env$tempCountSadness[["Neutral"]]
  env$onlyCountSadnessWord[["Neutral"]] <- env$onlyCountSadnessWord[["Neutral"]] + env$tempCountSadness[["Neutral"]]
  env$finalWeightSadness[["Neutral"]] <- env$finalCountSadness[["Neutral"]] * 0.4
  env$onlyWeightSadnessWord[["Neutral"]] <- env$onlyCountSadnessWord[["Neutral"]] * 0.4
  env$finalCountSadness[["High"]] <- env$finalCountSadness[["High"]] + env$tempCountSadness[["High"]]
  env$onlyCountSadnessWord[["High"]] <- env$onlyCountSadnessWord[["High"]] + env$tempCountSadness[["High"]]
  env$finalWeightSadness[["High"]] <- env$finalCountSadness[["High"]] * 0.6
  env$onlyWeightSadnessWord[["High"]] <- env$onlyCountSadnessWord[["High"]] * 0.6
  env$finalCountSadness[["Higher"]] <- env$finalCountSadness[["Higher"]] + env$tempCountSadness[["Higher"]]
  env$onlyCountSadnessWord[["Higher"]] <- env$onlyCountSadnessWord[["Higher"]] + env$tempCountSadness[["Higher"]]
  env$finalWeightSadness[["Higher"]] <- env$finalCountSadness[["Higher"]] * 0.8
  env$onlyWeightSadnessWord[["Higher"]] <- env$onlyCountSadnessWord[["Higher"]] * 0.8
  env$finalCountSadness[["Highest"]] <- env$finalCountSadness[["Highest"]] + env$tempCountSadness[["Highest"]]
  env$onlyCountSadnessWord[["Highest"]] <- env$onlyCountSadnessWord[["Highest"]] + env$tempCountSadness[["Highest"]]
  env$finalWeightSadness[["Highest"]] <- env$finalCountSadness[["Highest"]] * 1
  env$onlyWeightSadnessWord[["Highest"]] <- env$onlyCountSadnessWord[["Highest"]] * 1
  #----------START STORE TEMP-TALLY-OF-SADNESS TO FINAL-TALLY-OF-SADNESS----------
  #----------START STORE TEMP-TALLY-OF-ANGER TO FINAL-TALLY-OF-ANGER----------
  env$finalCountAnger[["Lowest"]] <- env$finalCountAnger[["Lowest"]] + env$tempCountAnger[["Lowest"]]
  env$onlyCountAngerWord[["Lowest"]] <- env$onlyCountAngerWord[["Lowest"]] + env$tempCountAnger[["Lowest"]]
  env$finalWeightAnger[["Lowest"]] <- env$finalCountAnger[["Lowest"]] * 0
  env$onlyWeightAngerWord[["Lowest"]] <- env$onlyCountAngerWord[["Lowest"]] * 0
  env$finalCountAnger[["Low"]] <- env$finalCountAnger[["Low"]] + env$tempCountAnger[["Low"]]
  env$onlyCountAngerWord[["Low"]] <- env$onlyCountAngerWord[["Low"]] + env$tempCountAnger[["Low"]]
  env$finalWeightAnger[["Low"]] <- env$finalCountAnger[["Low"]] * 0.2
  env$onlyWeightAngerWord[["Low"]] <- env$onlyCountAngerWord[["Low"]] * 0.2
  env$finalCountAnger[["Neutral"]] <- env$finalCountAnger[["Neutral"]] + env$tempCountAnger[["Neutral"]]
  env$onlyCountAngerWord[["Neutral"]] <- env$onlyCountAngerWord[["Neutral"]] + env$tempCountAnger[["Neutral"]]
  env$finalWeightAnger[["Neutral"]] <- env$finalCountAnger[["Neutral"]] * 0.4
  env$onlyWeightAngerWord[["Neutral"]] <- env$onlyCountAngerWord[["Neutral"]] * 0.4
  env$finalCountAnger[["High"]] <- env$finalCountAnger[["High"]] + env$tempCountAnger[["High"]]
  env$onlyCountAngerWord[["High"]] <- env$onlyCountAngerWord[["High"]] + env$tempCountAnger[["High"]]
  env$finalWeightAnger[["High"]] <- env$finalCountAnger[["High"]] * 0.6
  env$onlyWeightAngerWord[["High"]] <- env$onlyCountAngerWord[["High"]] * 0.6
  env$finalCountAnger[["Higher"]] <- env$finalCountAnger[["Higher"]] + env$tempCountAnger[["Higher"]]
  env$onlyCountAngerWord[["Higher"]] <- env$onlyCountAngerWord[["Higher"]] + env$tempCountAnger[["Higher"]]
  env$finalWeightAnger[["Higher"]] <- env$finalCountAnger[["Higher"]] * 0.8
  env$onlyWeightAngerWord[["Higher"]] <- env$onlyCountAngerWord[["Higher"]] * 0.8
  env$finalCountAnger[["Highest"]] <- env$finalCountAnger[["Highest"]] + env$tempCountAnger[["Highest"]]
  env$onlyCountAngerWord[["Highest"]] <- env$onlyCountAngerWord[["Highest"]] + env$tempCountAnger[["Highest"]]
  env$finalWeightAnger[["Highest"]] <- env$finalCountAnger[["Highest"]] * 1
  env$onlyWeightAngerWord[["Highest"]] <- env$onlyCountAngerWord[["Highest"]] * 1
  #----------END STORE TEMP-TALLY-OF-ANGER TO FINAL-TALLY-OF-ANGER----------
  #----------START STORE TEMP-TALLY-OF-DISGUST TO FINAL-TALLY-OF-DISGUST----------
  env$finalCountDisgust[["Lowest"]] <- env$finalCountDisgust[["Lowest"]] + env$tempCountDisgust[["Lowest"]]
  env$onlyCountDisgustWord[["Lowest"]] <- env$onlyCountDisgustWord[["Lowest"]] + env$tempCountDisgust[["Lowest"]]
  env$finalWeightDisgust[["Lowest"]] <- env$finalCountDisgust[["Lowest"]] * 0
  env$onlyWeightDisgustWord[["Lowest"]] <- env$onlyCountDisgustWord[["Lowest"]] * 0
  env$finalCountDisgust[["Low"]] <- env$finalCountDisgust[["Low"]] + env$tempCountDisgust[["Low"]]
  env$onlyCountDisgustWord[["Low"]] <- env$onlyCountDisgustWord[["Low"]] + env$tempCountDisgust[["Low"]]
  env$finalWeightDisgust[["Low"]] <- env$finalCountDisgust[["Low"]] * 0.2
  env$onlyWeightDisgustWord[["Low"]] <- env$onlyCountDisgustWord[["Low"]] * 0.2
  env$finalCountDisgust[["Neutral"]] <- env$finalCountDisgust[["Neutral"]] + env$tempCountDisgust[["Neutral"]]
  env$onlyCountDisgustWord[["Neutral"]] <- env$onlyCountDisgustWord[["Neutral"]] + env$tempCountDisgust[["Neutral"]]
  env$finalWeightDisgust[["Neutral"]] <- env$finalCountDisgust[["Neutral"]] * 0.4
  env$onlyWeightDisgustWord[["Neutral"]] <- env$onlyCountDisgustWord[["Neutral"]] * 0.4
  env$finalCountDisgust[["High"]] <- env$finalCountDisgust[["High"]] + env$tempCountDisgust[["High"]]
  env$onlyCountDisgustWord[["High"]] <- env$onlyCountDisgustWord[["High"]] + env$tempCountDisgust[["High"]]
  env$finalWeightDisgust[["High"]] <- env$finalCountDisgust[["High"]] * 0.6
  env$onlyWeightDisgustWord[["High"]] <- env$onlyCountDisgustWord[["High"]] * 0.6
  env$finalCountDisgust[["Higher"]] <- env$finalCountDisgust[["Higher"]] + env$tempCountDisgust[["Higher"]]
  env$onlyCountDisgustWord[["Higher"]] <- env$onlyCountDisgustWord[["Higher"]] + env$tempCountDisgust[["Higher"]]
  env$finalWeightDisgust[["Higher"]] <- env$finalCountDisgust[["Higher"]] * 0.8
  env$onlyWeightDisgustWord[["Higher"]] <- env$onlyCountDisgustWord[["Higher"]] * 0.8
  env$finalCountDisgust[["Highest"]] <- env$finalCountDisgust[["Highest"]] + env$tempCountDisgust[["Highest"]]
  env$onlyCountDisgustWord[["Highest"]] <- env$onlyCountDisgustWord[["Highest"]] + env$tempCountDisgust[["Highest"]]
  env$finalWeightDisgust[["Highest"]] <- env$finalCountDisgust[["Highest"]] * 1
  env$onlyWeightDisgustWord[["Highest"]] <- env$onlyCountDisgustWord[["Highest"]] * 1
  #----------END STORE TEMP-TALLY-OF-DISGUST TO FINAL-TALLY-OF-DISGUST----------
  #----------START STORE TEMP-TALLY-OF-FEAR TO FINAL-TALLY-OF-FEAR----------
  env$finalCountFear[["Lowest"]] <- env$finalCountFear[["Lowest"]] + env$tempCountFear[["Lowest"]]
  env$onlyCountFearWord[["Lowest"]] <- env$onlyCountFearWord[["Lowest"]] + env$tempCountFear[["Lowest"]]
  env$finalWeightFear[["Lowest"]] <- env$finalCountFear[["Lowest"]] * 0
  env$onlyWeightFearWord[["Lowest"]] <- env$onlyWeightFearWord[["Lowest"]] * 0
  env$finalCountFear[["Low"]] <- env$finalCountFear[["Low"]] + env$tempCountFear[["Low"]]
  env$onlyCountFearWord[["Low"]] <- env$onlyCountFearWord[["Low"]] + env$tempCountFear[["Low"]]
  env$finalWeightFear[["Low"]] <- env$finalCountFear[["Low"]] * 0.2
  env$onlyWeightFearWord[["Low"]] <- env$onlyCountFearWord[["Low"]] * 0.2
  env$finalCountFear[["Neutral"]] <- env$finalCountFear[["Neutral"]] + env$tempCountFear[["Neutral"]]
  env$onlyCountFearWord[["Neutral"]] <- env$onlyCountFearWord[["Neutral"]] + env$tempCountFear[["Neutral"]]
  env$finalWeightFear[["Neutral"]] <- env$finalCountFear[["Neutral"]] * 0.4
  env$onlyWeightFearWord[["Neutral"]] <- env$onlyCountFearWord[["Neutral"]] * 0.4
  env$finalCountFear[["High"]] <- env$finalCountFear[["High"]] + env$tempCountFear[["High"]]
  env$onlyCountFearWord[["High"]] <- env$onlyCountFearWord[["High"]] + env$tempCountFear[["High"]]
  env$finalWeightFear[["High"]] <- env$finalCountFear[["High"]] * 0.6
  env$onlyWeightFearWord[["High"]] <- env$onlyCountFearWord[["High"]] * 0.6
  env$finalCountFear[["Higher"]] <- env$finalCountFear[["Higher"]] + env$tempCountFear[["Higher"]]
  env$onlyCountFearWord[["Higher"]] <- env$onlyCountFearWord[["Higher"]] + env$tempCountFear[["Higher"]]
  env$finalWeightFear[["Higher"]] <- env$finalCountFear[["Higher"]] * 0.8
  env$onlyWeightFearWord[["Higher"]] <- env$onlyCountFearWord[["Higher"]] * 0.8
  env$finalCountFear[["Highest"]] <- env$finalCountFear[["Highest"]] + env$tempCountFear[["Highest"]]
  env$onlyCountFearWord[["Highest"]] <- env$onlyCountFearWord[["Highest"]] + env$tempCountFear[["Highest"]]
  env$finalWeightFear[["Highest"]] <- env$finalCountFear[["Highest"]] * 1
  env$onlyWeightFearWord[["Highest"]] <- env$onlyCountFearWord[["Highest"]] * 1
  #----------END STORE TEMP-TALLY-OF-FEAR TO FINAL-TALLY-OF-FEAR----------
}

total.emotions <- function(env = parent.frame()) {
  env$sumWeights[["Joy"]] <- base::sum(unlist(env$finalWeightJoy))
  env$sumCounts[["Joy"]] <- base::sum(unlist(env$finalCountJoy))
  env$onlySumWeightsWord[["Joy"]] <- base::sum(unlist(env$onlyWeightJoyWord))
  env$onlySumCountsWord[["Joy"]] <- base::sum(unlist(env$onlyCountJoyWord))
  env$sumWeights[["Sadness"]] <- base::sum(unlist(env$finalWeightSadness))
  env$sumCounts[["Sadness"]] <- base::sum(unlist(env$finalCountSadness))
  env$onlySumWeightsWord[["Sadness"]] <- base::sum(unlist(env$onlyWeightSadnessWord))
  env$onlySumCountsWord[["Sadness"]] <- base::sum(unlist(env$onlyCountSadnessWord))
  env$sumWeights[["Anger"]] <- base::sum(unlist(env$finalWeightAnger))
  env$sumCounts[["Anger"]] <- base::sum(unlist(env$finalCountAnger))
  env$onlySumWeightsWord[["Anger"]] <- base::sum(unlist(env$onlyWeightAngerWord))
  env$onlySumCountsWord[["Anger"]] <- base::sum(unlist(env$onlyCountAngerWord))
  env$sumWeights[["Disgust"]] <- base::sum(unlist(env$finalWeightDisgust))
  env$sumCounts[["Disgust"]] <- base::sum(unlist(env$finalCountDisgust))
  env$onlySumWeightsWord[["Disgust"]] <- base::sum(unlist(env$onlyWeightDisgustWord))
  env$onlySumCountsWord[["Disgust"]] <- base::sum(unlist(env$onlyCountDisgustWord))
  env$sumWeights[["Fear"]] <- base::sum(unlist(env$finalWeightFear))
  env$sumCounts[["Fear"]] <- base::sum(unlist(env$finalCountFear))
  env$onlySumWeightsWord[["Fear"]] <- base::sum(unlist(env$onlyWeightFearWord))
  env$onlySumCountsWord[["Fear"]] <- base::sum(unlist(env$onlyCountFearWord))
}
