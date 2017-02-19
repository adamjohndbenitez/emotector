emoticons.FuzzyRules <- function(emoticonsData, AllEmoticons, env = parent.frame()) {
  for (i in 1:length(AllEmoticons)) {
      print(typeof(AllEmoticons))
    for (j in 1:nrow(emoticonsData)) {
      print(emoticonsData[j, 1])
      if (identical(AllEmoticons[i], emoticonsData[j, 1])) {
        env$finalCountEmoticons[["Joy"]] <- env$finalCountEmoticons[["Joy"]] + 1
        env$finalWeightEmoticons[["Joy"]] <- env$finalWeightEmoticons[["Joy"]] + 0.4
        env$finalWeightJoy[["Neutral"]] <- env$finalWeightJoy[["Neutral"]] + 0.4
        break()
      }
      if (identical(AllEmoticons[i], emoticonsData[j, 2])) {
        env$finalCountEmoticons[["Sadness"]] <- env$finalCountEmoticons[["Sadness"]] + 1
        env$finalWeightEmoticons[["Sadness"]] <- env$finalWeightEmoticons[["Sadness"]] + 0.4
        env$finalWeightSadness[["Neutral"]] <- env$finalWeightSadness[["Neutral"]] + 0.4
        break()
      }
      if (identical(AllEmoticons[i], emoticonsData[j, 3])) {
        env$finalCountEmoticons[["Anger"]] <- env$finalCountEmoticons[["Anger"]] + 1
        env$finalWeightEmoticons[["Anger"]] <- env$finalWeightEmoticons[["Anger"]] + 0.4
        env$finalWeightAnger[["Neutral"]] <- env$finalWeightAnger[["Neutral"]] + 0.4
        break()
      }
      if (identical(AllEmoticons[i], emoticonsData[j, 4])) {
        env$finalCountEmoticons[["Disgust"]] <- env$finalCountEmoticons[["Disgust"]] + 1
        env$finalWeightEmoticons[["Disgust"]] <- env$finalWeightEmoticons[["Disgust"]] + 0.4
        env$finalWeightDisgust[["Neutral"]] <- env$finalWeightDisgust[["Neutral"]] + 0.4
        break()
      }
      if (identical(AllEmoticons[i], emoticonsData[j, 5])) {
        env$finalCountEmoticons[["Fear"]] <- env$finalCountEmoticons[["Fear"]] + 1
        env$finalWeightEmoticons[["Fear"]] <- env$finalWeightEmoticons[["Fear"]] + 0.4
        env$finalWeightFear[["Neutral"]] <- env$finalWeightFear[["Neutral"]] + 0.4
        break()
      }
    }
  }
}

