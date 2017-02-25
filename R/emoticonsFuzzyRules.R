emoticons.FuzzyRules <- function(emoticonsData, analyzePostAndItsComments, env = parent.frame()) {
  unicodeRegex <- "<U\\+[a-zA-Z0-9]*>"
  unicodeRegex2 <- "<U\\+[a-zA-Z0-9]*><U\\+[a-zA-Z0-9]*>"
  smileyRegex <- "([0-9A-Za-z'\\&\\-\\.\\/\\(\\)=:;]+)|((?::|;|=)(?:-)?(?:\\)|D|P))"
  PostsNativeEncoded <- enc2native(analyzePostAndItsComments)
  AllEmoticons <- c()
  for (i in 1:length(PostsNativeEncoded)) {
    everyWord <- tokenizers::tokenize_regex(x = PostsNativeEncoded[i], pattern = "\\s+", simplify = TRUE)
    for (j in 1:length(everyWord)) {
      isEdTag <- grepl(pattern = "<ed>", x = everyWord[j])
      isUnicode <- grepl(pattern = unicodeRegex, x = everyWord[j])
      isUnicode2 <- grepl(pattern = unicodeRegex2, x = everyWord[j])
      isSmiley <- grepl(pattern = smileyRegex, x = everyWord[j])
      if (isTRUE(isEdTag)) {
        everyUnicode <- strsplit(x = everyWord[j], split = "<ed>")
        for (k in everyUnicode) {
          AllEmoticons <- append(x = AllEmoticons, values = k)
        }
      }
      if (isTRUE(isUnicode) | isTRUE(isUnicode2)) {
        getEmoticons <- regexpr(pattern = unicodeRegex, text = everyWord[j])
        emoticonsLists <- regmatches(x = everyWord[j], m = getEmoticons)
        AllEmoticons <- append(x = AllEmoticons, values = emoticonsLists)
        getEmoticons2 <- regexpr(pattern = unicodeRegex2, text = everyWord[j])
        emoticonsLists2 <- regmatches(x = everyWord[j], m = getEmoticons2)
        AllEmoticons <- append(x = AllEmoticons, values = emoticonsLists2)
      }
      if (isTRUE(isSmiley)) {
        getSmileys <- regexpr(pattern = smileyRegex, text = everyWord[j])
        emoticonsLists3 <- regmatches(x = everyWord[j], m = getSmileys)
        AllEmoticons <- append(x = AllEmoticons, values = emoticonsLists3)
      }
    }
  }
  
  for (i in 1:length(AllEmoticons)) {
    for (j in 1:nrow(emoticonsData)) {
      if (identical(AllEmoticons[i], emoticonsData[j, 1]) & !is.na(emoticonsData[j, 1])) {
        env$finalCountEmoticons[["Joy"]] <- env$finalCountEmoticons[["Joy"]] + 1
        env$finalWeightEmoticons[["Joy"]] <- env$finalWeightEmoticons[["Joy"]] + 0.4
        env$finalCountJoy[["Neutral"]] <- env$finalCountJoy[["Neutral"]] + 1
        env$finalWeightJoy[["Neutral"]] <- env$finalWeightJoy[["Neutral"]] + 0.4
        break()
      }
      if (identical(AllEmoticons[i], emoticonsData[j, 2]) & !is.na(emoticonsData[j, 2])) {
        env$finalCountEmoticons[["Sadness"]] <- env$finalCountEmoticons[["Sadness"]] + 1
        env$finalWeightEmoticons[["Sadness"]] <- env$finalWeightEmoticons[["Sadness"]] + 0.4
        env$finalCountSadness[["Neutral"]] <- env$finalCountSadness[["Neutral"]] + 1
        env$finalWeightSadness[["Neutral"]] <- env$finalWeightSadness[["Neutral"]] + 0.4
        break()
      }
      if (identical(AllEmoticons[i], emoticonsData[j, 3]) & !is.na(emoticonsData[j, 3])) {
        env$finalCountEmoticons[["Anger"]] <- env$finalCountEmoticons[["Anger"]] + 1
        env$finalWeightEmoticons[["Anger"]] <- env$finalWeightEmoticons[["Anger"]] + 0.4
        env$finalCountAnger[["Neutral"]] <- env$finalCountAnger[["Neutral"]] + 1
        env$finalWeightAnger[["Neutral"]] <- env$finalWeightAnger[["Neutral"]] + 0.4
        break()
      }
      if (identical(AllEmoticons[i], emoticonsData[j, 4]) & !is.na(emoticonsData[j, 4])) {
        env$finalCountEmoticons[["Disgust"]] <- env$finalCountEmoticons[["Disgust"]] + 1
        env$finalWeightEmoticons[["Disgust"]] <- env$finalWeightEmoticons[["Disgust"]] + 0.4
        env$finalCountDisgust[["Neutral"]] <- env$finalCountDisgust[["Neutral"]] + 1
        env$finalWeightDisgust[["Neutral"]] <- env$finalWeightDisgust[["Neutral"]] + 0.4
        break()
      }
      if (identical(AllEmoticons[i], emoticonsData[j, 5]) & !is.na(emoticonsData[j, 5])) {
        print("5")
        env$finalCountEmoticons[["Fear"]] <- env$finalCountEmoticons[["Fear"]] + 1
        env$finalWeightEmoticons[["Fear"]] <- env$finalWeightEmoticons[["Fear"]] + 0.4
        env$finalWeightFear[["Neutral"]] <- env$finalWeightFear[["Neutral"]] + 1
        env$finalWeightFear[["Neutral"]] <- env$finalWeightFear[["Neutral"]] + 0.4
        break()
      }
    }
  }
}

