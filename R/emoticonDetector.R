emoticonDetector <- function() {
  # grep() - search for matches to argument pattern
  emoticonPattern <- "<U\\+[a-zA-Z0-9]*>"
  ed <- "<ed>"
  x <- "Give me chocolate and nobody gets hurt! <ed><U+00A0><U+00BD><ed><U+00B8><U+0082> <ed><U+00A0><U+00BC><ed><U+00BD><U+00AB> <ed><U+00A0><U+00BC><ed><U+00BD><U+00AB> <ed><U+00A0><U+00BC><ed><U+00BD><U+00AB> UpVee <ed><U+00A0><U+00BD><ed><U+00B1><U+0080> http://bit.ly/2kjXMKe"
  # values <- grep(pattern = pattern, x = x, value = TRUE)
  a <- grepl(pattern = emoticonPattern, x = x)
  
  if (a) {
    b <- grepl(pattern = ed, x = x)
    if (b) {
      values1 <- strsplit(x = x, split = "<ed>")
      # values2 <- strsplit(x = values1, split = "><")
      # values3 <- substr(values2, 2, nchar(values2)-1)
      print(values1)
      typeof(values1) 
      for (i in values1) {
        values2 <- strsplit(x = i, split = "><")
        values3 <- strsplit(x = i, split = "><")
        print(values2)
      }
      
    }
  }
  # ---------------------------------------------
  # values <- gsub(pattern, "replacement", x, ignore.case = FALSE, perl = FALSE,
  #    fixed = FALSE, useBytes = FALSE)
  
  # print(values[1])
  # print(values[2])
  
  # values1 <- gsub("<ed>", "", values[2], ignore.case = FALSE, perl = FALSE,
     # fixed = FALSE, useBytes = FALSE)
  # print(values1)
  # values2 <- strsplit(x = values[2], split = "<ed>")
  # values3 <- substr(values2, 2, nchar(values2)-1)
  # print(values3)
  # print(values2[1])
}
emoticonDetector()