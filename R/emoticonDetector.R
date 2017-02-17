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
      print(typeof(values1))
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


emoticonDetector2 <- function() {
  exp1 <- "My future home looks exactly as I dreamt it! What about yours?\xed��\xed�\u009f\xed��\xed�\u009f\nCheck this out \u2b07\nhttps://sillyswing.com/"
  exp2 <- "As soon as you'll learn these you'll be a better person ;) \u2b07 UpVee\xed��\xed�� https://bit.ly/2lbLyrf"
  exp3 <- "Make now you before & after photos! \xed��\xed�\u008d \xed��\xed�\u008d \xed��\xed�\u008d\nCheck this out \u2b07\nhttps://sillyswing.com/beforeandafter"
  # m <- regexpr(pattern = "(u00[a-zA-Z0-9]*)", text = exp1)
  # n <- regmatches(x = exp1, m = m)
  # n <- as.character(exp1)
  n <- as.
  print(n)
}
emoticonDetector2()
