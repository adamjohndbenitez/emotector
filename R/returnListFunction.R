# # returns and access list from function
# 
# listFunction <- function(a, b, c, d, e, f) {
#   a <- a + 1
#   b <- b + 1
#   c <- c + 1
#   d <- d + 1
#   e <- e + 1
#   f <- f + 1
#   c(a, b, c, d, e, f)
# }
# 
# getList <- listFunction(1, 2, 3, 4, 5, 6)
# 
# print(getList[2])
# 
# finalJoy <- c(count = c(lowest = 0, low = 1, neutral = 2, high = 3, higher = 4, highest = 5), weight = c(lowest = 0.0, low = 0.2, neutral = 0.4, high = 0.6, higher = 0.8, highest = 1.0))
# 
# print(finalJoy)
# finalJoy[count]
# finalJoy[weight]
# finalJoy["count.low"]
# finalJoy["count.highest"]
# finalJoy["weight.low"]
# finalJoy["weight.highest"]
# 
# emotionsCountFUnction <- function(finalJay) {
#   # lahiNa <- c(count = c(lowest = 0, low = 0, neutral = 0, high = 0, higher = 0, highest = 0), weight = c(lowest = 0, low = 0, neutral = 0, high = 0, higher = 0, highest = 0))
#   finalJay["count.highest"] <- finalJay["count.highest"] + 10
#   finalJay["weight.high"] <- finalJay["weight.high"] + 10
#   return(finalJay)
# }
# 
# finalJ <- c(count = c(lowest = 0, low = 1, neutral = 2, high = 3, higher = 4, highest = 5), weight = c(lowest = 0.0, low = 0.2, neutral = 0.4, high = 0.6, higher = 0.8, highest = 1.0))
# emotionsCountFUnction(finalJ)
# 
# # start
# # (1) Pass caller's environment. You can explicitly pass the parent environment and index into it. Try this:
# 
# f2a <- function(P, env = parent.frame()) {
#     env$calls <- env$calls + 1
#     print(env$calls)
#     return(P + env$c + env$d)
# }
# 
# a <- 1
# b <- 2
# # same as f1 except f2 removed and call to f2 replaced with call to f2a
# f1a <- function(){
#     c <- 3
#     d <- 4
#     calls <- 0
#     v <- vector()
#     for(i in 1:10){
#         v[i] <- f2a(P=0)
#         c <- c+1
#         d <- d+1
#       }
#      return(v)
# }
# f1a()
# # end
# 
# 
# # start
# # (2) Reset called function's environment is to reset the environment of f2b in f1b as shown here:
# f2b <- function(P) {
#     calls <<- calls + 1
#     print(calls)
#     return(P + c + d)
# }
# 
# a <- 1
# b <- 2
# # same as f1 except f2 removed, call to f2 replaced with call to f2b
# #  and line marked ## at the beginning is new
# f1b <- function(){
#     environment(f2b) <- environment() ##
#     c <- 3
#     d <- 4
#     calls <- 0
#     v <- vector()
#     for(i in 1:10){
#         v[i] <- f2b(P=0)
#         c <- c+1
#         d <- d+1
#       }
#      return(v)
# }
# f1b()
# # end