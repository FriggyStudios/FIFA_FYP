library(dplyr)

legacy_nearest <- function(x,compare,indexReturn = FALSE) {
  distances <-  c()
  if(compare[181] == "False") #prefers gk
    range <- 30:58
  else
    range <- 59:63
  for (i in 1:nrow(x)) {
    distance <- 0
    for(j in range){
      distance <- distance + abs(as.numeric(x[[i,j]])-as.numeric(compare[j]))
    }
    distances <- c(distances,distance)
  }
  if(indexReturn)
    (order(distances))
  else
    x[(order(distances)),]
}
