library(dplyr)

legacy_nearest <- function(x,compare) {
  distances <-  c()
  if(compare[181] == "False") #prefers gk
    range <- 30:58
  else
    range <- 59:63
  for (i in 1:nrow(x)) {
    distance <- 0
    for(j in range){
      distance <- distance + (as.numeric(x[[i,j]])*(as.numeric(x[[i,j]])
                            -(as.numeric(compare[j]))*(as.numeric(compare[j]))))
    }
    distances <- c(distances,distance)
  }
    x[(order(distances)),]
}
