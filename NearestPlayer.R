library(dplyr)
library(class)

nearest <- function(x,compare,weights) {
  distances <-  c()
  if(compare[181] == "False") #prefers gk
    range <- 30:58
  else
    range <- 59:63
  xWeighted <- x
  for(i in 1:nrow(xWeighted)){
    for(j in range){
      xWeighted[i,j] <- as.numeric(xWeighted[[i,j]]) * as.numeric(weights[j])
    }
  }
  for(j in range){
    compare[j] <- as.numeric(compare[j]) * as.numeric((weights[j]))
  }
  
  dfTemp <- xWeighted
  players <- c()
  for (i in 1:10) {
    kIndex <- dfTemp[ knn1(dfTemp[,range], compare[range], c(1:(length(dfTemp[,1])))) , ]$ID
    players <- c(players,kIndex)
    dfTemp <- dfTemp %>% filter(ID != kIndex)
  }
  output <- x[x$ID == players[1],]
  for(i in 2:10){
    output <- rbind(output,x[x$ID == players[i],])
}
  
  output
}

