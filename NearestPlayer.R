library(dplyr)
library(class)

nearest <- function(x,compare,weights= NULL,lengthReturn = 10,error = TRUE) {
  
  if(is.null(compare)){
    return (NULL)
  }
  if(!is.null(compare[90]) && compare[90]== "False") {#prefers gk
    range <- 30:58
    x <- filter(x,prefers_gk == "False")
  }
  else{
    range <- 59:63
    x <- filter(x,prefers_gk == "True")
  }
  xWeighted <- x
  if(!is.null(weights)){
  for(i in 1:nrow(xWeighted)){
    for(j in range){
      xWeighted[i,j] <- as.numeric(xWeighted[[i,j]]) * as.numeric(weights[j])
    }
  }
}
  for(j in range){
    compare[j] <- as.numeric(compare[j]) * as.numeric((weights[j]))
  }
  
  dfTemp <- xWeighted
  players <- c()
  for (i in 1:lengthReturn) {
    kIndex <- dfTemp[ knn1(dfTemp[,range], compare[range], c(1:(length(dfTemp$ID)))) , ]$ID
    players <- c(players, kIndex)
    dfTemp <- dfTemp %>% filter(ID != kIndex)
  }
  output <- x[x$ID == players[1],]
  for(i in 2:lengthReturn){
    output <- rbind(output,x[x$ID == players[i],])
  }
  if(error){
    output$meanSquaredError <- NA
    for (i in 1:nrow(output)) {
      distanceSquared <- 0
      for(j in range){
        distanceSquared <- distanceSquared + (as.numeric(output[[i,j]])*as.numeric(weights[j])-as.numeric(compare[j]))*
          (as.numeric(output[[i,j]])*as.numeric(weights[j])-as.numeric(compare[j]))
      }
      output$meanSquaredError[i] <- distanceSquared/length(range)
    }
    output <- output[,c(ncol(output),1:(ncol(output)-1))]
  }
  output
}

