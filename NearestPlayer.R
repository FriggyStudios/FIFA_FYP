library(dplyr)

nearest <- function(x,compare,gk) {
  distances <-  c()
  if(gk == "False")
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
  (df %>% 
    subset(full_name %in% x[(order(distances))[1:10],"full_name"]) )
}