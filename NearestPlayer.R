library(dplyr)

namePlayer <- "De Gea"
ageRangeLeft <- 18
ageRangeRight <- 35
position <- "prefers_gk"

minDistance <- function(x,compare,gk) {
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
  distances
}

displayNames <- function(x){
    print(dfLocal$name[x[1:10]])
}

dfLocal <- df %>%
  filter(df[,position] == "True") %>%
  filter(name != namePlayer) %>%
  filter(age >= ageRangeLeft, age <= ageRangeRight)

  dfLocal %>% 
    minDistance(df[df$name == namePlayer,],df$prefers_gk[df$name == namePlayer]) %>%
    order %>%
    displayNames