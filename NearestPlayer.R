library(dplyr)

namePlayer <- "L. Messi"
ageRangeLeft <- 18
ageRangeRight <- 35
priceRangeLeft <- 0
priceRangeRight <- 100
position <- "any"


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

positionFilter <- function(x,pos){
  if(pos == "any")
    x
  else
    filter(df[,position] == "True")
}

dfLocal <- df %>%
  positionFilter(position) %>%
  filter(name != namePlayer) %>%
  filter(age >= ageRangeLeft, age <= ageRangeRight) %>%
  filter(eur_value >= priceRangeLeft*1000000, eur_value <= priceRangeRight*1000000)
  

  dfLocal %>% 
    minDistance(df[df$name == namePlayer,],df$prefers_gk[df$name == namePlayer]) %>%
    order %>%
    displayNames