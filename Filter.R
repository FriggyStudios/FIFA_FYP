filterPlayers <- function(x,ageRange,value,positions,nationalities,leagues,clubs){
    xFiltered <- x %>% 
    filter(age >= ageRange[1], age <= ageRange[2]) %>%
    filter(eur_value >= value[1]*1000000, eur_value <= value[2]*1000000)
    
    playersRemove <- c()
    if(!is.null(positions)){
      for(i in 1:length(xFiltered[,1])){
        remove <- T
        for(j in 1:length(positions)){
          if(xFiltered[i,positions[j]] == "True")
          {
            remove <- F
            break
          }
        }
        if(remove)
          playersRemove <- c(playersRemove,i)
      }
      xFiltered <- xFiltered[-playersRemove,]
      playersRemove <- c()
    }
    
    xFiltered <- xFiltered %>%
      filterCondition("nationality",nationalities) %>%
      filterCondition("league",leagues) %>%
      filterCondition("club",clubs)
    
    xFiltered
}

filterCondition <- function(x,col,names){
  playersRemove <- c()
  if(!is.null(names)){
    for(i in 1:length(x[,1])){
      remove <- T
      for(j in 1:length(names)){
        if(x[i,col] == names[j]){
          remove <- F
          break
        }
      }
      if(remove)
        playersRemove <- c(playersRemove,i)
    }
    x <- x[-playersRemove,]
  }
  x
}