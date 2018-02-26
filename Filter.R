library(dplyr)

filterPlayers <- function(x,ageRange = NULL,value = NULL,positions = NULL,nationalities= NULL,leagues = NULL,clubs = NULL){
    xFiltered <- x
    if(!(is.null(ageRange) || (ageRange[1] <= 15 && ageRange[2] >= 50)))
      xFiltered <- xFiltered %>%
      filter(age >= ageRange[1], age <= ageRange[2])
  if(!(is.null(value) || (value[1] <= 0 && value[2] >= 150)))
      xFiltered <- xFiltered %>%
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
    if(!is.null(nationalities))
      xFiltered <- xFiltered %>%
      subset(nationality %in% nationalities)
    if(!is.null(leagues))
      xFiltered <- xFiltered %>%
      subset(league %in% leagues)
    if(!is.null(clubs))
      xFiltered <- xFiltered %>%
      subset(club %in% clubs)
    
    xFiltered
}