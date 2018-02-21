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
    if(is.null(nationalities))
      nationalities <- levels(x$nationality)
    if(is.null(leagues))
      leagues <- levels(x$league)
    if(is.null(clubs))
      clubs <- levels(x$club)
    
    xFiltered <- xFiltered %>%
      subset(nationality %in% nationalities) %>%
      subset(league %in% leagues) %>%
      subset(club %in% clubs)
    
    xFiltered
}