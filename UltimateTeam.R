library(dplyr)

orderPosition <- function(positionToOrder){
  range <- c(30:58,59:63)
  if(positionToOrder[1] == "prefers_gk")
    godPlayer <- df[5,]
  else
    godPlayer <- df[1,]
    for(i in range){
      godPlayer[i] <- 100
    }
  return (df %>% 
    filterPlayers(positions = positionToOrder) %>%
    nearest(godPlayer,indexReturn=F))
}

addPlayer <- function(team,rank){
  for(i in 1:nrow(rank)){
    if(!(rank[i,2] %in% team[,2])){
      team <- team %>%
        rbind(rank[i,])
      return (team)
    }
  }
  return (team)
}

makeTeam <-function(gkRank,cbRank,lbRank,rbRank,cmRank,lmRank,rmRank,stRank){
  team <- data.frame()
  
  team <- team %>%
    rbind(gkRank[1,])
  team <- team %>% 
    addPlayer(lbRank) %>%
    addPlayer(cbRank) %>%
    addPlayer(cbRank) %>%
    addPlayer(rbRank) %>%
    addPlayer(lmRank) %>%
    addPlayer(cmRank) %>%
    addPlayer(cmRank) %>%
    addPlayer(rmRank) %>%
    addPlayer(stRank) %>%
    addPlayer(stRank)
    
  team$position <- NA
  team <- team[,c(ncol(team),1:(ncol(team)-1))]
  team$position[1] <- "GK"
  team$position[2] <- "LB"
  team$position[3] <- "CB"
  team$position[4] <- "CB"
  team$position[5] <- "RB"
  team$position[6] <- "LM"
  team$position[7] <- "CM"
  team$position[8] <- "CM"
  team$position[9] <- "RM"
  team$position[10] <- "ST"
  team$position[11] <- "ST"
  return (team)
}


#ordered_gk <- orderPosition(c("prefers_gk"))
#ordered_cb <- orderPosition(c("prefers_cb"))
#ordered_lb <- orderPosition(c("prefers_lb"))
#ordered_rb <- orderPosition(c("prefers_rb"))
#ordered_cm <- orderPosition(c("prefers_cm"))
#ordered_lm <- orderPosition(c("prefers_lm","prefers_lw"))
#ordered_rm <- orderPosition(c("prefers_rw","prefers_rm"))
#ordered_st <- orderPosition(c("prefers_cf","prefers_st"))
