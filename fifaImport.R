df <- read.csv("fifaPlayers.csv", 
                 header = TRUE,
                 sep = ",")
for( i in 64:154){
 df[,64] = NULL 
}