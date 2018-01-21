
pos_prefer <- function(player){
  pref_pos <- list()
  j <- 1
  for(i in 155:181){
    if(df[player,i] == "True"){
      pref_pos[[j]] <- names(df)[i]
      j <- j + 1
    }
  }
  pref_pos
}
pref_positions <- c()
for(i in 1:17994){
    pref_positions[[i]] <- pos_prefer(i)
}
df2 <- df
for(i in 155:181){
  df2[[i]] <- NULL
}
df2$pref_pos <- pref_positions
