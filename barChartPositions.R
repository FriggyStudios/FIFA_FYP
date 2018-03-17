library(ggplot2)


renderBar <- function(){
  
  d <- data.frame(positions=c("Goalkeeper", "Centre Back",
                                  "Left Back","Right Back","Centre Midfield","Left Wing",
                                  "Right Wing","Striker"),
                  number=c(length(ordered_gk$ID),length(ordered_cb$ID),length(ordered_lb$ID),
                           length(ordered_rb$ID), length(ordered_cm$ID), length(ordered_lm$ID),
                           length(ordered_rm$ID), length(ordered_st$ID)))

  ggplot(data = d,aes(x = positions,y = number)) +
  geom_bar(stat="identity", position=position_dodge())

}

renderBar()