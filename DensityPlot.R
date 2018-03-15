library(ggplot2)
library(dplyr)

name <- "P. Pogba"
varComparing <- "finishing"
ageRangeLeft <- 16
ageRangeRight <- 35
priceRangeLeft <- 0
priceRangeRight <- 1000
position <- c("any")
nationality <- c("Republic of Ireland")
league <- c("any")
club <- c("any")

df %>% 
  filterPlayers(c(ageRangeLeft,ageRangeRight),
                c(priceRangeLeft,priceRangeRight),position,nationality,
                league,club) %>%
  ggplot +
  aes_string(varComparing) +
  geom_density() +
  geom_vline(xintercept=df[df$name == name,varComparing],color = "blue") +
  coord_cartesian(xlim = c(0, 100))
