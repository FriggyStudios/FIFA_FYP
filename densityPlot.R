library(ggplot2)
library(dplyr)

name <- "L. Messi"
varComparing <- "finishing"
ageRangeLeft <- 18
ageRangeRight <- 28
position <- "prefers_st"

df %>% 
  filter(df[,position] == "True") %>%
  filter(age >= ageRangeLeft, age <= ageRangeRight) %>%
  ggplot +
  aes_string(varComparing) +
  geom_density() +
  geom_vline(xintercept=df[df$name == name,varComparing],color = "blue") +
  coord_cartesian(xlim = c(0, 100)) 
