library(ggplot2)

ggplot(data = df)
        + aes_string("prefers_st")
       + geom_bar()