library(class)

k <- knn1(df[,30:58], df[2,30:58], c(1:length(df[,1])))
df[k,2]