# Library
library(fmsb)

# Create data: note in High school for Jonathan:
data=as.data.frame(df[c(1,7),30:63])
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(99,1) , rep(0,10) , data)

# Custom the radarChart !
radarchart( data  , axistype=1 ,
            
            #custom polygon
            pcol=c(rgb(0.2,0.5,0.5,0.8),rgb(0.8,0.2,0.2,0.8)) , pfcol=c(rgb(0.2,0.5,0.5,0.4),rgb(0.8,0.2,0.2,0.4)) , plwd=4 ,
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,99,25), cglwd=0.8,
            
            #custom labels
            vlcex=0.65
)