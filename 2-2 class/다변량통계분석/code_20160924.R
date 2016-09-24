stars(USairpollution, key.loc=c(15,2),cex=0.8, draw.segment=TRUE)
a=stars(USairpollution)

bball=read.csv("http://datasets.flowingdata.com/ppg2008.csv")
rownames(bball)=bball[,1]
bball=bball[,-1]
bball=as.matrix(bball)

heatmap(bball,Colv=NA,scale="column")

library(MASS)
attach(geyser)
density1=kde2d(waiting, duration,n=25)
image(density1,xlab="waiting",ylab="duration")

density2=kde2d(waiting, duration,n=100)
image(density2,xlab="waiting",ylab="duration")
contour(density2)


#### PCA
data=read.csv("open_closed.csv")
library(psych)
pairs.panels(data)