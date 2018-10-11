getwd();

#set your working directory
setwd("/Users/yago/Documents/Clase/DataMining-SpeedDating");
data <-read.csv("SpeedClean.csv", header=TRUE)

#to deal with mixed numeric and categorical variables
actives<-c(2:ncol(c))
dissimMatrix <- daisy(data[,c(2:36)], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D") #this should be a high cost operation but it's not. Maybe I'm doing something wrong?
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot


#this would be enough if we only had numerical variables
#clusters <- hclust(dist(as.matrix(data)),method="ward.D2")


plot(clusters) 