
#Install packages
#Little test
install.packages("BaylorEdPsych")
install.packages("plyr")
install.packages("dplyr")

library(BaylorEdPsych)
install.packages("mvnmle")
library(mvnmle)
installed.packages("cluster")
library(cluster)
installed.packages("dplyr")
library("plyr")
library("dplyr")


install.packages("cluster")

getwd();

#set your working directory
setwd("/Users/yago/Documents/Clase/DataMining-SpeedDating");
data <-read.csv("SpeedClean.csv", header=TRUE)

#to deal with mixed numeric and categorical variables
actives<-c(2:ncol(data))
dissimMatrix <- daisy(data[,c(2:ncol(data))], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

cluster <- hclust(distMatrix,method="ward.D") #this should be a high cost operation but it's not. Maybe I'm doing something wrong?
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot

#this would be enough if we only had numerical variables
#cluster <- hclust(dist(as.matrix(data)),method="ward.D2")

plot(cluster) 


numClusters <- 7
clusterCut <- cutree(cluster, numClusters)

clusterCutRect <- rect.hclust(cluster, numClusters, border="red") 
clustersTable <- table(clusterCut, data$match)

clustersMatchTable <- table(clusterCut, data$match)

plot(clusterCut)


getMatchChanceForEachCluster <- function(clustersMatchTable) {
  vector <- 1:nrow(clustersMatchTable)
  for (row in 1:nrow(clustersMatchTable)) {
    N <- clustersMatchTable[row, "N"];
    Y <- clustersMatchTable[row, "Y"];
    vector[row] <- (Y/N)*100
  }
  return (vector)
}

getMatchChanceForEachCluster(clustersMatchTable)

