install.packages("ggplot2")
install.packages("cluster")
install.packages("dplyr")
install.packages("factoextra")

library(ggplot2)
library(cluster)
library(dplyr)
library(factoextra)

#know your actual working directory
getwd();

#set your working directory
setwd("/Users/yago/Documents/Clase/DataMining-SpeedDating");

#retrieve data obtained from preprocessing
data <-read.csv("SpeedClean.csv", header=TRUE)

#Algorithm to create clusters dealing with mixed numeric and categorical variables
actives<-c(1:ncol(data))
dissimMatrix <- daisy(data[,c(2:ncol(data))], metric = "gower", stand=TRUE)
distMatrix<-dissimMatrix^2
cluster <- hclust(distMatrix,method="ward.D") #this should be a high cost operation but it's not. Maybe I'm doing something wrong?
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot
plot(cluster)

#this would be enough if we only had numerical variables
#cluster <- hclust(dist(as.matrix(data)),method="ward.D2")

#Calculate optimal number of clusters. Possible methods: "silhouette", "wss", "gap_stat". "gap_stat not working tho"
fviz_nbclust(data, hcut, method = "silhouette") +
geom_vline(xintercept = 3, linetype = 2)

#Assign the optimal number of clusters obtained from the functions above. It can also be any other number.
numClusters <- 3
clusterCut <- cutree(cluster, numClusters)
clusterCutRect <- rect.hclust(cluster, numClusters, border="red") 
clustersTable <- table(clusterCut, data$match)
clustersMatchTable <- table(clusterCut, data$match)

#get percentage of matches (yes/(yes+no)) for each cluster
getMatchChanceForEachCluster <- function(clustersMatchTable) {
  vector <- 1:nrow(clustersMatchTable)
  for (row in 1:nrow(clustersMatchTable)) {
    N <- clustersMatchTable[row, "N"];
    Y <- clustersMatchTable[row, "Y"];
    vector[row] <- (Y/(Y+N))*100
  }
  return (vector)
}
getMatchChanceForEachCluster(clustersMatchTable)

#count how many rows/elements/entries are there in each cluster.
cluster_append <- mutate(data, cluster = clusterCut)
count(cluster_append,cluster)


