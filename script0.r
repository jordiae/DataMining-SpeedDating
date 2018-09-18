mydata <-read.csv("Speed.csv", header=TRUE,na.strings=c("","NA"))
sum(is.na(mydata))
na_count <-sapply(mydata, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
nrow(mydata)
sum(!is.na(mydata))
sum(is.na(mydata))/sum(!is.na(mydata))*100