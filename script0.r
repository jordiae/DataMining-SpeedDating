mydata <-read.csv("Speed.csv", header=TRUE,na.strings=c("","NA"))
sum(is.na(mydata))
na_count <-sapply(mydata, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
nrow(mydata)
sum(!is.na(mydata))
sum(is.na(mydata))/sum(!is.na(mydata))*100

mydata2 <- mydata [!(mydata$wave==5 | mydata$wave ==6 | mydata$wave ==7 | mydata$wave ==8 | mydata$wave ==9 | mydata$wave ==12 | mydata$wave ==13 | mydata$wave ==14 | mydata$wave ==18 | mydata$wave ==19 | mydata$wave ==20 | mydata$wave ==21),]

mydata3 <- subset( mydata2, select = -c( attr1_s : amb5_3 ))