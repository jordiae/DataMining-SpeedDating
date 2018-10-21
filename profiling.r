getwd();
setwd("/Users/yago/Documents/Clase/DataMining-SpeedDating")
dd <- read.table("SpeedClean.csv",header=T, sep=";", dec='.');

names(dd)

attach(dd)


actives<-c(1:ncol(data))


#for numerical variables we'll just do the mean for each variable and cluster
numericalMeanOfEachCluster <- aggregate(data[, c(2,3,5,7,9,10,11,12,13,14,16,17,18,19,20,21,22,24,25,27,28,33,34,36)], list(data$cluster), mean)
numericalMeanOfEachCluster

#choose
active<-c(1,4,6,8,15,23,26,29,30,31,32,35)
active<-c(2,3,5,7,9,10,11,12,13,14,16,17,18,19,20,21,22,24,25,27,28,33,34,36)

Match    <- as.factor(data$match)

#createCPG(dd[,active], Tipo.trabajo)


plotConditionalTable<-function(data, res)
{
  if(ncol(data)==0)
  {
    cat("Number of columns of dataset is 0")	
    return()
  }#endif
  if(nrow(data)==0) 
  {
    cat("Number of rows of dataset is 0")  
    return()
  }#endif
  #proceed only if data frame is non empty
  
  #transform response variable into a suitable string for printing purposes
  response<-factor(res)
  
  #create an auxiliary matrix with as much rows as classes to keep the position of figures in the CPG
  nc<-length(levels(response))
  K<-dim(data)[2]
  ncells<-nc*K
  
  mat<- matrix(data=c(1:ncells),nrow= nc, ncol=K, byrow=FALSE)
  
  #ojo, que si esta buit el panell peta
  dev.off()
  layout(mat, widths= rep.int(1, K), heights= rep.int(1,nc))
  
  for (k in 1:K){
    Vnum<-data[,k]
    for(niv in levels(response)){
      print(niv)
      s<-subset(Vnum, response==niv)
      if(is.numeric(data[,k]))
      {  hist(s, main=paste(names(data)[k], niv))
        #evenctually add other summary statistics, like vc
      }else{
        barplot(table(s), las=3, cex.names=0.5, main=paste("Barplot of", names(data)[k]))
      }#endifelse
    }#end for niv       
  }#end for k
}#end plot conditional table      


#data do not contain the response variable

createCPG<- function(data, response)
{
  if (!is.factor(response)) 
  {
    cat("The variable ", names(response), " must be a factor" )	
  }
  else
  {
    #alerta! el maxim es 7 per fila
    #sembla que 3 per columna ho fa amb numeriques. Mes ja no se. Qualis donen problemes
    plotConditionalTable(data, response)
  }#end else
}#endcreateCPG



#Fer gran la finestra del R
createCPG(data[,active], as.factor(data$match))

#attach(data)

#fer creixer la finestra de plots
#control - per fer menor el tipus de lletra en R
createCPG(data[,active], as.factor(clusterCut))


levels(Match) <- c("si","no")

#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum,P){
  #freq dis of fac
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  #mitjanes x grups
  xk <- tapply(Xnum,P,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); 
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}


ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula);
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2]);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  zkj <- dpf/dvt; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}


dades<-data
#dades<-df
K<-dim(dades)[2]
par(ask=TRUE)

P<-clusterCut #antigament c2
nc<-length(levels(as.factor(P)))
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dades)))
nameP<-"Class"
n<-dim(dades)[1]

for(k in 1:K){
  if (is.numeric(dades[,k])){ 
    print(paste("Anàlisi per classes de la Variable:", names(dades)[k]))
    
    boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=TRUE)
    
    barplot(tapply(dades[[k]], P, mean),main=paste("Means of", names(dades)[k], "by", nameP ))
    abline(h=mean(dades[[k]]))
    legend(0,mean(dades[[k]]),"global mean",bty="n")
    print("Estadístics per groups:")
    for(s in levels(as.factor(P))) {print(summary(dades[P==s,k]))}
    o<-oneway.test(dades[,k]~P)
    print(paste("p-valueANOVA:", o$p.value))
    kw<-kruskal.test(dades[,k]~P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
    pvalk[,k]<-ValorTestXnum(dades[,k], P)
    print("p-values ValorsTest: ")
    print(pvalk[,k])      
  }else{
    #qualitatives
    print(paste("Variable", names(dades)[k]))
    table<-table(P,dades[,k])
    #   print("Cross-table")
    #   print(table)
    rowperc<-prop.table(table,1)
    
    colperc<-prop.table(table,2)
    #  print("Distribucions condicionades a files")
    # print(rowperc)
    
    marg <- table(as.factor(P))/n
    print(append("Categories=",levels(dades[,k])))
    plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(dades[,k])))
    for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
    
    #with legend
    plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(dades[,k])))
    for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
    legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
    
    #condicionades a classes
    print(append("Categories=",levels(dades[,k])))
    plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(dades[,k])))
    for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
    
    #with legend
    plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(dades[,k])))
    for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
    legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
    
    #amb variable en eix d'abcisses
    marg <-table(dades[,k])/n
    print(append("Categories=",levels(dades[,k])))
    plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(as.factor(P))))
    for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c]) }
    
    #with legend
    plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
    legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
    
    #condicionades a columna 
    plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(as.factor(P))))
    for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c]) }
    
    #with legend
    plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
    legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
    
    table<-table(dades[,k],P)
    print("Cross Table:")
    print(table)
    print("Distribucions condicionades a columnes:")
    print(colperc)
    
    #diagrames de barres apilades                                         
    
    paleta<-rainbow(length(levels(dades[,k])))
    barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
    
    barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
    legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
    
    #diagrames de barres adosades
    barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta )
    
    barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta)
    legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
    
    print("Test Chi quadrat: ")
    print(chisq.test(dades[,k], as.factor(P)))
    
    print("valorsTest:")
    print( ValorTestXquali(P,dades[,k]))
  }
}#endfor



################ Arreglar pvalues


for (c in 1:length(levels(as.factor(P)))) { if(!is.na(levels(as.factor(P))[c])){print(paste("P.values per class:",levels(as.factor(P))[c])); print(sort(pvalk[c,]), digits=3) }}

#afegir la informacio de les modalitats de les qualitatives a la llista de pvalues i fer ordenacio global

#saving the dataframe in an external file
#write.table(dd, file = "credscoClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)


