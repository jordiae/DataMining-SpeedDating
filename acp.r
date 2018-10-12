dd <-read.csv("SpeedClean.csv", header=TRUE)

# attach(dd)
# names(dd)

nums <- unlist(lapply(dd, is.numeric))  
dd_numeric <- dd[ , nums]
path <- "plots/9acp/"


#set a list of numerical variables

# PRINCIPAL COMPONENT ANALYSIS OF dd_numeric

pc1 <- prcomp(dd_numeric, scale=TRUE)
class(pc1)
attributes(pc1)

print(pc1)


# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?

pc1$sdev
inerProj<- pc1$sdev^2
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)

#Cummulated Inertia in subspaces, from first principal component to the 24th dimension subspace
png(paste(path,"a-cummulated-inertia-barplot.png",sep = ""))
barplot(100*cumsum(pc1$sdev[1:dim(dd_numeric)[2]]^2)/dim(dd_numeric)[2])
dev.off()
percInerAccum<-100*cumsum(pc1$sdev[1:dim(dd_numeric)[2]]^2)/dim(dd_numeric)[2]
percInerAccum

# 14th col = 79.921189 (80%)
# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)
nd = 14

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
Psi = pc1$x[,1:nd]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden = row.names(dd_numeric)
etiq = names(dd_numeric)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

for (axis_h in 1:(nd-1)) {
  for (axis_v in (axis_h+1):nd)  {
    axis_name <- paste("x-",axis_h,"_","y-",axis_v,sep = "")
    
    # PLOT OF INDIVIDUALS [APARTAT b 1]
    #select your axis
    eje1<-axis_h
    eje2<-axis_v
    
    png(paste(path,axis_name,"b-1-individulals.png",sep = ""))
    
    plot(Psi[,eje1],Psi[,eje2])
    text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
    axis(side=1, pos= 0, labels = F, col="cyan")
    axis(side=3, pos= 0, labels = F, col="cyan")
    axis(side=2, pos= 0, labels = F, col="cyan")
    axis(side=4, pos= 0, labels = F, col="cyan")
    dev.off()
    
    #library(rgl)
    #png(paste(path,axis_name,"b-1-individulals-3d.png",sep = ""))
    #plot3d(Psi[,1],Psi[,2],Psi[,3])
    #dev.off()
    
    #Projection of variables [APARTAT b 2]
    
    Phi = cor(dd_numeric,Psi)
    
    #select your axis
    
    X<-Phi[,eje1]
    Y<-Phi[,eje2]
    png(paste(path,axis_name,"b-2-proj-all-nums.png",sep = ""))
    plot(Psi[,eje1],Psi[,eje2],type="n")
    axis(side=1, pos= 0, labels = F)
    axis(side=3, pos= 0, labels = F)
    axis(side=2, pos= 0, labels = F)
    axis(side=4, pos= 0, labels = F)
    arrows(ze, ze, X, Y, length = 0.07,col="blue")
    text(X,Y,labels=etiq,col="darkblue", cex=0.7)
    dev.off()
    
    
    #zooms
    png(paste(path,axis_name,"b-2-zooms-proj-all-nums.png",sep = ""))
    plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(min(X,0),max(X,0)))
    axis(side=1, pos= 0, labels = F)
    axis(side=3, pos= 0, labels = F)
    axis(side=2, pos= 0, labels = F)
    axis(side=4, pos= 0, labels = F)
    arrows(ze, ze, X, Y, length = 0.07,col="blue")
    text(X,Y,labels=etiq,col="darkblue", cex=0.7)
    dev.off()
    
    
    
    #Now we project both cdgs of levels of a selected qualitative variable without
    #representing the individual anymore
    
    png(paste(path,axis_name,"b-2-match.png",sep = ""))
    plot(Psi[,eje1],Psi[,eje2],type="n")
    axis(side=1, pos= 0, labels = F, col="cyan")
    axis(side=3, pos= 0, labels = F, col="cyan")
    axis(side=2, pos= 0, labels = F, col="cyan")
    axis(side=4, pos= 0, labels = F, col="cyan")
    
    #select your qualitative variable: MATCH
    varcat<-dd[,"match"]
    fdic1 = tapply(Psi[,eje1],varcat,mean)
    fdic2 = tapply(Psi[,eje2],varcat,mean)
    
    #points(fdic1,fdic2,pch=16,col="blue", labels=levels(varcat))
    text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.7)
    dev.off()
    
    png(paste(path,axis_name,"b-2-dec.png",sep = ""))
    plot(Psi[,eje1],Psi[,eje2],type="n")
    axis(side=1, pos= 0, labels = F, col="cyan")
    axis(side=3, pos= 0, labels = F, col="cyan")
    axis(side=2, pos= 0, labels = F, col="cyan")
    axis(side=4, pos= 0, labels = F, col="cyan")
    
    #select your qualitative variable: DEC
    varcat<-dd[,"dec"]
    fdic1 = tapply(Psi[,eje1],varcat,mean)
    fdic2 = tapply(Psi[,eje2],varcat,mean)
    
    #points(fdic1,fdic2,pch=16,col="blue", labels=levels(varcat))
    text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.7)
    dev.off()
    
    png(paste(path,axis_name,"b-2-dec_o.png",sep = ""))
    plot(Psi[,eje1],Psi[,eje2],type="n")
    axis(side=1, pos= 0, labels = F, col="cyan")
    axis(side=3, pos= 0, labels = F, col="cyan")
    axis(side=2, pos= 0, labels = F, col="cyan")
    axis(side=4, pos= 0, labels = F, col="cyan")
    
    #select your qualitative variable: DEC_O
    varcat<-dd[,"dec_o"]
    fdic1 = tapply(Psi[,eje1],varcat,mean)
    fdic2 = tapply(Psi[,eje2],varcat,mean)
    
    #points(fdic1,fdic2,pch=16,col="blue", labels=levels(varcat))
    text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.7)
    dev.off()
    
    
    
    
    
    #nominal qualitative variables
    
    facts <- unlist(lapply(dd, is.factor))  
    dcat <- names(dd[ , facts])
    dcat <- dcat[dcat != "goal" & dcat !="go_out" & dcat != "date"]
    #dcat$goal <- NULL
    #dcat$go_out <- NULL
    #dcat$date <- NULL
    #divide categoricals in several graphs if joint representation saturates
    
    #build a palette with as much colors as qualitative variables
    
    #colors<-c("blue","red","green","orange","darkgreen")
    #install.packages("viridis")
    #library(viridis)
    #viridis_pal(option = "D")(length(dcat)) # n = number of colors seeked
    
    #alternative
    colors<-rainbow(length(dcat))
    
    c<-1
    for(k in dcat){
      seguentColor<-colors[c]
      fdic1 = tapply(Psi[,eje1],dd[,k],mean)
      fdic2 = tapply(Psi[,eje2],dd[,k],mean)
      
      text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.6)
      c<-c+1
    }
    legend("bottomleft",names(dd)[dcat],pch=1,col=colors, cex=0.6)
    
    #determine zoom level
    #use the scale factor or not depending on the position of centroids
    # ES UN FACTOR D'ESCALA PER DIBUIXAR LES FLETXES MES VISIBLES EN EL GRAFIC
    fm = round(max(abs(Psi[,1])))
    #  fm=20
    
    #scale the projected variables
    #X<-fm*U[,eje1]
    #Y<-fm*U[,eje2]
    X<-fm*Psi[,eje1]
    Y<-fm*Psi[,eje2]
    
    png(paste(path,axis_name,"b-2-all_cat.png",sep = ""))
    #represent numerical variables in background
    plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1), ylim=c(-3,1))
    #plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
    axis(side=1, pos= 0, labels = F, col="cyan")
    axis(side=3, pos= 0, labels = F, col="cyan")
    axis(side=2, pos= 0, labels = F, col="cyan")
    axis(side=4, pos= 0, labels = F, col="cyan")
    
    #add projections of numerical variables in background
    arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
    text(X,Y,labels=etiq,col="gray", cex=0.7)
    
    #add centroids
    c<-1
    for(k in dcat){
      seguentColor<-colors[c]
      
      fdic1 = tapply(Psi[,eje1],dd[,k],mean)
      fdic2 = tapply(Psi[,eje2],dd[,k],mean)
      
      #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
      text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.6)
      c<-c+1
    }
    legend("bottomleft",names(dd)[dcat],pch=1,col=colors, cex=0.6)
    
    
    #add ordinal qualitative variables. Ensure ordering is the correct
    
    # dordi<-c(8)
    # go_out = 31, date = 30 , goal = 29
    dordi <- c(29,30,31)
    
    
    levels(dd[,dordi[1]])
    #reorder modalities(GOAL): when required
    dd[,dordi[1]] <- factor(dd[,dordi[1]], ordered=TRUE,  levels= c("Other","Fun","Say","Meet","Date","Serious"))
    levels(dd[,dordi[1]])
    
    
    #reorder modalities(DATE): when required
    dd[,dordi[2]] <- factor(dd[,dordi[2]], ordered=TRUE,  levels= c("Unknown","Never", "SevYear","1Month", "2Month","1Week","2Week","SevWeek"))
    levels(dd[,dordi[2]])
    
    
    #reorder modalities(GO_OUT): when required
    dd[,dordi[3]] <- factor(dd[,dordi[3]], ordered=TRUE,  levels= c("Never", "SevYear","1Month", "2Month","1Week","2Week","SevWeek"))
    levels(dd[,dordi[3]])
    
    
    
    c<-1
    col <- c
    for(k in dordi){
      #seguentColor<-colors[col]
      seguentColor<-colors[c]
      fdic1 = tapply(Psi[,eje1],dd[,k],mean)
      fdic2 = tapply(Psi[,eje2],dd[,k],mean)
      
      #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
      #connect modalities of qualitative variables
      lines(fdic1,fdic2,pch=16,col=seguentColor)
      text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.6)
      c<-c+1
      col<-col+1
    }
    legend("topleft",names(dd)[dordi],pch=1,col=colors[1:length(dordi)], cex=0.6)
    
    dev.off()
    
    # PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
    # PROJECCIÓ OF INDIVIDUALS DIFFERENTIATING THE Dictamen
    # (we need a numeric Dictamen to color)
    # MATCH
    varcat=dd[,"match"]
    png(paste(path,axis_name,"b-2-match-ill-proj.png",sep = ""))
    plot(Psi[,1],Psi[,2],col=varcat)
    axis(side=1, pos= 0, labels = F, col="darkgray")
    axis(side=3, pos= 0, labels = F, col="darkgray")
    axis(side=2, pos= 0, labels = F, col="darkgray")
    axis(side=4, pos= 0, labels = F, col="darkgray")
    legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)
    
    
    # Overproject THE CDG OF  LEVELS OF varcat
    fdic1 = tapply(Psi[,1],varcat,mean)
    fdic2 = tapply(Psi[,2],varcat,mean)
    
    text(fdic1,fdic2,labels=levels(varcat),col="cyan", cex=0.75)
    dev.off()
    # DEC
    varcat=dd[,"dec"]
    png(paste(path,axis_name,"b-2-match-dec-proj.png",sep = ""))
    plot(Psi[,1],Psi[,2],col=varcat)
    axis(side=1, pos= 0, labels = F, col="darkgray")
    axis(side=3, pos= 0, labels = F, col="darkgray")
    axis(side=2, pos= 0, labels = F, col="darkgray")
    axis(side=4, pos= 0, labels = F, col="darkgray")
    legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)
    
    
    # Overproject THE CDG OF  LEVELS OF varcat
    fdic1 = tapply(Psi[,1],varcat,mean)
    fdic2 = tapply(Psi[,2],varcat,mean)
    
    text(fdic1,fdic2,labels=levels(varcat),col="cyan", cex=0.75)
    dev.off()
    
    # DEC_O
    varcat=dd[,"dec_o"]
    png(paste(path,axis_name,"b-2-match-ill-dec-o.png",sep = ""))
    plot(Psi[,1],Psi[,2],col=varcat)
    axis(side=1, pos= 0, labels = F, col="darkgray")
    axis(side=3, pos= 0, labels = F, col="darkgray")
    axis(side=2, pos= 0, labels = F, col="darkgray")
    axis(side=4, pos= 0, labels = F, col="darkgray")
    legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)
    
    
    # Overproject THE CDG OF  LEVELS OF varcat
    fdic1 = tapply(Psi[,1],varcat,mean)
    fdic2 = tapply(Psi[,2],varcat,mean)
    
    text(fdic1,fdic2,labels=levels(varcat),col="cyan", cex=0.75)
    dev.off()
  }
}

