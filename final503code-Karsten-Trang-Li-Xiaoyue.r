library(shapefiles)
library(ggplot2)
library(maps)
library(animation)
library(GGally)
library(vegan)
library(tourr)
library(rgdal)
library(mclust)
library(xtable)
library(cluster)
library(fpc)
library(clv)

#######################
### Import the data ###
#######################
#bird = read.csv('http://streaming.stat.iastate.edu/dataexpo/2011/resources/data/birds.csv')
birds=read.dbf('DeadLiveERDC20101221_draworder.dbf')$dbf
birds$Week=as.integer(birds$DateRetrie-14734)%/%7+1
birds$Month=as.integer(substring(as.character(birds$Date),6,7))
birds$Gridx=as.integer(cut(birds$Longitude,seq(-96,-81.5,0.5)))
birds$Gridy=as.integer(cut(birds$Latitude,24:31))
birds$Nspecies=as.factor(substring(birds$Species,unlist(lapply(gregexpr(" ",birds$Species),function(x){x[length(x)]}))+1))
CoreSpecies=names(sort(table(birds$Nspecies)))[63:66]
birds$Group=as.factor(sapply(as.character(birds$Nspecies),function(x){ifelse(x %in% CoreSpecies,x,"other")}))
birds$Group=factor(birds$Group,levels=levels(birds$Group)[c(2,4,5,1,3)])
birds=birds[,c(5,4,13,14,2,11,12,3,15,16,6,7)]
colnames(birds)[5]="Date"
birds$Distance=sqrt((birds$Longitude+88.37)^2+(birds$Latitude-28.74)^2)

## Plots
qplot(Distance,data=birds,geom='histogram',fill=Group)
par(xpd=TRUE)
birds$Oiling=factor(birds$Oiling,levels=levels(birds$Oiling)[3:1])
mosaicplot(table(birds$Oiling,birds$Condition),col=T,main='',cex=1.4)


###################
### Aggregation ###
###################

## Aggregate by Week ##
total=data.frame(cbind(table(birds[,c(6,12)]),table(birds[,c(6,11)])))
total$DeathPct=total$Dead/(total$Dead+total$Live)*100
total$OiledPct=total$`Visibly.Oiled`/(total$Dead+total$Live)*100

Gull=data.frame(cbind(table(birds[birds$Group=='Gull',c(6,12)]),table(birds[birds$Group=='Gull',c(6,11)])))
Gull$DeathPct=Gull$Dead/(Gull$Dead+Gull$Live)*100
Gull$OiledPct=Gull$`Visibly.Oiled`/(Gull$Dead+Gull$Live)*100

Pelican=data.frame(cbind(table(birds[birds$Group=='Pelican',c(6,12)]),table(birds[birds$Group=='Pelican',c(6,11)])))
Pelican$DeathPct=Pelican$Dead/(Pelican$Dead+Pelican$Live)*100
Pelican$OiledPct=Pelican$`Visibly.Oiled`/(Pelican$Dead+Pelican$Live)*100

Tern=data.frame(cbind(table(birds[birds$Group=='Tern',c(6,12)]),table(birds[birds$Group=='Tern',c(6,11)])))
Tern$DeathPct=Tern$Dead/(Tern$Dead+Tern$Live)*100
Tern$OiledPct=Tern$`Visibly.Oiled`/(Tern$Dead+Tern$Live)*100

Gannet=data.frame(cbind(table(birds[birds$Group=='Gannet',c(6,12)]),table(birds[birds$Group=='Gannet',c(6,11)])))
Gannet$DeathPct=Gannet$Dead/(Gannet$Dead+Gannet$Live)*100
Gannet$OiledPct=Gannet$`Visibly.Oiled`/(Gannet$Dead+Gannet$Live)*100

other=data.frame(cbind(table(birds[birds$Group=='other',c(6,12)]),table(birds[birds$Group=='other',c(6,11)])))
other$DeathPct=other$Dead/(other$Dead+other$Live)*100
other$OiledPct=other$`Visibly.Oiled`/(other$Dead+other$Live)*100

AggWeek=data.frame(matrix(0,nrow=27,ncol=42))
AggWeek[,1:7]=total
AggWeek[rownames(Gull),8:14]=Gull
AggWeek[rownames(Pelican),15:21]=Pelican
AggWeek[rownames(Tern),22:28]=Tern
AggWeek[rownames(Gannet),29:35]=Gannet
AggWeek[rownames(other),36:42]=other
colnames(AggWeek)=paste(rep(c('total','Gull','Pelican','Tern','Gannet','other'),each=7),rep(c('Dead','Live','Not.Visibly.Oiled','Unknown','Visibly.Oiled','DeathPct','OiledPct'),length=42),sep='_')

## Aggregate by Month ##
total=data.frame(cbind(table(birds[,c(7,12)]),table(birds[,c(7,11)])))
total$DeathPct=total$Dead/(total$Dead+total$Live)*100
total$OiledPct=total$`Visibly.Oiled`/(total$Dead+total$Live)*100

Gull=data.frame(cbind(table(birds[birds$Group=='Gull',c(7,12)]),table(birds[birds$Group=='Gull',c(7,11)])))
Gull$DeathPct=Gull$Dead/(Gull$Dead+Gull$Live)*100
Gull$OiledPct=Gull$`Visibly.Oiled`/(Gull$Dead+Gull$Live)*100

Pelican=data.frame(cbind(table(birds[birds$Group=='Pelican',c(7,12)]),table(birds[birds$Group=='Pelican',c(7,11)])))
Pelican$DeathPct=Pelican$Dead/(Pelican$Dead+Pelican$Live)*100
Pelican$OiledPct=Pelican$`Visibly.Oiled`/(Pelican$Dead+Pelican$Live)*100

Tern=data.frame(cbind(table(birds[birds$Group=='Tern',c(7,12)]),table(birds[birds$Group=='Tern',c(7,11)])))
Tern$DeathPct=Tern$Dead/(Tern$Dead+Tern$Live)*100
Tern$OiledPct=Tern$`Visibly.Oiled`/(Tern$Dead+Tern$Live)*100

Gannet=data.frame(cbind(table(birds[birds$Group=='Gannet',c(7,12)]),table(birds[birds$Group=='Gannet',c(7,11)])))
Gannet$DeathPct=Gannet$Dead/(Gannet$Dead+Gannet$Live)*100
Gannet$OiledPct=Gannet$`Visibly.Oiled`/(Gannet$Dead+Gannet$Live)*100

other=data.frame(cbind(table(birds[birds$Group=='other',c(7,12)]),table(birds[birds$Group=='other',c(7,11)])))
other$DeathPct=other$Dead/(other$Dead+other$Live)*100
other$OiledPct=other$`Visibly.Oiled`/(other$Dead+other$Live)*100

AggMonth=data.frame(matrix(0,nrow=7,ncol=42))
rownames(AggMonth)=5:11
AggMonth[,1:7]=total
AggMonth[rownames(Gull),8:14]=Gull
AggMonth[rownames(Pelican),15:21]=Pelican
AggMonth[rownames(Tern),22:28]=Tern
AggMonth[rownames(Gannet),29:35]=Gannet
AggMonth[rownames(other),36:42]=other
colnames(AggMonth)=paste(rep(c('total','Gull','Pelican','Tern','Gannet','other'),each=7),rep(c('Dead','Live','Not.Visibly.Oiled','Unknown','Visibly.Oiled','DeathPct','OiledPct'),length=42),sep='_')
rownames(AggMonth)=c('May','Jun','Jul','Aug','Sept','Oct','Nov')

## Aggregate by Location ##
states <- map_data("state")
plot(birds$Longitude,birds$Latitude,pch=20,xlab='longitude',ylab='latitude')
abline(h=25:31,v=seq(-96,-81,0.5),lty=3)
points(-88.4,28.7,col=2,pch=17,cex=1.2)

birds$Week=factor(birds$Week)
ggplot(data=birds,aes(x=Longitude,y=Latitude)) + geom_tile(data=loc,aes(x=long,y=lat),fill='lightgrey') + geom_point(aes(colour=Week),alpha=0.3,size=3) + geom_path(data=states,aes(x=long,y=lat),alpha=0.3) + geom_hline(aes(yintercept=24:32),linetype=3) + geom_vline(aes(xintercept=seq(-99,-80,0.5)),linetype=3) + xlim(c(-98,-81)) + ylim(c(24,31)) + geom_point(aes(x=-88.4,y=28.7),col='red',size=4,shape=17) + geom_text(aes(x=-88.4,y=28.7,label="Deepwater Horizon Oil Platform"),col='red',size=4,hjust=-0.05, vjust=1)+ geom_text(aes(x=-90,y=26,label="Gulf of Maxico"),col='grey',size=16) + geom_text(aes(x=c(-97,-92,-82.5),y=c(30,30.5,30),label=c("TEXAS","LOUISIANA","FLORIDA")),col='grey',size=8)

plot(birds$Longitude,birds$Latitude,xlim=c(-91,-84),ylim=c(27,31),pch=20)
abline(h=27:31,v=seq(-91,-84,0.5),lty=3)
points(-88.4,28.7,col=2,pch=17,cex=1.2)

total=ddply(birds,.(Gridx,Gridy),summarise,
total.Count=length(Condition),
total.Dead=sum(Condition=='Dead'),
total.Live=sum(Condition=='Live'),
total.Not.Visibly.Oiled=sum(Oiling=='Not Visibly Oiled'),
total.Unknown=sum(Oiling=='Unknown'),
total.Visibly.Oiled=sum(Oiling=='Visibly Oiled'),
total.DeathPct=sum(Condition=='Dead')/length(Condition)*100,
total.OiledPct=sum(Oiling=='Visibly Oiled')/length(Condition)*100
)

totalspecies=ddply(birds,.(Gridx,Gridy,Group),summarise,
Count=length(Condition),
Dead=sum(Condition=='Dead'),
Live=sum(Condition=='Live'),
Not.Visibly.Oiled=sum(Oiling=='Not Visibly Oiled'),
Unknown=sum(Oiling=='Unknown'),
Visibly.Oiled=sum(Oiling=='Visibly Oiled'),
DeathPct=sum(Condition=='Dead')/length(Condition)*100,
OiledPct=sum(Oiling=='Visibly Oiled')/length(Condition)*100
)
Gull=totalspecies[totalspecies$Group=='Gull',-3]
Pelican=totalspecies[totalspecies$Group=='Pelican',-3]
Tern=totalspecies[totalspecies$Group=='Tern',-3]
Gannet=totalspecies[totalspecies$Group=='Gannet',-3]
other=totalspecies[totalspecies$Group=='other',-3]

AggLoc=data.frame(matrix(0,nrow=47,ncol=50))
colnames(AggLoc)[3:50]=paste(rep(c('total','Gull','Pelican','Tern','Gannet','other'),each=8),rep(c('Count','Dead','Live','Not.Visibly.Oiled','Unknown','Visibly.Oiled','DeathPct','OiledPct'),length=48),sep='_')
colnames(AggLoc)[1:2]=colnames(total)[1:2]
AggLoc[,1:10]=total[,1:10]
AggLoc[which(apply(total[,1:2],1,paste,collapse='_') %in% apply(Gull[,1:2],1,paste,collapse='_')),11:18]=Gull[3:10]
AggLoc[which(apply(total[,1:2],1,paste,collapse='_') %in% apply(Pelican[,1:2],1,paste,collapse='_')),19:26]=Pelican[3:10]
AggLoc[which(apply(total[,1:2],1,paste,collapse='_') %in% apply(Tern[,1:2],1,paste,collapse='_')),27:34]=Tern[3:10]
AggLoc[which(apply(total[,1:2],1,paste,collapse='_') %in% apply(Gannet[,1:2],1,paste,collapse='_')),35:42]=Gannet[3:10]
AggLoc[which(apply(total[,1:2],1,paste,collapse='_') %in% apply(other[,1:2],1,paste,collapse='_')),43:50]=other[3:10]


## Aggregate by Location & Week ##
total=ddply(birds,.(Gridx,Gridy,Week),summarise,
total.Count=length(Condition),
total.Dead=sum(Condition=='Dead'),
total.Live=sum(Condition=='Live'),
total.Not.Visibly.Oiled=sum(Oiling=='Not Visibly Oiled'),
total.Unknown=sum(Oiling=='Unknown'),
total.Visibly.Oiled=sum(Oiling=='Visibly Oiled'),
total.DeathPct=sum(Condition=='Dead')/length(Condition)*100,
total.OiledPct=sum(Oiling=='Visibly Oiled')/length(Condition)*100
)

totalspecies=ddply(birds,.(Gridx,Gridy,Group,Week),summarise,
Count=length(Condition),
Dead=sum(Condition=='Dead'),
Live=sum(Condition=='Live'),
Not.Visibly.Oiled=sum(Oiling=='Not Visibly Oiled'),
Unknown=sum(Oiling=='Unknown'),
Visibly.Oiled=sum(Oiling=='Visibly Oiled'),
DeathPct=sum(Condition=='Dead')/length(Condition)*100,
OiledPct=sum(Oiling=='Visibly Oiled')/length(Condition)*100
)
Gull=totalspecies[totalspecies$Group=='Gull',-3]
Pelican=totalspecies[totalspecies$Group=='Pelican',-3]
Tern=totalspecies[totalspecies$Group=='Tern',-3]
Gannet=totalspecies[totalspecies$Group=='Gannet',-3]
other=totalspecies[totalspecies$Group=='other',-3]

AggLocWeek=data.frame(matrix(0,nrow=435,ncol=51))
colnames(AggLocWeek)[4:51]=paste(rep(c('total','Gull','Pelican','Tern','Gannet','other'),each=8),rep(c('Count','Dead','Live','Not.Visibly.Oiled','Unknown','Visibly.Oiled','DeathPct','OiledPct'),length=48),sep='_')
colnames(AggLocWeek)[1:3]=colnames(total)[1:3]
AggLocWeek[,1:11]=total
AggLocWeek[which(apply(total[,1:3],1,paste,collapse='_') %in% apply(Gull[,1:3],1,paste,collapse='_')),12:19]=Gull[4:11]
AggLocWeek[which(apply(total[,1:3],1,paste,collapse='_') %in% apply(Pelican[,1:3],1,paste,collapse='_')),20:27]=Pelican[4:11]
AggLocWeek[which(apply(total[,1:3],1,paste,collapse='_') %in% apply(Tern[,1:3],1,paste,collapse='_')),28:35]=Tern[4:11]
AggLocWeek[which(apply(total[,1:3],1,paste,collapse='_') %in% apply(Gannet[,1:3],1,paste,collapse='_')),36:43]=Gannet[4:11]
AggLocWeek[which(apply(total[,1:3],1,paste,collapse='_') %in% apply(other[,1:3],1,paste,collapse='_')),44:51]=other[4:11]

### Export ###
#save(birds,file='birds.rda')
#write.csv(birds,file='birds.csv',row.names=FALSE)
#write.csv(AggWeek,file='AggWeek.csv')
#write.csv(AggMonth,file='AggMonth.csv')
#write.csv(AggLoc,file='AggLocation.csv',row.names=FALSE)
#write.csv(AggLocWeek,file='AggLocWeek.csv',row.names=FALSE)


#########################
### Exploration Plots ###
#########################

states <- map_data("state")
week = AggWeek
loc = AggLoc
locweek = AggLocWeek

loc$lat <- loc$Gridy + 23.5
loc$long <- (loc$Gridx/2) - 96.25
locweek$lat <- locweek$Gridy + 23.5
locweek$long <- (locweek$Gridx/2) - 96.25

## Plots of coastline rates ##
map <- ggplot(loc, aes(x=Gridx, y=Gridy))
map + geom_tile(aes(fill=total_DeathPct))
map + geom_tile(aes(fill=total_OiledPct))

map <- ggplot(loc, aes(x=long, y=lat))
map + geom_tile(aes(fill=total_DeathPct)) + 
 	geom_path(data=states,aes(x=long,y=lat)) +
	xlim(c(-100,-80)) + ylim(c(24,32)) 
map + geom_tile(aes(fill=total_OiledPct)) + 
 	geom_path(data=states,aes(x=long,y=lat)) +
	xlim(c(-100,-80)) + ylim(c(24,32)) 
	
## Facet by Time ##
# plot coastline change in percent oiled over the weeks
map2 <- ggplot(locweek, aes(x=long, y=lat))
m <- map2 + geom_tile(aes(fill=total_OiledPct)) +
 	geom_path(data=states,aes(x=long,y=lat)) +
	xlim(c(-100,-80)) + ylim(c(24,32)) 
m + facet_wrap(~ Week)	

## Animation ##

saveLatex({
for (i in 1:27) {
	tmpdata = locweek[locweek$Week==i,]
	map2 <- ggplot(tmpdata, aes(x=long, y=lat), main=paste('Week',i),xlab='longitude',ylab='latitude')
	m <- map2 + geom_tile(aes(fill=total_OiledPct)) +
		geom_path(data=states,aes(x=long,y=lat)) +
		xlim(c(-100,-80)) + ylim(c(24,32)) + xlab("Longitude") + ylab("Latitude") +
		opts(aspect.ratio=1/2,title = paste("Week",i)) +
		scale_fill_gradient(limits=c(0, 100))
	print(m)
}
}, 27, ani.dev='png', ani.ext='png', ani.width=1000, ani.height=500,img.name='OiledPct',caption='Oiled Percent')

saveLatex({
for (i in 1:27) {
	tmpdata = locweek[locweek$Week==i,]
	map2 <- ggplot(tmpdata, aes(x=long, y=lat), main=paste('Week',i),xlab='longitude',ylab='latitude')
	m <- map2 + geom_tile(aes(fill=total_DeathPct)) +
		geom_path(data=states,aes(x=long,y=lat)) +
		xlim(c(-100,-80)) + ylim(c(24,32)) + xlab("Longitude") + ylab("Latitude") +
		opts(aspect.ratio=1/2,title = paste("Week",i)) +
		scale_fill_gradient(limits=c(0, 100))
	print(m)
}
}, 27, ani.dev='png', ani.ext='png', ani.width=1000, ani.height=500,img.name='DeathPct',caption='Death Percent')


##################
### clustering ###
##################
# Read in and Prepare Data
mydata=AggLoc[,c(1:3,9,10)]
mydata <- rescaler(mydata) # standardize variables 

#Determine the number of clusters  ##
Error.df=(nrow(mydata)-1)
MSE=sum(apply(mydata,2,var))
SSE = Error.df*MSE
for (i in 2:15) SSE[i] <- sum(kmeans(mydata,
   centers=i)$withinss)
plot(1:15, SSE, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares") 


## Clustering Methods ##

# 1.  K-Means Cluster Analysis
fit1 <- kmeans(mydata, 4) 
mydata1 <- data.frame(mydata, fit1$cluster) 

##Plotting Cluster Solutions 
# Cluster Plot against 1st 2 principal components
clusplot(mydata1, fit1$cluster, color=TRUE, shade=TRUE,   labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
####Remember to remove append cluster from mydata before plotting
plotcluster(mydata, fit1$cluster,main="Centroid Plot",ylab="Discriminant 2",xlab="Discriminant 1" ) 


# 2.   Ward's Hierarchical Clustering

###There are a wide range of hierarchical clustering
# approaches. Ward's method seems to be create more 
# reasonable clustering

d <- dist(mydata, method = "euclidean") # distance matrix
fit2 <- hclust(d, method="ward")
plot(fit2) # display dendogram
groups <- cutree(fit2, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit2, k=5, border="red") 

## results from Ward's
mydata$group=as.factor(groups)
res=ddply(mydata,.(group),summarize,Count_mean=mean(total_Count),DeathPct_mean=mean(total_DeathPct),OiledPct_mean=mean(total_OiledPct))
qplot(total_OiledPct,total_DeathPct,data=mydata,geom='point',size=total_Count,facets=group~.)
qplot(Gridx,Gridy,data=mydata,geom='point',size=total_Count,facets=group~.)


## 3. Model Based Clustering
modclust<-EMclust(mydata,1:8,c("EII","VII","EEV","EEE","VVV"))
fit3 = plot(modclust,main="BIC vs Number of Clusters for Model Types")
print(fit3)

#EEV has best BIC with 4 components
AggLoc.std=mydata
smry <- summary(modclust,AggLoc.std)
cl <- smry$classification

mcspecific <- EMclust(AggLoc.std, 4,"EEV")
smry <- summary(mcspecific,AggLoc.std)
c3 <- summary(mcspecific,AggLoc.std)$classification
table(c3,groups)


## 4. The Cluster Validation Criteria   #####
cl=groups
AggLoc.std=rescaler(AggLoc)
## a. Ward Check ###
cl.w.attr <- cls.attrib(AggLoc.std,cl)
cl.w.W.matrix <- wcls.matrix(AggLoc.std, cl, cl.w.attr$cluster.center)
cl.w.B.matrix <- bcls.matrix(cl.w.attr$cluster.center,cl.w.attr$cluster.size, cl.w.attr$mean)
cl.w.T.matrix <- cl.w.W.matrix + cl.w.B.matrix
cl.w.crit1 = sum(diag(cl.w.W.matrix))
cl.w.crit2 = sum(diag(cl.w.B.matrix))/sum(diag(cl.w.W.matrix))
cl.w.crit3 = det(cl.w.W.matrix)/det(cl.w.T.matrix)
wardcheck <- c(cl.w.crit1,cl.w.crit2,cl.w.crit3)

## b. Model-based Check  ##
cl.mod.attr <- cls.attrib(AggLoc.std, c3)
cl.mod.W.matrix <- wcls.matrix(AggLoc.std, c3, cl.mod.attr
$cluster.center)
cl.mod.B.matrix <- bcls.matrix(cl.mod.attr$cluster.center,
cl.mod.attr$cluster.size, cl.mod.attr$mean)
cl.mod.T.matrix <- cl.mod.W.matrix + cl.mod.B.matrix
cl.mod.crit1 = sum(diag(cl.mod.W.matrix))
cl.mod.crit2 = sum(diag(cl.mod.B.matrix))/sum(diag(cl.mod.W.matrix))
cl.mod.crit3 = det(cl.mod.W.matrix)/det(cl.mod.T.matrix)
modEEVcheck <- c(cl.mod.crit1,cl.mod.crit2,cl.mod.crit3) 

clusterchecking <- cbind(wardcheck,modEEVcheck)
xtable(clusterchecking)


## 5. Plots of coastline rates ##
#Total death percent
loc=AggLoc[,c(1:3,9)]
map <- ggplot(loc, aes(x=Gridx, y=Gridy),xlab="Longtitude",ylab="Latitude")
map + geom_tile(aes(fill=total_DeathPct))

#Total Oil Percent
loc=AggLoc[,c(1:3,10)]
map <- ggplot(loc, aes(x=Gridx, y=Gridy),xlab="Longtitude",ylab="Latitude")
map + geom_tile(aes(fill=total_OiledPct))

loc$cluster=factor(cl)
map <- ggplot(loc, aes(x=long, y=lat))
map + geom_tile(aes(fill=cluster)) + 
 	geom_path(data=states,aes(x=long,y=lat)) +
	xlim(c(-100,-80)) + ylim(c(24,32)) 
