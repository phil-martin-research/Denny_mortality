#script to look at seedling presence/absence and density
#in Denny plots and how this relates to canopy/basal area/mature density

#author: Phil Martin
#last edited: 22/04/15

#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(plotrix)

#load data
rm(list=ls(all=TRUE))
Seedlings<-read.csv("Data/Seedlings_denny.csv",stringsAsFactors=F)
Plots<-read.csv("Data/Denny_plots_trees_M.csv")
Browsing<-read.csv("Data/Browsing_Denny.csv",,stringsAsFactors=F)

#tidy seedlings data
Seedlings$Plot<-sapply(strsplit(Seedlings$Plot2,"-"),'[',2)
drop<-"Plot2"
Seedlings<-Seedlings[,!(names(Seedlings) %in% drop)]
head(Seedlings)
Seedlings2<-subset(Seedlings,Species=="Fagus sylvatica"|Species=="Quercus spp"|Species=="Ilex aquifolium")
Seed_melt<-melt(Seedlings2,id.vars = c("Plot","Count"))
Seed_melt2<-ddply(Seed_melt,.(Plot,value),summarize,Count=sum(Count)*100)
Seed_melt3<-spread(Seed_melt2, value, Count)
Seed_melt3[is.na(Seed_melt3)] <- 0
colnames(Seed_melt3)<-c("Plot","Beech","Holly","Oak")

#tidy browsing data
Browsing$Plot2<-sapply(strsplit(Browsing$Plot,"-"),'[',2)
drop<-"Plot"
Browsing<-Browsing[,!(names(Browsing) %in% drop)]
colnames(Browsing)[7]<-"Plot"
Browsing<-Browsing[complete.cases(Browsing),]

#merge browsing data and seedling data
Seed_browsing<-merge(Browsing,Seed_melt3,by="Plot")
str(Seed_browsing)


#before analysis produce mean +/- SE for each species
#and put in a table for later
Seedling_density<-data.frame(Species=c("Beech","Oak","Holly"),
           Mean_seedling_density=c(mean(Seed_browsing$Beech),mean(Seed_browsing$Oak),mean(Seed_browsing$Holly)),
           SE=c(std.error(Seed_browsing$Beech),std.error(Seed_browsing$Oak),std.error(Seed_browsing$Holly)))
write.csv(Seedling_density,"Tables/Seedling_density.csv",row.names=F)

#explore the relationships between seedling density and
#browsing pressure/canopy openness
ggpairs(Seed_browsing[2:10])

#explore in more detail
#canopy relationship with beech seedlings
ggplot(Seed_browsing,aes(x=Canopy_open,y=Beech))+geom_point()+geom_smooth(method="glm",family="poisson")+xlim(0,40)
#canopy openness and holly
ggplot(Seed_browsing,aes(x=Canopy_open,y=Holly))+geom_point()+geom_smooth(method="glm",family="poisson")+xlim(0,40)
#canopy openness and Oak
ggplot(Seed_browsing,aes(x=Canopy_open,y=Oak))+geom_point()+geom_smooth(method="glm",family="poisson")+xlim(0,40)


#Horse and Deer browsing pressure with beech seedlings
ggplot(Seed_browsing,aes(x=Deer_dung,y=Beech))+geom_point()+geom_smooth(method="glm",family="poisson")
ggplot(Seed_browsing,aes(x=Horse_dung,y=Beech))+geom_point()+geom_smooth(method="glm",family="poisson")
ggplot(Seed_browsing,aes(x=Deer_dung+Horse_dung,y=Beech))+geom_point()+geom_smooth(method="glm",family="poisson")

#browsing pressure and canopy openness
str(Seed_browsing)
ggplot(Seed_browsing,aes(x=Canopy_open,y=Deer_dung))+geom_point()+geom_smooth(method="lm")
ggplot(Seed_browsing,aes(x=Canopy_open,y=Horse_dung))+geom_point()+geom_smooth(method="lm")
ggplot(Seed_browsing,aes(x=Canopy_open,y=Deer_dung+Horse_dung))+geom_point()+geom_smooth(method="lm")
ggplot(Seed_browsing,aes(x=Canopy_open,y=Bramble_browsed))+geom_point()+geom_smooth(method="lm")
ggplot(Seed_browsing,aes(x=Canopy_open,y=Holly_browsed))+geom_point()+geom_smooth(method="lm")
ggplot(Seed_browsing,aes(x=Canopy_open,y=Sward_h))+geom_point()+geom_smooth(method="lm")

#there is little evidence that there is a relationship between browsing pressure in denny and the density of beech
#seedlings - this could possibly be becuase browsing pressure is high everywhere (?!)
#in addition there is no relationship between canopy openness and browsing pressure
head(Seed_browsing)

Seed_browsing2<-cbind(Seed_browsing[,1],apply(X=Seed_browsing[,2:(ncol(Seed_browsing)-3)],MARGIN=2,FUN=function(x) {(x-mean(x))/sd(x)}),Seed_browsing[,8:10])


#now test to see if there is a relationship between canopy openness and beech seedling density
M0<-glm(I(Beech/100)~1,data=Seed_browsing2,family="poisson")
M1<-glm(I(Beech/100)~Canopy_open,data=Seed_browsing2,family="poisson")
M2<-glm(I(Beech/100)~Canopy_open+I(Canopy_open^2),data=Seed_browsing2,family="poisson")
M3<-glm(I(Beech/100)~Canopy_open+I(Canopy_open^2)+Deer_dung,data=Seed_browsing2,family="poisson")
M4<-glm(I(Beech/100)~Canopy_open+I(Canopy_open^2)+Horse_dung,data=Seed_browsing2,family="poisson")
M5<-glm(I(Beech/100)~Canopy_open+I(Canopy_open^2)+Horse_dung+Deer_dung,data=Seed_browsing2,family="poisson")
AICc(M0,M1,M2,M3,M4,M5)
par(mfrow=c(2,2))
plot(M1)
summary(M0)
summary(M1)
1-(97.742/129.762)

par(mfrow=c(1,1))
plot(Seed_browsing$Canopy_open,Seed_browsing$Beech/100)
points(Seed_browsing$Canopy_open,exp(predict(M1)),col="red")
#there is a positive relationship between beech seedling density and canopy openness
#but there is no evidence of other relationships


#now test to see if there is a relatioship between canopy openness and holly seedling density
M0<-glm(I(Holly/100)~1,data=Seed_browsing,family="poisson")
M1<-glm(I(Holly/100)~Canopy_open,data=Seed_browsing,family="poisson")
M2<-glm(I(Holly/100)~Canopy_open+I(Canopy_open^2),data=Seed_browsing,family="poisson")
M3<-glm(I(Holly/100)~Canopy_open+I(Canopy_open^2)+Deer_dung,data=Seed_browsing,family="poisson")
M4<-glm(I(Holly/100)~Canopy_open+I(Canopy_open^2)+Horse_dung,data=Seed_browsing,family="poisson")
M5<-glm(I(Holly/100)~Canopy_open+I(Canopy_open^2)+I(Horse_dung+Deer_dung),data=Seed_browsing,family="poisson")

AICc(M0,M1,M2,M3,M4,M5)
par(mfrow=c(2,2))
plot(M5)
summary(M0)
summary(M4)
1-(81573/111495)

par(mfrow=c(1,1))
plot(Seed_browsing$Canopy_open,Seed_browsing$Holly/100)
points(Seed_browsing$Canopy_open,exp(predict(M4)),col="red")
#holly seedling density peaks at a canopy openness of about 30% before dropping again
