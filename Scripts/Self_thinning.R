#script to look at evidence for self thinning in Denny


#import data
rm(list=ls(all=TRUE))
Trees<-read.csv("Data/Denny_trees_cleaned.csv")
Trees<-subset(Trees,Block<51)
head(Trees)
#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)
library(scales)
library(reshape)

#write a loop to show stem dynamics for each time period
Trees_ddply<-ddply(Trees,.(Year,Block),summarise,BA=(mean(DBH))^2*0.00007854,SDM=length(Tree_ID))
YUN<-unique(Trees_ddply$Year)[-1]
BUN<-unique(Trees_ddply$Block)
Thin_slope<-NULL
for (i in 2:length(YUN)){
  T1<-subset(Trees_ddply,Year==YUN[i-1])
  T2<-subset(Trees_ddply,Year==YUN[i])
  T_merge<-merge(T1,T2,by="Block")
  T_merge$Slope<-(log(T_merge$SDM.x)-log(T_merge$SDM.y))/(log(T_merge$BA.x)-log(T_merge$BA.y))
  colnames(T_merge)<-c("Block","Year1","BA1","SD1","Year2","BA2","SD2","Slope")
  head(T_merge)
  T_merge$Survey<-paste(YUN[i-1],"-",YUN[i],sep="")
  Thin_slope<-rbind(T_merge,Thin_slope)
}



Thin_slope$Thinning<-ifelse(Thin_slope$Slope>=0,"Not thinning","Thinning")
Thin_slope$Thinning<-ifelse((Thin_slope$BA2-Thin_slope$BA1)<0,"Not thinning",Thin_slope$Thinning)


ddply(Thin_slope,.(Survey,Thinning),summarise,T_NT=length(Thinning))


