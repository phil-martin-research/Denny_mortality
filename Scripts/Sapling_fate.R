#a script to track the fate of beech saplings at each survey

#author: Phil Martin
#last edited: 27/03/15

#load packages
library(ggplot2)
library(dplyr)

#load data
rm(list=ls(all=TRUE))
Trees<-read.csv("Data/Denny_trees_cleaned.csv")

#I need to:
#(i) - work out which trees are saplings (<10cm DBH) at each time point
#(ii) - work out what happened to them by the next survey e.g. Increased >10cm DBH, were still <10cm DBH or died

Trees$Year<-ifelse(Trees$Year==1999,1996,Trees$Year)


Fate2<-NULL
YU<-unique(Trees$Year)[-1]
for (i in 2:length(YU)){
  T1<-subset(Trees,Year==YU[i-1]&DBH<10&Status==1&Species=="F")
  T2<-subset(Trees,Year==YU[i]&Species=="F")
  Merged<-merge(T1,T2,by="Tree_ID",all.x=T)  
  Merged$Status.y<-ifelse(is.na(Merged$Status.y),0,Merged$Status.y)
  Merged$Above10<-ifelse(Merged$DBH.y>10,1,0)
  Merged$Above10<-ifelse(is.na(Merged$Above10),0,Merged$Above10)
  TotT1<-nrow(T1)
  Dead<-sum(Merged$Status.x,na.rm = T)-sum(Merged$Status.y,na.rm = T)
  Above10<-sum(Merged$Above10,na.rm = T)
  Below10<-(TotT1-(Dead+Above10))
  TotT2<-TotT1-Dead
  Mort<-1-(TotT2/TotT1)^(1/(YU[i]-YU[i-1]))
  Fate<-data.frame(Year_1=YU[i-1],Year_2=YU[i],No_T1=TotT1,Died=Dead,Prop_died=round(Dead/TotT1,2),
                   Increased=Above10,Prop_inc=round(Above10/TotT1,2),No_increase=Below10, Prop_no_inc=round(Below10/TotT1,2),
                   Mort=Mort)
  Fate2<-rbind(Fate,Fate2)
}

write.csv(Fate2,"Tables/Sapling_fate_Beech.csv",row.names=F)
