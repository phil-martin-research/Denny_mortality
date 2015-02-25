#script to produce summary of mortality and recruitment
#for beech, oak and holly for each survey period

rm(list=ls(all=TRUE))
library(ggplot2)
library(plyr)
library(reshape2)

#load data
MR<-read.csv("Data/Dead_size.csv")
MR<-unique(MR)
MR<-subset(MR,Species=="Q"|Species=="F"|Species=="I")
MR$Dead3<-ifelse(MR$Dead2==1,1,0)
MR$Recruit<-ifelse(MR$Dead2==2,1,0)


#get the mortality rate for different species
Mort_summ<-ddply(MR,.(Year,Species,Dead3),summarize,No=length(Dead3))

#now get recruitment rate
MR2<-subset(MR,Dead3==0)
Rec_summ<-ddply(MR2,.(Year,Species,Recruit),summarize,No=length(Recruit))

#look at balance of recruitment vs mortality
Rec<-subset(Rec_summ,Recruit==1)
Mort<-subset(Mort_summ,Dead3==1)

Bal<-merge(Mort,Rec,by=c("Year","Species"),all=T)

drops<-c("Dead3","Recruit")
Bal<-Bal[,!(names(Bal) %in% drops)]
colnames(Bal)<-c("Year","Species","Mort","Rec")
Bal$Rec<-ifelse(is.na(Bal$Rec),0,Bal$Rec)
Bal$Diff<-Bal$Rec-Bal$Mort
Bal<-subset(Bal,Year>1964)
#now plot this difference
ggplot(Bal,aes(x=Year,y=Diff,colour=Species,group=Species))+geom_point(size=3)+geom_line()

#calculate mortality per species per period
Mort_species2<-NULL
MR_rows<-unique(Mort_summ[c("Species")])
for (i in 1:nrow(MR_rows)){
  Mort_species<-subset(Mort_summ,Species==MR_rows$Species[i])
  Mort_species$MR<-NA
  for (y in 3:nrow(Mort_species)){
    if (Mort_species$Dead3[y]==1){
      T<-(Mort_species$Year[y]-Mort_species$Year[y-3])
      Mort_species$MR[y]<-1-(((Mort_species$No[y-3]-Mort_species$No[y])/Mort_species$No[y-3])^(1/T))
    }
    }
  Mort_species2<-rbind(Mort_species,Mort_species2)
  }


Mort_rate<-subset(Mort_species2,!is.na(MR))

ggplot(Mort_rate,aes(x=Year,y=MR,colour=Species,group=Species))+geom_point(size=3)+geom_line()


#calculate recruitment per species per period
Rec_species2<-NULL
Rec_rows<-unique(Rec_summ[c("Species")])
for (i in 1:nrow(Rec_rows)){
  Rec_species<-subset(Rec_summ,Species==Rec_rows$Species[1])
  Rec_species$RR<-NA
  Years<-unique(Rec_species$Year)
  for (y in 3:nrow(Rec_species)){
    if (Rec_species$Recruit[y]==1){
      T<-(Rec_species$Year[y]-Rec_species$Year[y-3])
      Rec_species$MR[y]<-1-(((Rec_species$No[y-3]-Rec_species$No[y])/Rec_species$No[y-3])^(1/T))
    }
  }
  Rec_species2<-rbind(Rec_species,Rec_species2)
}
