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
MR$Dead4<-ifelse(MR$Dead3==1,0,1)
MR$Recruit<-ifelse(MR$Dead2==2,1,0)

#bootstrap of recruitment for beech 
head(MR)
keeps<-c("Year","Species","Dead","Dead3")
Mort<-MR[keeps]
MR_rows<-unique(Mort[c("Species","Year")])
MR_rows<-MR_rows[with(MR_rows, order(Species,Year)), ]

#run a loop to get bootstrap CIs for each of these estimates
Mort_boot<-NULL
Boot_results<-NULL
Species_un<-unique(MR_rows$Species)
Years_un<-unique(MR_rows$Year)
for (k in 1:length(Species)){
  Mort2<-subset(Mort,Species==Species_un[k])
  for (i in 2:length(Years_un)){
  Mort_boot2<-NULL
  N0<-subset(Mort2,Year==Years_un[i-1])
  N1<-subset(Mort2,Year==Years_un[i])
  for (j in 1:10000){
    N0_alive<-nrow(N0)-sum(N0$Dead,na.rm = T)
    Mort_sample<-sample(N1$Dead3,size=N0_alive,replace = T)
    T<-Years_un[i]-Years_un[i-1]
    Val<-1-(((N0_alive-sum(Mort_sample))/N0_alive)^(1/T))
    Mort_boot2<-rbind(Val,Mort_boot2) 
  }
  Mort_boot<-data.frame(Period=as.character(paste(Years_un[i-1],"-",Years_un[i],sep="")),
                        Species=as.character(Species_un[k]),
                        Mort=quantile(Mort_boot2,probs = c(0.025,0.5,0.975))[2],
                        Mort_UCI=quantile(Mort_boot2,probs = c(0.025,0.5,0.975))[3],
                        Mort_LCI=quantile(Mort_boot2,probs = c(0.025,0.5,0.975))[1])
  Boot_results<-rbind(Mort_boot,Boot_results)
}
}

Boot_results$Period2<-factor(Boot_results$Period,c("1964-1984","1984-1988","1988-1996","1996-2014"))

#plot bootstrapped mortality estimates
theme_set(theme_bw(base_size=12))
Mort<-ggplot(Boot_results,aes(x=Period2,y=Mort*100,ymax=Mort_UCI*100,ymin=Mort_LCI*100,colour=Species))+geom_pointrange(position=position_dodge(width=0.1))
Mort2<-Mort+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Mort2+ylab("Precentage annual mortality")+xlab("Time period")
ggsave("Figures/Annual_mortality.png",height=4,width=6,dpi=1200,units="in")




Mort<-NULL
for (i in 1:10000){
  Mort_sample<-sample(M_1984$Dead3,size = nrow(M_1964),replace = T)
  T<-20
  Val<-1-(((nrow(M_1964)-sum(Mort_sample))/nrow(M_1964))^(1/T))
  Mort<-rbind(Val,Mort)
}
quantile(Mort,probs = c(0.025,0.975))

    
    head(M_sub)

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
