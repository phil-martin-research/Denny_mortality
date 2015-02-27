#script to produce summary of mortality and recruitment
#for beech, oak and holly for each survey period

rm(list=ls(all=TRUE))
library(ggplot2)
library(plyr)
library(reshape2)

#load data
MR<-read.csv("Data/Dead_size.csv")
head(MR)
MR<-subset(MR,Species=="Q"|Species=="F"|Species=="I")
MR<-subset(MR,!is.na(Dead2))
MR$Dead3<-ifelse(MR$Dead2==1,1,0)#1=dead 0=alive
MR$Recruit<-ifelse(MR$Dead2==2,1,0)


#########################################################
#calculate mortality rate and bootstrapped CIs for this##
#based on Margrove et al 2015 - Impacts of an Extreme 
#Precipitation Event on Dipterocarp Mortality 
#and Habitat Filtering in a Bornean Tropical Rain Forest

#bootstrap of recruitment for beech 
keeps<-c("Year","Species","Dead","Dead3")
Mort<-MR[keeps]
head(Mort)
MR_rows<-unique(Mort[c("Species","Year")])
MR_rows<-MR_rows[with(MR_rows, order(Species,Year)), ]

#run a loop to get bootstrap CIs for each of these estimates
Mort_boot<-NULL
Boot_results<-NULL
Species_un<-unique(MR_rows$Species)
Years_un<-sort(unique(MR_rows$Year))
for (k in 1:length(Species_un)){
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

Mort_results<-Boot_results

#plot bootstrapped mortality estimates
theme_set(theme_bw(base_size=12))
Mort_plot<-ggplot(Mort_results,aes(x=Period2,y=Mort*100,ymax=Mort_UCI*100,ymin=Mort_LCI*100,colour=Species))+geom_pointrange(position=position_dodge(width=0.1))
Mort_plot2<-Mort_plot+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Mort_plot2+ylab("Precentage annual mortality")+xlab("Time period")
ggsave("Figures/Annual_mortality.png",height=4,width=6,dpi=1200,units="in")




####
#bootstrap of recruitment
keeps<-c("Year","Species","Dead","Recruit")
Rec<-MR[keeps]
head(Rec)
Rec_rows<-unique(Rec[c("Species","Year")])
Rec_rows<-Rec_rows[with(Rec_rows, order(Species,Year)), ]



Rec_boot<-NULL
Boot_results<-NULL
Species_un<-as.character(unique(Rec_rows$Species))
Years_un<-sort(unique(Rec_rows$Year))
for (k in 1:length(Species_un)){
  Rec2<-subset(Rec,Species==Species_un[k])
  for (i in 2:length(Years_un)){
    Rec_boot2<-NULL
    N0<-subset(Rec2,Year==Years_un[i-1])#number alive at time step 0
    N1<-subset(Rec2,Year==Years_un[i]) #number of recruits at time step 1
    for (j in 1:10000){
      N0_alive<-nrow(N0)-sum(N0$Dead,na.rm = T)
      Rec_sample<-sample(N1$Recruit,size=N0_alive,replace = T)
      T<-Years_un[i]-Years_un[i-1]
      Val<-1-(((N0_alive-sum(Rec_sample))/N0_alive)^(1/T))
      Rec_boot2<-rbind(Val,Rec_boot2) 
    }
    Rec_boot<-data.frame(Period=as.character(paste(Years_un[i-1],"-",Years_un[i],sep="")),
                          Species=as.character(Species_un[k]),
                          Rec=quantile(Rec_boot2,probs = c(0.025,0.5,0.975))[2],
                          Rec_UCI=quantile(Rec_boot2,probs = c(0.025,0.5,0.975))[3],
                          Rec_LCI=quantile(Rec_boot2,probs = c(0.025,0.5,0.975))[1])
    Boot_results<-rbind(Rec_boot,Boot_results)
  }
}

head(Boot_results)

#plot bootstrapped recruitment estimates
Boot_results$Period2<-factor(Boot_results$Period,c("1964-1984","1984-1988","1988-1996","1996-2014"))
Rec_results<-Boot_results
Mort_results

theme_set(theme_bw(base_size=12))
Rec_plot<-ggplot(Rec_results,aes(x=Period2,y=Rec*100,ymax=Rec_UCI*100,ymin=Rec_LCI*100,colour=Species))+geom_pointrange(position=position_dodge(width=0.1))
Rec_plot2<-Rec_plot+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Rec_plot2+ylab("Percentage annual recruitment")+xlab("Time period")
ggsave("Figures/Annual_recruitment.png",height=4,width=6,dpi=1200,units="in")

Rec_results$Change<-Rec_results$Rec-Mort_results$Mort
Rec_results$Change_UCI<-Rec_results$Rec_UCI-Mort_results$Mort_LCI
Rec_results$Change_LCI<-Rec_results$Rec_LCI-Mort_results$Mort_UCI

#now plot percentage change in populations
theme_set(theme_bw(base_size=12))
Change_plot<-ggplot(Rec_results,aes(x=Period2,y=Change*100,ymax=Change_UCI*100,ymin=Change_LCI*100,colour=Species))+geom_pointrange(position=position_dodge(width=0.1))
Change_plot2<-Change_plot+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Change_plot2+ylab("Percentage annual change in population")+xlab("Time period")
ggsave("Figures/Annual_change.png",height=4,width=6,dpi=1200,units="in")


