#import data
rm(list=ls(all=TRUE))
Beech_preds<-read.csv("Data/Beech_mort_preds.csv")
Holly_preds<-read.csv("Data/Oak_mort_preds.csv")
Holly_preds<-read.csv("Data/Holy_mort_preds.csv")
Dead<-read.csv("Data/Dead_size.csv")
Dead_F<-subset(Dead,Species=="F"&!is.na(Dead))
Dead_Q<-subset(Dead,Species=="Q"&!is.na(Dead))
Dead_I<-subset(Dead,Species=="I"&!is.na(Dead))

#open packages needed
library(ggplot2)
library(reshape)
library(plyr)

###################################
#figures of beech models
##################################
head(Beech_preds)

#subset models to give only predictions associated with particular parameters
Beech_growth<-subset(Beech_preds,Model=="Growth rate")
Beech_DBH<-subset(Beech_preds,Model=="DBH")
Dead_BA<-subset(Beech_preds,Model=="Distance to dead tree")

#growth rate
Dead_F$GRbin<-round_any(Dead_F$GR,1)
Growth<-ddply(Dead_F, .(GRbin,Dead), summarize, number=length(Dead))
Growth2<-ddply(Dead_F, .(GRbin), summarize, mean=mean(Dead))

head(Growth)

summary(Beech_growth)
theme_set(theme_bw(base_size=12))
Growth_plot<-ggplot(Beech_growth,aes(x=GR,y=plogis(Preds)))+geom_line()+geom_ribbon(fill="grey",alpha=0.3,aes(ymax=plogis(Preds+(SE*1.96)),ymin=plogis(Preds-(SE*1.96))))
Growth_plot2<-Growth_plot+geom_point(data=Growth,aes(x=GRbin,y=Dead,size=number,ymax=NULL,ymin=NULL),shape=0)+xlim(-10,20)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
Growth_plot2+ylab("Probability of tree death")+xlab("Annual DBH growth rate (mm)")+geom_point(data=Growth2,aes(x=GRbin,y=mean),size=1,colour="black")
ggsave("Figures/Beech_GR_Mort.png",width = 8,height=6,units = "in",dpi=300)

#dbh plot
Dead_F$DBHbin<-round_any(Dead_F$DBH2,10)
DBH<-ddply(Dead_F, .(DBHbin,Dead), summarize, number=length(Dead))
DBH2<-ddply(Dead_F, .(DBHbin), summarize, mean=mean(Dead))


theme_set(theme_bw(base_size=12))
DBH_plot<-ggplot(Beech_DBH,aes(x=DBH2,y=plogis(Preds)))+geom_line()+geom_ribbon(fill="grey",alpha=0.3,aes(ymax=plogis(Preds+(SE*1.96)),ymin=plogis(Preds-(SE*1.96))))
DBH_plot2<-DBH_plot+geom_point(data=DBH,aes(x=DBHbin,y=Dead,size=number,ymax=NULL,ymin=NULL),shape=0)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
DBH_plot2+ylab("Probability of tree death")+xlab("DBH (cm)")+geom_point(data=DBH2,aes(x=DBHbin,y=mean),shape=15,colour="black")
ggsave("Figures/Beech_DBH_Mort.png",width = 8,height=6,units = "in",dpi=300)

#dead_BA
Dead_F$DBA_bin<-round_any(Dead_F$Dead_BA,0.1)
DBA<-ddply(Dead_F, .(DBA_bin,Dead), summarize, number=length(Dead))
DBA2<-ddply(Dead_F, .(DBA_bin), summarize, mean=mean(Dead))

head(DBA)

summary(Beech_growth)
theme_set(theme_bw(base_size=12))
Growth_plot<-ggplot(Dead_BA,aes(x=Dead_BA,y=plogis(Preds)))+geom_line()+geom_ribbon(fill="grey",alpha=0.3,aes(ymax=plogis(Preds+(SE*1.96)),ymin=plogis(Preds-(SE*1.96))))
Growth_plot2<-Growth_plot+geom_point(data=DBA,aes(x=DBA_bin,y=Dead,size=number,ymax=NULL,ymin=NULL),shape=0)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
Growth_plot2+ylab("Probability of tree death")+xlab("BA of dead trees within 10m radius")+geom_point(data=DBA2,aes(x=DBA_bin,y=mean),size=1,colour="black")
ggsave("Figures/Beech_Dist.png",width = 8,height=6,units = "in",dpi=300)

#dbh plot
Dead_F$DBHbin<-round_any(Dead_F$DBH2,10)
DBH<-ddply(Dead_F, .(DBHbin,Dead), summarize, number=length(Dead))
DBH2<-ddply(Dead_F, .(DBHbin), summarize, mean=mean(Dead))


theme_set(theme_bw(base_size=12))
DBH_plot<-ggplot(Beech_DBH,aes(x=DBH2,y=plogis(Preds)))+geom_line()+geom_ribbon(fill="grey",alpha=0.3,aes(ymax=plogis(Preds+(SE*1.96)),ymin=plogis(Preds-(SE*1.96))))
DBH_plot2<-DBH_plot+geom_point(data=DBH,aes(x=DBHbin,y=Dead,size=number,ymax=NULL,ymin=NULL),shape=0)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
DBH_plot2+ylab("Probability of tree death")+xlab("DBH (cm)")+geom_point(data=DBH2,aes(x=DBHbin,y=mean),shape=15,colour="black")
ggsave("Figures/Beech_DBH_Mort.png",width = 8,height=6,units = "in",dpi=300)

#####################################
#figures of oak models###############
#####################################

head(Oak_preds)

#subset models to give only predictions associated with particular parameters
Oak_growth<-subset(Oak_preds,Model=="Growth rate")
Oak_DBH<-subset(Oak_preds,Model=="DBH")
Oak_Dead_BA<-subset(Oak_preds,Model=="Distance to dead tree")

#growth rate
Dead_Q$GRbin<-round_any(Dead_Q$GR,1)
Growth<-ddply(Dead_Q, .(GRbin,Dead), summarize, number=length(Dead))
Growth2<-ddply(Dead_Q, .(GRbin), summarize, mean=mean(Dead))

head(Growth)

summary(Oak_growth)
theme_set(theme_bw(base_size=12))
QGP<-ggplot(Oak_growth,aes(x=GR,y=plogis(Preds)))+geom_line()+geom_ribbon(fill="grey",alpha=0.3,aes(ymax=plogis(Preds+(SE*1.96)),ymin=plogis(Preds-(SE*1.96))))
QGP2<-QGP+geom_point(data=Growth,aes(x=GRbin,y=Dead,size=number,ymax=NULL,ymin=NULL),shape=0)+xlim(-10,16)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
QGP2+ylab("Probability of tree death")+xlab("Annual DBH growth rate (mm)")+geom_point(data=Growth2,aes(x=GRbin,y=mean),size=1,colour="black")
ggsave("Figures/Oak_GR_Mort.png",width = 8,height=6,units = "in",dpi=300)

#dbh plot
Dead_Q$DBHbin<-round_any(Dead_Q$DBH2,10)
DBH<-ddply(Dead_Q, .(DBHbin,Dead), summarize, number=length(Dead))
DBH2<-ddply(Dead_Q, .(DBHbin), summarize, mean=mean(Dead))


theme_set(theme_bw(base_size=12))
DBH_plot<-ggplot(Oak_DBH,aes(x=DBH2,y=plogis(Preds)))+geom_line()+geom_ribbon(fill="grey",alpha=0.3,aes(ymax=plogis(Preds+(SE*1.96)),ymin=plogis(Preds-(SE*1.96))))
DBH_plot2<-DBH_plot+geom_point(data=DBH,aes(x=DBHbin,y=Dead,size=number,ymax=NULL,ymin=NULL),shape=0)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
DBH_plot2+ylab("Probability of tree death")+xlab("DBH (cm)")+geom_point(data=DBH2,aes(x=DBHbin,y=mean),shape=15,colour="black")
ggsave("Figures/Oak_DBH_Mort.png",width = 8,height=6,units = "in",dpi=300)


#dead_BA
Dead_Q$DBA_bin<-round_any(Dead_Q$Dead_BA,0.1)
DBA<-ddply(Dead_Q, .(DBA_bin,Dead), summarize, number=length(Dead))
DBA2<-ddply(Dead_Q, .(DBA_bin), summarize, mean=mean(Dead))

head(DBA)

summary(Oak_growth)
theme_set(theme_bw(base_size=12))
BDAQP<-ggplot(Oak_Dead_BA,aes(x=Dead_BA,y=plogis(Preds)))+geom_line()+geom_ribbon(fill="grey",alpha=0.3,aes(ymax=plogis(Preds+(SE*1.96)),ymin=plogis(Preds-(SE*1.96))))
BDAQP2<-BDAQP+geom_point(data=DBA,aes(x=DBA_bin,y=Dead,size=number,ymax=NULL,ymin=NULL),shape=0)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
BDAQP2+ylab("Probability of tree death")+xlab("BA of dead trees within 10m radius")+geom_point(data=DBA2,aes(x=DBA_bin,y=mean),size=1,colour="black")
ggsave("Figures/Oak_Dist.png",width = 8,height=6,units = "in",dpi=300)

######################################
#figures for holly models#############
######################################

head(Holly_preds)

#subset models to give only predictions associated with particular parameters
Holly_growth<-subset(Holly_preds,Model=="Growth rate")
Holly_DBH<-subset(Holly_preds,Model=="DBH")
Holly_Dead_BA<-subset(Holly_preds,Model=="Distance to dead tree")

head(Holly_Dead_BA)

#growth rate
Dead_I$GRbin<-round_any(Dead_I$relGR,1)
Growth<-ddply(Dead_I, .(GRbin,Dead), summarize, number=length(Dead))
Growth2<-ddply(Dead_I, .(GRbin), summarize, mean=mean(Dead))

head(Growth)

summary(Holly_growth)
theme_set(theme_bw(base_size=12))
IGP<-ggplot(Holly_growth,aes(x=relGR,y=plogis(Preds)))+geom_line()+geom_ribbon(fill="grey",alpha=0.3,aes(ymax=plogis(Preds+(SE*1.96)),ymin=plogis(Preds-(SE*1.96))))
IGP2<-IGP+geom_point(data=Growth,aes(x=GRbin,y=Dead,size=number,ymax=NULL,ymin=NULL),shape=0)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
IGP2+ylab("Probability of tree death")+xlab("Relative growth rate")+geom_point(data=Growth2,aes(x=GRbin,y=mean),size=1,colour="black")
ggsave("Figures/Holly_GR_Mort.png",width = 8,height=6,units = "in",dpi=300)

#dbh plot
Dead_I$DBHbin<-round_any(Dead_I$DBH2,5)
DBH<-ddply(Dead_I, .(DBHbin,Dead), summarize, number=length(Dead))
DBH2<-ddply(Dead_I, .(DBHbin), summarize, mean=mean(Dead))


theme_set(theme_bw(base_size=12))
IDP<-ggplot(Holly_DBH,aes(x=DBH2,y=plogis(Preds)))+geom_line()+geom_ribbon(fill="grey",alpha=0.3,aes(ymax=plogis(Preds+(SE*1.96)),ymin=plogis(Preds-(SE*1.96))))
IDP2<-IDP+geom_point(data=DBH,aes(x=DBHbin,y=Dead,size=number,ymax=NULL,ymin=NULL),shape=0)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
IDP2+ylab("Probability of tree death")+xlab("DBH (cm)")+geom_point(data=DBH2,aes(x=DBHbin,y=mean),shape=15,colour="black")
ggsave("Figures/Holly_DBH_Mort.png",width = 8,height=6,units = "in",dpi=300)


#dead_BA
Dead_I$Dist_bin<-round_any(Dead_I$Dead_dist,10)
DBA<-ddply(Dead_I, .(Dist_bin,Dead), summarize, number=length(Dead))
DBA2<-ddply(Dead_I, .(Dist_bin), summarize, mean=mean(Dead))

head(Holly_Dead_BA)

summary(Oak_growth)
theme_set(theme_bw(base_size=12))
DistIP<-ggplot(Holly_Dead_BA,aes(x=Dead_dist,y=plogis(Preds)))+geom_line()+geom_ribbon(fill="grey",alpha=0.3,aes(ymax=plogis(Preds+(SE*1.96)),ymin=plogis(Preds-(SE*1.96))))
DistIP2<-DistIP+geom_point(data=DBA,aes(x=Dist_bin,y=Dead,size=number,ymax=NULL,ymin=NULL),shape=0)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
DistIP2+ylab("Probability of tree death")+xlab("Distance to nearest dead holly tree")+geom_point(data=DBA2,aes(x=Dist_bin,y=mean),size=1,colour="black")
ggsave("Figures/Holly_Dist.png",width = 8,height=6,units = "in",dpi=300)

