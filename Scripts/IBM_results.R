rm(list=ls(all=TRUE))

library(ggplot2)
library(plyr)
library(grid)


Coll<-read.csv("Netlogo/Model_outputs/Denny_mort_4.3.40_Exp1.csv")
BA<-read.csv("Data/Denny_plots.csv")
BA$Year<-ifelse(BA$Year==1999,1996,BA$Year)


head(BA)
BA_summ<-(ddply(BA,.(Year),summarise,m_BA=mean(BAM),SD_BA=sd(BAM)))
BA_summ2<-BA_summ[rep(seq_len(nrow(BA_summ)), each=11),]
BA_summ2$juv.mort<-rep(c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),length.out=55)

Coll$spat_feed<-as.factor(ifelse(Coll$spat_feed==FALSE,"No spatial feedback","Spatial feedback"))
Coll$gap_death<-as.factor(ifelse(Coll$gap_death==FALSE,"No","Yes"))

Coll_summ<-ddply(Coll,.(time,juv.mort,spat_feed,gap_death),summarise,m_BA=median(BA),SD_BA=sd(BA),m_Trees=median(no_trees))
Coll_summ<-subset(Coll_summ,time>1)
Coll_summ<-subset(Coll_summ,juv.mort<=0.5)
BA_summ2<-subset(BA_summ2,juv.mort<=0.5)

theme_set(theme_bw(base_size=16))
P1<-ggplot(Coll_summ,aes(x=time+1962,y=m_BA,colour=gap_death))+geom_line(size=1,aes(lty=spat_feed))+facet_wrap(~juv.mort,ncol=3)
P2<-P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3<-P2+ylab("Basal area")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("Year")
P3+geom_point(data=BA_summ2,aes(x=Year,y=m_BA),size=3,shape=1,colour="black")+scale_linetype("Feedback type")+scale_color_discrete("Juvenile gap death")
ggsave("Figures/IBM_results.png",height=6,width=10,dpi=400,units="in")


head(Drought)
Drought<-read.csv("Netlogo/Model_outputs/Denny_mort_4.3.40_D1.csv")
Drought$spat_feed<-as.factor(ifelse(Drought$spat_feed==FALSE,"No spatial feedback","Spatial feedback"))
Drought$gap_death<-as.factor(ifelse(Drought$gap_death==FALSE,"No juvenile gap death","Juvenile gap death"))
Drought$drought.rate<-round(Drought$drought.rate)
Drought_summ<-ddply(Drought,.(time,juv.mort,spat_feed,gap_death,drought.rate),summarise,m_BA=median(BA),SD_BA=sd(BA),m_Trees=median(no_trees),m_canopy=median(canopy))
Drought_summ<-subset(Drought_summ,juv.mort==0.4)

Canopy_plot1<-ggplot(Drought_summ,aes(x=time+1962,y=m_canopy,colour=as.factor(drought.rate)))+geom_line(size=1)+facet_grid(gap_death~spat_feed)
Canopy_plot2<-Canopy_plot1+geom_hline(yintercept=60,lty=2)+scale_colour_brewer("Annual percentage chance of drought",palette = "Set1")+ylab("Canopy openness (%)")+xlab("Year")
Canopy_plot2
ggsave("Figures/IBM_canopy.png",height=6,width=10,dpi=400,units="in")
