rm(list=ls(all=TRUE))

library(ggplot2)
library(plyr)
library(grid)

Coll<-read.csv("Netlogo/Model_outputs/Denny_mort_3.9-exp2_1.csv")
BA<-read.csv("Data/Plot_BA_Denny.csv")
BA$Year<-ifelse(BA$Year==1999,1996,BA$Year)

BA_summ<-(ddply(BA,.(Year),summarise,m_BA=mean(BAM),SD_BA=sd(BAM)))
BA_summ2<-BA_summ[rep(seq_len(nrow(BA_summ)), each=4),]
BA_summ2$spat_feed<-rep(c("No spatial feedback","Spatial feedback"),times=4,length.out = 20)
BA_summ2$diff_mort<-rep(c("No differential juvenile mortality","Differential juvenile mortality"),each = 2,length.out=20)


ggplot(BA_summ2,aes(x=Year,y=m_BA,ymax=m_BA+SD_BA,ymin=m_BA-SD_BA))+geom_pointrange()+facet_grid(spat_feed~diff_mort)

Coll$spat_feed<-as.factor(ifelse(Coll$spat_feed==FALSE,"No spatial feedback","Spatial feedback"))
Coll$diff_mort<-as.factor(ifelse(Coll$diff_mort==FALSE,"No differential juvenile mortality","Differential juvenile mortality"))

Coll_summ<-ddply(Coll,.(time,juv.mort,diff_mort,spat_feed),summarise,m_BA=mean(BA),SD_BA=sd(BA))

summary(Coll)

theme_set(theme_bw(base_size=12))

P1<-ggplot(Coll_summ,aes(x=time+1964,y=m_BA))+geom_line(size=1,aes(colour=as.factor(juv.mort)))+facet_grid(spat_feed~diff_mort)
P2<-P1+scale_colour_brewer("Annual probability of \n juvenile death",type="seq",palette="Reds")+scale_fill_brewer("Annual probability of \n juvenile death",type="seq",palette="Reds")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P3<-P2+ylab("Basal area")+xlab("Year")+ theme(panel.margin = unit(2, "lines"))
P3+geom_point(data=BA_summ2,aes(x=Year,y=m_BA,ymax=m_BA+SD_BA,ymin=m_BA-SD_BA),colour="black",shape=1,size=3)
P3
ggsave("Figures/IBM_results.png",height=8,width=10,dpi=400,units="in")

