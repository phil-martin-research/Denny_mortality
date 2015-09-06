rm(list=ls(all=TRUE))

library(ggplot2)
library(plyr)

Coll<-read.csv("Netlogo/Model_outputs/Mort_exp.csv")
Coll$spat_feed<-as.factor(ifelse(Coll$spat_feed==FALSE,"No spatial feedback","Spatial feedback"))
Coll$diff_mort<-as.factor(ifelse(Coll$diff_mort==FALSE,"No differential juvenile mortality","Differential juvenile mortality"))

Coll_summ<-ddply(Coll,.(time,juv.mort,diff_mort,spat_feed),summarise,m_BA=mean(BA))

theme_set(theme_bw(base_size=12))
P1<-ggplot(Coll,aes(x=time,y=BA,group=run_no,colour=as.factor(juv.mort)))+geom_line(alpha=0.6,size=0.1)+facet_grid(spat_feed~diff_mort)+geom_smooth(aes(group=as.factor(juv.mort),colour=as.factor(juv.mort)),size=2)
P2<-P1+scale_colour_brewer("Annual probability of \n juvenile death",type="seq",palette="Reds")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
P2+ylab("Basal area")+xlab("Time (years")

ggsave("Figures/IBM_results.png",height=8,width=10,dpi=400,units="in")

ggplot(Coll_summ,aes(x=juv.mort,y=time,fill=m_BA))+geom_raster()+facet_grid(diff_mort~spat_feed)+scale_fill_gradient(low="white",high="black")
