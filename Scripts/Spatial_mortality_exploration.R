#script to explore spatial characterisics of mortality
#beech, oak and holly

#load packages
library(ggplot2)
library(ncf)

#import data
rm(list=ls(all=TRUE))
Mort<-read.csv("Data/Dead_size.csv")
Mort<-unique(Mort)
head(Mort)
keeps<-c("ID2","Year","Northing","Easting","Dead_cum2","Species")
Mort_F<-subset(Mort,Species=="F")
head(Mort_F)

theme_set(theme_bw(base_size=12))
ggplot(Mort_F,aes(x=Easting,y=Northing,colour=as.factor(Dead_cum2)))+geom_point(shape=1,size=0.5,alpha=0.5)+facet_wrap(~Year)+scale_colour_manual(values = c("green","red"))+ theme(legend.position="none")+coord_fixed()+ 
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
ggsave("Figures/Spat_mortality.png",width = 6,height=6,units = "in",dpi=300)


Beech_mort<-subset(Mort,Species=="F")

#produce a correlogram of death for each year

#loop to create correlograms for each year
Years<-unique(Beech_mort$Year)
Beech_corr<-NULL
for (i in 1:length(Years)){
  Beech_sub<-subset(Beech_mort,Year=Years[i])
  b.cor <- correlog(Beech_sub$Easting, Beech_sub$Northing, Beech_sub$Dead_cum,increment=1, resamp=500)
  b.cor$P2<-ifelse(b.cor$p<0.05,"<0.05",">0.05")
  b.cor2<-data.frame(Dist=b.cor$mean.of.class,Cor=b.cor$correlation,P_val=b.cor$P2,n=b.cor$n,Year=Years[i])
  Beech_corr<-rbind(b.cor2,Beech_corr)
}

head(Beech_corr)
theme_set(theme_bw(base_size=12))
Moran_plot1<-ggplot(Beech_corr,aes(x=Dist,y=Cor,colour=P_val,size=n))+geom_point(shape=1)+geom_line(size=1,colour="black",alpha=0.2)+facet_wrap(~Year)+xlim(0,250)
Moran_plot2<-Moran_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
Moran_plot2
