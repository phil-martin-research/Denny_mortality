#script to explore spatial characterisics of mortality
#beech, oak and holly

#load packages
library(ggplot2)
library(ncf)
library(plyr)
library(reshape)


#import data
rm(list=ls(all=TRUE))
Mort<-read.csv("Data/Dead_spat.csv")
Mort<-unique(Mort)
head(Mort)
keeps<-c("ID2","Year","Northing","Easting","Dead_cum2","Species","DBH2","DBH","Dead")
Mort<-Mort[keeps]
Mort<-ddply(Mort, .(ID2,Year), head, n = 1) 

head(Mort,n = 10)

#set DBH2 to be (i) that from previous DBH reading, (ii) last DBH reading when tree was alive or (iii) current DBH for live trees
Mort_RC<-NULL
Tree_ID<-unique(Mort$ID2)
for (i in 1:length(Tree_ID)){
  Mort_sub<-subset(Mort,ID2==Tree_ID[i])
  for (j in 1:nrow(Mort_sub)){
    if ((Mort_sub$Year[j]==1964)==T){
      Mort_sub$DBH2[j]<-Mort_sub$DBH[j]
      Mort_sub$Dead_cum2[j]<-Mort_sub$Dead[j]
    }else if (Mort_sub$Dead_cum[j]==1&(!is.na(Mort_sub$DBH[j-1]))==T){
      Mort_sub$DBH2[j]<-Mort_sub$DBH[j-1]
    }else if (Mort_sub$Dead_cum[j]==1&(is.na(Mort_sub$DBH[j-1]))==T){
      Mort_sub$DBH2[j]<-Mort_sub$DBH2[j-1]
    } else if (!is.na(Mort_sub$DBH[j])==T){
      Mort_sub$DBH2[j]<-Mort_sub$DBH[j]
    }else{
      Mort_sub$DBH2[j]<-NA
    }
  }
  Mort_RC<-rbind(Mort_RC,Mort_sub)
}

head(Mort_RC,n=10)
Mort_RC$DBH10<-ifelse(Mort_RC$DBH2>10,">10cm",NA)
Mort_RC$DBH10<-ifelse(Mort_RC$DBH2<=10,"<10cm",Mort_RC$DBH10)


#now use just beech
Mort_F<-subset(Mort_RC,Species=="F"&!is.na(DBH2))

uyear<-length(unique(Mort_RC$Year))

theme_set(theme_bw(base_size=12))
ggplot(Mort_F,aes(x=Easting,y=Northing,colour=as.factor(Dead_cum2)))+geom_point(shape=1,size=0.5,alpha=0.5)+facet_wrap(DBH10~Year,ncol=2*uyear)+scale_colour_manual(values = c("green","red"))+ theme(legend.position="none")+coord_fixed()+ 
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
ggsave("Figures/Spat_mortality.png",width = 12,height=6,units = "in",dpi=300)


#produce a correlogram of death for each year

#loop to create correlograms for each year
YU<-unique(Mort_F$Year)
Beech_corr<-NULL
for (i in 1:length(YU)){
  Beech_sub<-subset(Mort_F,Year==YU[i]&!is.na(Dead_cum2))
  b.cor<-spline.correlog(Beech_sub$Easting, Beech_sub$Northing, Beech_sub$Dead_cum2,xmax = 50)
  b.cor2<-data.frame(Dist=b.cor$boot$boot.summary$predicted$x[1,],Cor=b.cor$boot$boot.summary$predicted$y[6,],UCI=b.cor$boot$boot.summary$predicted$y[2,],LCI=b.cor$boot$boot.summary$predicted$y[10,],Year=Y_DBH$Year[i],Size=Y_DBH$DBH10[i])
  Beech_corr<-rbind(b.cor2,Beech_corr)
}


head(Beech_corr)
theme_set(theme_bw(base_size=12))
Moran_plot1<-ggplot(Beech_corr,aes(x=Dist,y=Cor,ymax=LCI,ymin=UCI))+geom_ribbon(alpha=0.2)+geom_line(size=0.5,colour="black")+facet_wrap(~Year)
Moran_plot2<-Moran_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")+xlim(0,50)
Moran_plot2+geom_hline(y=0,lty=2)+xlab("Distance between trees (m)")+ylab("Moran's I correlation")+scale_size_continuous(range = c(1,3))
ggsave("Figures/Dead_correl.png",height=6,width=8,units="in",dpi=300)

