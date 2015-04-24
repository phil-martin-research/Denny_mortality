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


#produce a correlogram of death for each year for each species

#loop to create correlograms for each year
Un_Sp_DBH<-expand.grid(Species=unique(Mort_RC$Species),DBH10=c(">10cm","<10cm"))
for (y in 1:nrow(Un_Sp_DBH)){
Mort_sub<-subset(Mort_RC,Species==Un_Sp_DBH$Species[y]&DBH10==Un_Sp_DBH$DBH10[y])
YU<-unique(Mort_sub$Year)
Corr<-NULL
for (i in 1:length(YU)){
  Beech_sub<-subset(Mort_sub,Year==YU[1]&!is.na(Dead_cum2))
  b.cor<-spline.correlog(Beech_sub$Easting, Beech_sub$Northing, Beech_sub$Dead_cum2,xmax = 100)
  b.cor2<-data.frame(Dist=b.cor$boot$boot.summary$predicted$x[1,],Cor=b.cor$boot$boot.summary$predicted$y[6,],UCI=b.cor$boot$boot.summary$predicted$y[2,],LCI=b.cor$boot$boot.summary$predicted$y[10,],Year=YU[i],Size=Un_Sp_DBH$DBH10[y])
  Corr<-rbind(b.cor2,Corr)
}
theme_set(theme_bw(base_size=12))
Moran_plot1<-ggplot(Corr,aes(x=Dist,y=Cor,ymax=LCI,ymin=UCI))+geom_ribbon(alpha=0.2)+geom_line(size=0.5,colour="black")+facet_wrap(~Year)
Moran_plot2<-Moran_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")+xlim(0,50)
Moran_plot2+geom_hline(y=0,lty=2)+xlab("Distance between trees (m)")+ylab("Moran's I correlation")+scale_size_continuous(range = c(1,3))
ggsave(paste("Figures/Dead_correl_",Un_Species[i],".png",sep=""),height=6,width=8,units="in",dpi=300)
}



