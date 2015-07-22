#script to look at sapling abundance
#in Denny plots and how this relates to canopy

#author: Phil Martin
#last edited: 29/06/15
library(pscl)
library(ggplot2)
library(plyr)
library(reshape2)
library(tidyr)
library(plyr)


#load data
rm(list=ls(all=TRUE))
Saplings<-read.csv("Data/Denny_plots.csv",stringsAsFactors=F)
Browsing<-read.csv("Data/Browsing_Denny.csv",stringsAsFactors=F)

#tidy saplings data
Saplings<-subset(Saplings,Year==2014)
Saplings2<-ddply(Saplings,.(Block),summarize,Count=sum(FS))


#tidy browsing data
Browsing$Plot2<-sapply(strsplit(Browsing$Plot,"-"),'[',2)
drop<-"Plot"
Browsing<-Browsing[,!(names(Browsing) %in% drop)]
colnames(Browsing)[7]<-"Plot"
Browsing<-Browsing[complete.cases(Browsing),]

#merge browsing data and seedling data
Sapling_browsing<-merge(Browsing,Saplings2,by.x="Plot",by.y="Block")
str(Sapling_browsing)

#now test a zero -inflated model relating canopy openness to sapling density
summary(M1 <- zeroinfl(Count ~ Canopy_open|I(Deer_dung+Horse_dung), data = Sapling_browsing))


M1<-glm(Count~Canopy_open,family="poisson",data=Sapling_browsing)
summary(M1)
1-(2140/2882)




summary(Sapling_browsing)
new.data<-data.frame(Canopy_open=seq(0.6,60,0.01))
new.data$Count<-predict(M1,newdata = new.data,type = "response")

#now plot this model
theme_set(theme_bw(base_size=12))
P1<-ggplot(new.data,aes(x=Canopy_open,y=Count))+geom_line()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")                                                                                                                                            
P2<-P1+geom_point(data=Sapling_browsing,aes(x=Canopy_open,y=Count),shape=1,size=2)
P2+xlab("Canopy openness")+ylab("Number of beech sapling per plot")
ggsave("Figures/Beech_saplings_canopy.png",height=6,width=8,dpi=600,units="in")
