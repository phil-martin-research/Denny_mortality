#script to look at evidence for self thinning in Denny

#import data
rm(list=ls(all=TRUE))
Trees<-read.csv("Data/Denny_trees_cleaned.csv")

head(Trees)
#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)
library(scales)
library(reshape)
library(grid)
std <- function(x) sd(x)/sqrt(length(x))

#write a loop to show stem dynamics for each time period
Trees_ddply<-ddply(Trees,.(Year,Block),summarise,BA=mean((DBH)^2*(pi/4))/10000,SDM=length(Tree_ID),BA_tot=sum((DBH)^2*(pi/4))/10000)
Trees_subset<-subset(Trees_ddply,Year==1964|Year==2014)
unique(Trees_subset$Year)


nrow(Thin_slope)*0.04

YUN<-unique(Trees_subset$Year)
BUN<-unique(Trees_subset$Block)
Thin_slope<-NULL
for (i in 2:length(YUN)){
  T1<-subset(Trees_subset,Year==YUN[i-1])
  T2<-subset(Trees_subset,Year==YUN[i])
  T_merge<-merge(T1,T2,by="Block")
  T_merge$Slope<-((log(T_merge$BA.y)-log(T_merge$BA.x))/(log(T_merge$SDM.y)-log(T_merge$SDM.x)))
  T_merge$BA_change<-(T_merge$BA_tot.y-T_merge$BA_tot.x)/T_merge$BA_tot.x
  colnames(T_merge)<-c("Block","Year1","BA1","SD1","BA_tot1","Year2","BA2","SD2","BA_tot2","Slope","BA_change")
  head(T_merge)
  T_merge$Survey<-paste(YUN[i-1],"-",YUN[i],sep="")
  Thin_slope<-rbind(T_merge,Thin_slope)
}

Thin_slope
head(Trees_subset)
ddply(Trees_subset,.(Year),summarise,BA2=mean(BA),SDM2=mean(SDM))

t.test(x = Thin_slope$Slope,mu = -0.75)
mean(Thin_slope$Slope)
std(Thin_slope$Slope)

line_pred<-data.frame(SDM=33.77,BA=0.0709544,Year=1964)

qplot(Thin_slope$BA_change,Thin_slope$Slope)+geom_hline(y=-0.75,lty=2)



preds<-data.frame(SDM=seq(min(Trees_subset$SDM),max(Trees_subset$SDM),0.1))
preds$BA<-preds$SDM*-1.031957
preds$BA_UCI<-preds$SDM*-0.6778439
preds$BA_LCI<-preds$SDM*-1.3860699
preds$BA_3_2<-preds$SDM*-0.75

#plot slopes- with arrows
lb1 <-expression(paste("mean slope=-0.63" %+-% "0.26"))
theme_set(theme_bw(base_size=12))
ST_plot1<-ggplot(Trees_subset,aes(y=BA,x=SDM,group=Block))+geom_path(colour="black",alpha=0.2,arrow = arrow(length = unit(0.5, "cm")))+scale_x_log10(limits=c(1, 100))+scale_y_log10()
ST_plot2<-ST_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
ST_plot3<-ST_plot2+geom_line(data=preds,aes(x=SDM,y=BA,group=NULL))+ylab(expression(paste("Mean tree basal area (", m^bold("2"),")")))+xlab("Subplot stem density")
ST_plot3+annotate("text", x = 20, y = 0.9, label ="mean slope= -0.63 +/-0.26, not significantly \ndifferent from -0.75 (P=0.65)")
ggsave("Figures/Thinning1.png",width = 8,height=6,units = "in",dpi=300)


