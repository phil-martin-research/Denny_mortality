#script to look at evidence for self thinning in Denny

#import data
rm(list=ls(all=TRUE))
Trees<-read.csv("Data/Denny_trees_cleaned.csv")

#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)
library(scales)
library(reshape)
library(grid)
std <- function(x) sd(x)/sqrt(length(x))

#set year as 1996 where it is 1999
Trees$Year<-ifelse(Trees$Year==1999,1996,Trees$Year)
Trees<-subset(Trees,Year==1964|Year==1996|Year==2014)
Trees<-subset(Trees,Block!=25|Block!=26)

#write a loop to show stem dynamics for each time period
Trees_ddply<-ddply(Trees,.(Year,Block),summarise,BA=sum(((DBH)^2*(pi/4))/10000),SDM=length(Tree_ID))
Trees_ddply<-subset(Trees_ddply,Block!=26)


Blocks<-unique(Trees_ddply$Block)
Increase_BA<-NULL
for (i in 1:length(Blocks)){
  Trees_sub<-subset(Trees_ddply,Block==Blocks[i])
  if (((Trees_sub$BA[2]-Trees_sub$BA[1])>0)&((Trees_sub$BA[3]-Trees_sub$BA[1])>0)){
    Increase_BA<-rbind(Trees_sub,Increase_BA)
  }else{
  }
}

ggplot(Increase_BA,aes(x=SDM,y=BA,shape=as.factor(Year),colour=as.factor(Year),group=Block))+geom_point()+geom_path(colour="black",alpha=0.3,lty=2)+scale_x_log10()+scale_y_log10()


#model the relationship between changes in SDM and those in BA
#first put everything on a log scale
Increase_BA$log_BA<-log(Increase_BA$BA)
Increase_BA$log_SDM<-log(Increase_BA$SDM)


M1<-lmer(log_BA~log_SDM+(log_SDM|Block),data=Increase_BA)
summary(M1)
r.squaredGLMM(M1)
