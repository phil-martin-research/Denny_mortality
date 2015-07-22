#script to look at how basal area
#in Denny plots relates to canopy
#to be used in individual based model


#author: Phil Martin
#last edited: 08/07/15
library(pscl)
library(ggplot2)
library(plyr)
library(reshape2)
library(tidyr)
library(plyr)


#load data
rm(list=ls(all=TRUE))
Plots<-read.csv("Data/Denny_plots.csv",stringsAsFactors=F)
Browsing<-read.csv("Data/Browsing_Denny.csv",stringsAsFactors=F)
Plots<-subset(Plots,Year==2014)

#tidy browsing data
Browsing$Plot2<-sapply(strsplit(Browsing$Plot,"-"),'[',2)
drop<-"Plot"
Browsing<-Browsing[,!(names(Browsing) %in% drop)]
colnames(Browsing)[7]<-"Plot"
Browsing<-Browsing[complete.cases(Browsing),]


Blocks<-merge(Browsing,Plots,by.x="Plot",by.y="Block")
head(Blocks)
ggplot(Blocks,aes(x=FM,y=Canopy_open))+geom_point()+geom_smooth(method="lm")
