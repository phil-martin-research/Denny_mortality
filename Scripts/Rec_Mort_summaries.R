#script to produce summary of mortality and recruitment
#for beech, oak and holly for each survey period

rm(list=ls(all=TRUE))
library(ggplot2)
library(plyr)
library(reshape2)

#load data
MR<-read.csv("Data/Dead.csv")
MR<-unique(MR)

#dead and 
Mort<-subset(MR,Dead2==1)
Rec<-subset(MR,Dead2==2)


#plot data to check it looks ok
