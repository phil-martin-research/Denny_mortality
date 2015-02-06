#script to look at the number of trees recruited per year in Denny

rm(list=ls(all=TRUE))

#open packages neeeded for analysis
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(nlme)
library(MuMIn)
library(gridExtra)
library(MASS)
library(survival)
library(GGally)
library(lme4)
library(fields)
library(ROCR)

#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Location<-read.csv("Plot_coords.csv")
Location<-unique(Location[,c(3,5:6)])
DBH<-read.csv("Denny_trees_cleaned.csv")
head(DBH)
unique(DBH$Year)
head(Location)
#subset trees to give only those inside plots
DBH<-subset(DBH,In_out=="In")

#first some exploratory analysis to look at variation in tree deaths by year
#subset the data to only include data for the enclosed transect
DBH<-subset(DBH,Block<52)

ggplot(subset(DBH),aes(x=Easting,y=Northing,colour=as.factor(Species)))+geom_point(shape=16,alpha=0.2)+facet_wrap(~Year)


#create loop to give trees that were not in previous time period

#loop subsets to give trees that are alive at beginning of survey period
#and then determines mortality during period (Mort sub)
#Tree_sub section makes assumption that when a tree is missing from a survey
#it has died
Surv_years<-cbind(c(1964,1984,1988,1996),c(1984,1988,1996,2014))
Rec<-NULL
for (i in 2:nrow(Surv_years)){
  Alive<-subset(DBH,Year<=Surv_years[i,1])[,3]
  Alive<-factor(Alive)
  sort(Alive)
  sort(Alive2$Tree_ID)
  Alive2<-subset(DBH,Year==Surv_years[i,2])
  Alive2$Tree_ID<-factor(Alive2$Tree_ID)
  Alive3<-subset(Alive2, !((Alive2$Tree_ID) %in% (Alive)))
  Alive4<-count(Alive3,vars =c("Species","Block"))
  Alive4$Year<-Surv_years[i,2]
  Alive4$Per_year<-Alive4$freq/(Surv_years[i,2]-Surv_years[i,1])
  Rec<-rbind(Rec,Alive4)
}

unique(DBH$Species)

Recruitment2<-data.frame(Block=rep(unique(DBH$Block),4*8),Year=rep(c(c(1984,1988,1996,2014)),times =8,each=48),Species=rep(unique(DBH$Species),each=48*4),Sp_lab=rep(c("White birch","Oak","Holly","Beech","Sycamore","Ash","Yew","Douglas fir"),each=48*4))
Recruitment2

Recruitment3<-merge(Recruitment2,Rec,by = c("Block","Year","Species"),all = T)

for (i in 1:nrow(Recruitment3)){
Recruitment3$Per_year[i]<-ifelse(is.na(Recruitment3$Per_year[i]),0,Recruitment3$Per_year[i])
}

Recruitment3$Per_year_trans<-plogis(Recruitment3$Per_year)-mean(plogis(Recruitment3$Per_year))

M0<-lme(Per_year_trans~1,random=~1|Block,data=Recruitment3)
M1<-lme(Per_year_trans~Species,random=~1|Block,data=Recruitment3)
M2<-lme(plogis(Per_year)+Year~Species,random=~1|Block,data=Recruitment3)
M3<-lme(plogis(Per_year)*Year~Species,random=~1|Block,data=Recruitment3)

plot(M0)
qqnorm(M1)

Rec_plot1<-ggplot(Recruitment3,aes(x=Sp_lab,y=Per_year,colour=Sp_lab))+geom_boxplot()+scale_y_sqrt()


ggplot(subset(DBH,Species=="I"&Year==1996|Species=="I"&Year==2014),aes(x=Year,y=DBH,colour=as.factor(Status),group=Tree_ID))+geom_point()+geom_line()
