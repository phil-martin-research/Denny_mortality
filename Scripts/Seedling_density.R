#script to look at seedling presence/absence and density
#in Denny plots and how this relates to canopy/basal area/mature density

#author: Phil Martin
#last edited: 27/03/15

#load data
rm(list=ls(all=TRUE))
Seed<-read.csv("Data/GF_ab.csv")
Plots<-read.csv("Data/Denny_plots_trees_M.csv")

#load packages
library(ggplot2)
library(lme4)

#change year for seedling to match other dataset
Seed$Year<-ifelse(Seed$Year==2001,1996,Seed$Year)

#merge the two datsets
Seed_plot<-merge(Seed,Plots,by=c("Block","Year"))
Seed_plot[is.na(Seed_plot)] <- 0 #replace NAs with zeros

#exploratory plots
ggplot(Seed_plot,aes(x=F_BA,y=Beech))+geom_point()+facet_wrap(~Year)
ggplot(Seed_plot,aes(x=I_BA,y=Holly))+geom_point()+facet_wrap(~Year)
ggplot(Seed_plot,aes(x=Q_BA,y=Oak))+geom_point()+facet_wrap(~Year)

#currently there is not enough data to do anything with here - need data from this years survey on abundance