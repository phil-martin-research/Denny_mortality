#script to look at differences in plot level mortality rate

library(plyr)
library(ggplot2)
library(MuMIn)
library(dplyr)
library(tidyr)
library(reshape2)
library(lme4)


#load in data
Mort<-read.csv("Data/For_mort_rates.csv")
head(Mort)
Mort$Rate_corrected<-ifelse(Mort$Rate==0,Mort$Rate+0.001,Mort$Rate-0.001)
Mort$Rate_logis<-qlogis(Mort$Rate_corrected)
Mort<-subset(Mort,Period!="1964-1984")

#subset data to only include data for oak, beech and holly
Mort_sub<-subset(Mort,Species=="Q"|Species=="F"|Species=="I")
Mort_sub$index<-as.numeric(row.names(Mort_sub))

#first I need to explore the data
#1 - Check for outliers
ggplot(Mort_sub,aes(y=index,x=Rate))+geom_point()+facet_grid(Period~Species)
ggplot(Mort_sub,aes(y=index,x=Mort_adj))+geom_point()+facet_grid(Period~Species)

ggplot(Mort_sub,aes(y=index,x=Sand))+geom_point()
ggplot(Mort_sub,aes(y=index,x=Silt))+geom_point()
ggplot(Mort_sub,aes(y=index,x=Clay))+geom_point()

#very little evidence of outliers
#2 - normality of data
ggplot(Mort_sub,aes(x=Rate))+geom_histogram()+facet_wrap(~Species)
ggplot(Mort_sub,aes(x=Sand))+geom_histogram()#sand data is skewed towards larger values >50
ggplot(Mort_sub,aes(x=Silt))+geom_histogram()#there is little skew in the silt data
ggplot(Mort_sub,aes(x=Clay))+geom_histogram()#clay data is skewed towards smaller values <20
ggplot(Mort_sub,aes(x=Mort_adj))+geom_histogram()#adjacent mortality data is clearly skewed towards smaller values
ggplot(Mort_sub,aes(x=log(Mort_adj+0.0001)))+geom_histogram()#adjacent mortality data is clearly skewed towards smaller values

#6 - exploration of relationships between x and y variables
ggplot(Mort_sub,aes(x=Sand,y=Rate))+geom_point()+facet_grid(Period~Species)+scale_y_continuous(limits=c(0,0.25))
ggplot(Mort_sub,aes(x=Silt,y=Rate))+geom_point()+facet_grid(Period~Species)+scale_y_continuous(limits=c(0,0.25))
ggplot(Mort_sub,aes(x=Clay,y=Rate))+geom_point()+facet_grid(Period~Species)+scale_y_continuous(limits=c(0,0.25))
ggplot(Mort_sub,aes(x=Mort_adj,y=Rate))+geom_point()+facet_grid(Period~Species)+scale_y_continuous(limits=c(0,0.25))

#there seem to be no statistical problems

#subset data to only include beech species
Mort_F<-subset(Mort_sub,Species=="F")

M0_F<-lmer(Rate~1+(1|Block),data=Mort_F)
M1_F<-lmer(Rate~Sand+(1|Block),data=Mort_F)
M2_F<-lmer(Rate~Clay+(1|Block),data=Mort_F)
M3_F<-lmer(Rate~Period+(1|Block),data=Mort_F)
M4_F<-lmer(Rate~Mort_adj+(1|Block),data=Mort_F)
AICc(M0_F,M1_F,M2_F,M3_F)

#subset data to only include oak trees
Mort_Q<-subset(Mort_sub,Species=="Q")

M0_Q<-lmer(Rate~1+(1|Block),data=Mort_Q)
M1_Q<-lmer(Rate~Sand+(1|Block),data=Mort_Q)
M2_Q<-lmer(Rate~Clay+(1|Block),data=Mort_Q)
M3_Q<-lmer(Rate~Period+(1|Block),data=Mort_Q)
M4_Q<-lmer(Rate~Mort_adj+(1|Block),data=Mort_Q)
AICc(M0_Q,M1_Q,M2_Q,M3_Q,M4_Q)

#subset data to only include holly trees
Mort_I<-subset(Mort_sub,Species=="I")

M0_I<-lmer(Rate~1+(1|Block),data=Mort_I)
M1_I<-lmer(Rate~Sand+(1|Block),data=Mort_I)
M2_I<-lmer(Rate~Clay+(1|Block),data=Mort_I)
M3_I<-lmer(Rate~Period+(1|Block),data=Mort_I)
M4_I<-lmer(Rate~Mort_adj+(1|Block),data=Mort_I)
AICc(M0_I,M1_I,M2_I,M3_I,M4_I)

#noe of the models seem to be much good. The null model is almost always the 'best' model.