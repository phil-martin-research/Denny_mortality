#script to look at the probability of a tree dying

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
library(zoo)
library(KernSmooth)

#import data
Location<-read.csv("Data/Plot_coords.csv")
Location<-unique(Location[,c(3,5:6)])
DBH<-read.csv("Data/Denny_trees_cleaned.csv")
#subset trees to give only those inside plots
DBH<-subset(DBH,In_out=="In")

#first some exploratory analysis to look at variation in tree deaths by year
#subset the data to only include data for the enclosed transect
DBH<-subset(DBH,Block<51&Year>1960)
DBH<-subset(DBH,DBH>=45)


ggplot(DBH,aes(x=Easting,y=Northing,colour=as.factor(Status)))+geom_point(shape=1)+facet_wrap(~Year)

#create loop to give trees that were alive at previous time period
#loop subsets to give trees that are alive at beginning of survey period
#and then determines mortality during period (Mort sub)
#Tree_sub section makes assumption that when a tree is missing from a survey
#it has died

Surv_years<-cbind(c(1964,1984,1988,1996),c(1984,1988,1996,2014))
Mort<-NULL
for (i in 1:nrow(Surv_years)){
  Alive<-subset(DBH,Year==Surv_years[i,1]&Status==1)[,3]
  for (y in 1:length(Alive)){
   Tree_sub<-subset(DBH,Tree_ID==Alive[y])[1,]
   Tree_sub$Year<-Surv_years[i,2]
   Tree_sub$Status<-0
   Mort_sub<-subset(DBH,Tree_ID==Alive[y]&Year==Surv_years[i,2])
   if(nrow(Mort_sub)==0){
     Mort_sub2<-Tree_sub
   }else{
      Mort_sub2<-Mort_sub
    }
  Mort_sub2$Period<-as.factor(paste(Surv_years[i,1],"-",Surv_years[i,2],sep = ""))
  Mort_sub2$Length<-Surv_years[i,2]-Surv_years[i,1]
  Mort<-rbind(Mort_sub2,Mort)
}
}

#now change status to 1=dead and 0=alive
Mort$Dead<-ifelse(Mort$Status==1,0,1)


#subset the data to give only data from the 1964-1984 survey

Mort_64_84<-subset(Mort,Period=="1964-1984")


#remove trees for which there is no status
Mort_CC<-Mort[complete.cases(Mort$Dead),]
head(Mort_CC)

#bin data for tree size into 5cm size classes
summary(Mort_CC$DBH)
Mort_CC$Size_bin<-findInterval(Mort_CC$DBH,seq(0,150,10))

plot(Mort_CC$Size_bin,Mort_CC$DBH)

#create plot to show the balance of dead vs alive for size classses
Plots3<-ddply(Mort_CC,.(Dead,Size_bin),summarise,freq=length(Dead))

Plots4<-ddply(Mort_CC,.(Size_bin,Year),summarise,mean=mean(Dead/Length),freq=length(Dead))


ggplot(Plots3,aes(x=Size_bin*10,y=Dead,size=freq))+geom_point()+geom_point(data=Plots4,aes(y=mean),size=3,colour="red",shape=0)+xlim(0,150)

ggplot(Mort_CC,aes(x=DBH))+geom_histogram()+facet_wrap(~Year)

#look at frequency distributions
Mort_CC_alive<-subset(Mort_CC,Dead==0&Species=="F")

Plots4<-ddply(Mort_CC_alive,.(Size_bin,Year),summarise,freq=length(Dead))
head(Plots4)

head(Mort_CC_alive)


ggplot(Mort_CC_alive,aes(x=Year,y=DBH,group=Tree_ID))+geom_point()+geom_line(alpha=0.2)+geom_smooth(aes(group=NULL),method="lm",size=4)


head(Mort_CC)


ggplot(Mort_CC,aes(x=DBH,y=Dead))+facet_wrap(~Period)+geom_smooth(method="glm",formula=y ~ poly(x, 2, raw=TRUE),family=binomial)+geom_rug()+geom_point()

M1<-glmer(Dead~DBH+(Length|Tree_ID),data=Mort_CC,family=binomial)
M2<-glmer(Dead~DBH+I(DBH^2)+(Length|Tree_ID),data=Mort_CC,family=binomial)
M3<-glmer(Dead~DBH*Period+I(DBH^2)+(Length|Tree_ID),data=Mort_CC,family=binomial)

AICc(M1,M2,M3)

summary(M1)
summary(M2)
summary(M3)

M2<-lmer(Dead_sin~DBH+I(DBH^2)+(1|Tree_ID),data=Mort_CC)
M3<-lmer(Dead_sin~DBH+I(DBH^2)+Species+(1|Tree_ID),data=Mort_CC)
M4<-lmer(Dead_sin~DBH*Species+I(DBH^2)+(1|Tree_ID),data=Mort_CC)

AICc(M1,M2,M3,M4)

ggplot(Mort_CC,aes(x=DBH))+geom_histogram()+facet_grid(Species~Year)


plot(Mort_CC$DBH,sin(predict(M2,re.form=NA))^2)


plot(Mort_CC$DBH,plogis((predict(M3,re.form=NA))))
plot(Mort_CC$DBH,Mort_CC$Dead_corr)
plot(Mort_CC$DBH,resid(M3))
plot(fitted(M2),resid(M3))


fit_dead<-(data.frame(DBH=Mort_CC$DBH,fit=(sin(predict(M2,re.form=NA)))^2))
fit_dead<-fit_dead[with(fit_dead,order(DBH)),]
lines(fit_dead$DBH,fit_dead$fit,col="red")
rolling<-data.frame(Dead=Mort_CC$Dead_corr,DBH=Mort_CC$DBH)
rolling<-rolling[with(rolling,order(DBH)),]





#bin data into 5cm DBH classes

Mort_CC$bin <- cut(Mort_CC$DBH,seq(0,max(Mort_CC$DBH),10))
Mort_bin <- ddply(Mort_CC, "bin", function(DF) {
  data.frame(mean=numcolwise(mean)(DF), length=numcolwise(length)(DF))
})
head(Mort_bin)

ggplot(Mort_bin,aes(x=mean.DBH,y=mean.Dead_corr,size=length.DBH))+geom_point()


