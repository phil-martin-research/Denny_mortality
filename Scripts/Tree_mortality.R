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

#and divide by Length of survey period
Mort$Dead_corr<-Mort$Dead/Mort$Length
#and arcsine square root the data
Mort$Dead_sin<-asin(sqrt(Mort$Dead_corr))
head(Mort)

#remove trees for which there is no status
Mort_CC<-Mort[complete.cases(Mort$Dead),]


ggplot(Mort_CC,aes(x=DBH,y=Dead_sin))+facet_wrap(~Period)+geom_smooth(method="glm",formula=y ~ poly(x, 2, raw=TRUE))+geom_rug()+geom_point()

M1<-lmer(Dead_sin~DBH+(1|Tree_ID),data=Mort_CC)
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

Mort_CC$bin <- cut(Mort_CC$DBH,seq(0,max(Mort_CC$DBH),5))
Mort_bin <- ddply(Mort_CC, "bin", function(DF) {
  data.frame(mean=numcolwise(mean)(DF), length=numcolwise(length)(DF))
})
head(Mort_bin)

ggplot(Mort_bin,aes(x=mean.DBH,y=mean.Dead_corr,size=length.DBH))+geom_point()


AICc(M1,M2,M3,M4,M5,M6,M7)

M2<-lmer(Dead~DBH+(Length|Tree_ID),data=Mort,family=binomial(link = "logit"))
M3<-glmer(cbind(Dead,Length-Dead)~DBH+I(DBH^2)+(1|Tree_ID),data=Mort,family=binomial)

AICc(M2,M3)
summary(M2)



#work out distance to nearest dead tree
Distances<-rdist(Mort_88_2[9:10])
Mort_88_2$Tree_ID
arrayInd(4, dim(Distances))

#produce function to remove distances for live trees
for (i in 1:nrow(Distances)){
  for (y in 1:ncol(Distances)){
    Distances[i,y]<-ifelse(Mort_88_2$Dead[i]==1,Distances[i,y],NA)
  }
}
Distances[Distances==0]<-NA

#now work out minimum distance to a dead tree
for (i in 1:ncol(Distances)){
  Mort_88_2$Dead_Dist[i]<-min(Distances[,i],na.rm = T)
}
summary(Mort_88_2$Dead_Dist)

#now work out DBH of nearest dead tree
for (i in 1:ncol(Distances)){
  Index<-match(min(Distances[,i],na.rm = T),Distances[,i])
  Mort_88_2$Dead_DBH[i]<-Mort_88_2$DBH[Index]
}

summary(Mort_88_2$Dead_DBH)


#model of mortality from 1964-1988
M0<-glm(Dead~1,data=Mort_88_2,family=binomial(link = "logit"))
M1<-glm(Dead~DBH,data=Mort_88_2,family=binomial(link = "logit"))
M2<-glm(Dead~DBH+I(DBH^2),data=Mort_88_2,family=binomial(link = "logit"))
M3<-glm(Dead~DBH+Dead_Dist,data=Mort_88_2,family=binomial(link = "logit"))
M4<-glm(Dead~Dead_Dist,data=Mort_88_2,family=binomial(link = "logit"))
M5<-glm(Dead~Dead_Dist+Dead_DBH,data=Mort_88_2,family=binomial(link = "logit"))
M6<-glm(Dead~Dead_Dist*Dead_DBH,data=Mort_88_2,family=binomial(link = "logit"))
M7<-glm(Dead~DBH+Species,data=Mort_88_2,family=binomial(link = "logit"))
M8<-glm(Dead~DBH*Species,data=Mort_88_2,family=binomial(link = "logit"))



plot(Mort_88_2$Dead_Dist,(plogis(predict(M4)))/24)
plot(Mort_88_2$DBH,(plogis(predict(M1)))/24)
plot(Mort_88_2$DBH,Mort_88_2$Dead)

auc.tmp <- performance(predict(M1),"auc")
auc <- as.numeric(auc.tmp@y.values)
plot(M1)

################################################################################
#below this point is the survival analysis code, not sure that this is much use#
################################################################################


#include only complete cases - those with data on whether stems are alive or dead and not nas
DBH<-DBH[complete.cases(DBH[,6]),]
#recode deatch as 1 and alive as 0
DBH$Death<-ifelse(DBH$Status==1,0,1)

#create function to work out time since first measurment for each tree
Tree_ID2<-unique(DBH$Tree_ID)
Tree_surv<-NULL
for (i in 1:length(Tree_ID)){
  Tree_sub<-subset(DBH,Tree_ID==Tree_ID2[i])
  Tree_sub$TSFM<-max(Tree_sub$Year)-min(Tree_sub$Year)
  Tree_sub2<-data.frame(start=min(Tree_sub$Year),stop=max(Tree_sub$Year),min_dbh=min(Tree_sub$DBH),max_dbh=max(Tree_sub$DBH),
             gr=(max(Tree_sub$DBH)-min(Tree_sub$DBH))/max(Tree_sub$TSFM),death=max(Tree_sub$Death),Sp=Tree_sub$Species[1])
  Tree_surv<-rbind(Tree_surv,Tree_sub2)
}

head(Tree_surv)

#subset to include only trees that have been measured at two time periods or more
Tree_surv<-subset(Tree_surv,stop>start)
Tree_surv<-subset(Tree_surv,max_dbh>min_dbh)
selected<-c("F","I","Q")
Tree_surv<-Tree_surv[Tree_surv$Sp %in% selected,]
Tree_surv$Sp<-factor(Tree_surv$Sp)

#create sruvival object
Tree_surv$S<-Surv(Tree_surv$min_dbh,Tree_surv$max_dbh,Tree_surv$death)
summary(Tree_surv$Sp)

(xtabs( ~ S+S, data=Tree_surv))

#create model using  coph model type
M1<- coxph(S~Sp, data = Tree_surv)

#plot base survival over dbh
ggsurv(survfit(M1))

#plot species specific survival
tree_surv2<- survfit(Surv(Tree_surv$min_dbh,Tree_surv$max_dbh,Tree_surv$death)~Sp, data = Tree_surv)
ggsurv(tree_surv2,plot.cens = F)




#plot base survival by speceis


