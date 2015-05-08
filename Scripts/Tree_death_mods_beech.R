#script to calculate the probability of mortality for 
#beech trees based on:
#1. Size, 2. Growth rate, 3. Distance to nearest dead tree
#4. Distance to nearest live tree

#load packages
library(ggplot2)
library(GGally)
library(lme4)
library(lattice)
library(MuMIn)
library(plyr)

#import data
rm(list=ls(all=TRUE))
Dead<-read.csv("Data/Dead_size.csv")
head(Dead)

#subset to remove data prior to 1984
Dead<-subset(Dead,Year>1984)
#filter out trees that have unrealistic growth rates
Dead<-subset(Dead,!is.na(Dead))
Dead<-subset(Dead,GR>-20&GR<20&relBAGR<4)

#subset data into individual species
Dead_F<-subset(Dead,Species=="F")
keeps<-c("ID2","Block","Dead","Easting","Northing","SL","Species","GR","BAGR","relGR","relBAGR","DBH2","BA2","relSize","Dead_dist","Dead_No")
Dead_F<-Dead_F[keeps]

#standardise variables following Zuur et al recommendations
head(Dead_F)
Dead_F_st<-cbind(Dead_F[,1:7],apply(X=Dead_F[,8:ncol(Dead_F)],MARGIN=2,FUN=function(x) {(x-mean(x))/sd(x)}))
head(Dead_F_st)

#candidates for growth rate variables
M0<-glmer(Dead~1+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))        
M_GR<-glmer(Dead~GR+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_BAGR<-glmer(Dead~BAGR+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_relGR<-glmer(Dead~relGR+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_relBAGR<-glmer(Dead~relBAGR+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))

AICc(M0,M_GR,M_BAGR,M_relGR,M_relBAGR)

new.dat<-data.frame(GR=seq(min(Dead_F$GR),max(Dead_F$GR),length=500),SL=1)
plot(Dead_F_st$GR,Dead_F_st$Dead)
points(new.dat$GR,plogis(predict(M_GR,newdata=new.dat,re.form=NA)),col="red")

#growth rate in DBH looks best - slow growing trees are more likley to die

#candidates for size variables
M0<-glmer(Dead~1+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))        
M_DBH<-glmer(Dead~DBH2+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_DBH2<-glmer(Dead~DBH2+I(DBH2^2)+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_BA<-glmer(Dead~BA2+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_BA2<-glmer(Dead~BA2+I(BA2^2)+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))

AICc(M0,M_DBH,M_DBH2,M_BA,M_BA2)
new.dat<-data.frame(DBH2=seq(min(Dead_F$DBH2),max(Dead_F$DBH2),length=500),SL=1)
plot(Dead_F_st$DBH2,Dead_F_st$Dead)
plot(new.dat$DBH2,plogis(predict(M_DBH,newdata=new.dat,re.form=NA)),col="red")
#bigger trees are more likley to die - DBH is the best predictor of this

#candidates for spatial relationships with dead trees
M0<-glmer(Dead~1+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))        
M_D_dist<-glmer(Dead~Dead_dist+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_D_no<-glmer(Dead~Dead_No+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
AICc(M0,M_D_dist,M_D_no)

#so distance to dead trees is best

#now remove variables that I'm not going to use in the analyses
Dead_F_st$DBH2_sq<-Dead_F_st$DBH2^2
head(Dead_F_st)
keeps<-c("Dead","DBH2","DBH2_sq","GR","Dead_dist","SL","Block")
Dead_F_st2<-Dead_F_st[keeps]

ggpairs(Dead_F_st2[,2:5]) #not much corrrelation between variables so should be safe to use them all

#now a model of DBH, growth, distance to dead trees, number of live trees
M1<-glmer(Dead~Dead_dist+DBH2+DBH2_sq+GR+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))

dotplot(ranef(M1,condVar=TRUE),
        lattice.options=list(layout=c(1,2)))

#now do model averaging
options(na.action = "na.fail")
models<-dredge(M1,trace = T,fixed="offset(log(SL))")

#produce model selection table
MS<-model.sel(models)
MS

Avs<-model.avg(MS,fit = T,subset =delta<=7)
summary(Avs)
importance(Avs)

#produce predictions from the model averaged coefficients

#first for growth rate
new.data.GR<-data.frame(GR=seq(min(Dead_F_st$GR),max(Dead_F_st$GR),length.out=500),SL=1,Dead_dist=mean(Dead_F_st$Dead_dist),DBH2=mean(Dead_F_st$DBH2))
new.data.GR$DBH2_sq<-new.data.GR$DBH2^2
new.data.GR$Dead<-plogis(predict(Avs,newdata =new.data.GR,re.form=NA))
#next for DBH
new.data.DBH<-data.frame(GR=mean(Dead_F_st$GR),SL=1,Dead_dist=mean(Dead_F_st$Dead_dist),DBH2=seq(min(Dead_F_st$DBH2),max(Dead_F_st$DBH2),length.out=500))
new.data.DBH$DBH2_sq<-new.data.GR$DBH2^2
new.data.DBH$Dead<-plogis(predict(Avs,newdata =new.data.DBH,re.form=NA))
plot(new.data.DBH$DBH2,new.data.DBH$Dead)
#next for distance to dead tree
new.data.Dead<-data.frame(GR=mean(Dead_F_st$GR),SL=1,Dead_dist=seq(min(Dead_F_st$Dead_dist),max(Dead_F_st$Dead_dist),length.out=500),DBH2=mean(Dead_F_st$DBH2))
new.data.Dead$DBH2_sq<-new.data.GR$DBH2^2
new.data.Dead$Dead<-plogis(predict(Avs,newdata =new.data.Dead,re.form=NA))
plot(new.data.Dead$Dead_dist,new.data.Dead$Dead)
