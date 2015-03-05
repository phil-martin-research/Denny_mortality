#script to calculate the probability of mortality for 
#beech trees based on:
#1. Size, 2. Growth rate, 3. Distance to nearest dead tree
#4. Distance to nearest live tree

#import data
rm(list=ls(all=TRUE))
Dead<-read.csv("Data/Dead_size.csv")

#subset to remove data prior to 1984
Dead<-subset(Dead,Year>1984)
#filter out trees that ahave unrealistic growth rates
Dead<-subset(Dead,GR>-20&GR<20&relBAGR<4)
Dead<-subset(Dead,!is.na(Dead))

#load packages
library(ggplot2)
library(GGally)
library(lme4)
library(lattice)
library(MuMIn)
library(ResourceSelection)
library(arm)
library(rms)
library(MKmisc)



#subset data into individual species
Dead_F<-subset(Dead,Species=="F")
keeps<-c("Dead","Easting","Northing","SL","GR","BAGR","relGR","relBAGR","DBH2","BA2","relSize","Dead_dist","Dead_No")
Dead_F<-Dead_F[keeps]
str(Dead_F)

#standardise variables
Dead_F_st<-cbind(Dead_F[,1:4],apply(X=Dead_F[,5:ncol(Dead_F)],MARGIN=2,FUN=function(x) {(x-mean(x))/sd(x)}))

#candidates for growth rate variables
ggplot(Dead_F_st,aes(x=GR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F_st,aes(x=BAGR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F_st,aes(x=relGR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F_st,aes(x=relBAGR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")

#now build glmms of these
M0<-glmer(Dead~1+(1|SL),Dead_F_st,family="binomial")
M_GR<-glmer(Dead~GR+(GR|SL),Dead_F_st,family="binomial")
M_BAGR<-glmer(Dead~BAGR+(BAGR|SL),Dead_F_st,family="binomial")
M_relGR<-glmer(Dead~relGR+(relGR|SL),Dead_F_st,family="binomial")
M_relBAGR<-glmer(Dead~relBAGR+(relBAGR|SL),Dead_F_st,family="binomial")

AICc(M0,M_GR,M_BAGR,M_relGR,M_relBAGR)

plot(Dead_F_st$GR,plogis(predict(M_GR,re.form=NA)))
points(Dead_F_st$GR,plogis(predict(M_GR)),col="red")

#growth rate in DBH looks best - slow growing trees are more likley to die

#candidates for size variables
ggplot(Dead_F_st,aes(x=DBH2,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F_st,aes(x=BA2,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")


#now size
M0<-glmer(Dead~1+(1|SL),Dead_F_st,family="binomial")
M_DBH<-glmer(Dead~DBH2+(DBH2|SL),Dead_F_st,family="binomial")
M_DBH2<-glmer(Dead~DBH2+I(DBH2^2)+(DBH2|SL),Dead_F_st,family="binomial")
M_BA<-glmer(Dead~BA2+(BA2|SL),Dead_F_st,family="binomial")
M_BA2<-glmer(Dead~BA2+I(BA2^2)+(BA2|SL),Dead_F_st,family="binomial")

AICc(M0,M_DBH,M_DBH2,M_BA,M_BA2)
plot(Dead_F_st$BA2,plogis(predict(M_BA2,re.form=NA)))
points(Dead_F_st$BA2,plogis(predict(M_BA2)),col="red")

#bigger trees are more likley to die - DBH is the best predictor of this


#candidates for spatial relationships with dead trees
ggplot(Dead_F_st,aes(x=Dead_dist,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F_st,aes(x=Dead_No,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")

#now spatial relationships with dead trees
M0<-glmer(Dead~1+(1|SL),Dead_F_st,family="binomial")
M_D_dist<-glmer(Dead~Dead_dist+(Dead_dist|SL),Dead_F_st,family="binomial")
M_D_no<-glmer(Dead~Dead_No+(Dead_No|SL),Dead_F_st,family="binomial")

AICc(M0,M_D_dist,M_D_no)

plot(Dead_F_st$Dead_BA,plogis(predict(M_D_BA2,re.form=NA)))
points(Dead_F_st$Dead_BA,plogis(predict(M_D_BA2)),col="red")

#so BA of dead trees seems best
#remove variables that I'm not going to use in the analyses
Dead_F_st$DBH2_sq<-Dead_F_st$DBH2^2
keeps<-c("Dead","DBH2","DBH2_sq","GR","Dead_No","SL")
Dead_F_st2<-Dead_F_st[keeps]


ggpairs(Dead_F_st2[,2:5])



#now a model of DBH, growth, distance to dead trees, number of live trees
M0.1<-glmer(Dead~1+(DBH2|SL)+(GR|SL)+(Dead_BA|SL)+(Live_No|SL),Dead_F,family="binomial")
M0.2<-glmer(Dead~1+(DBH2|SL)+(GR|SL)+(Dead_BA|SL),Dead_F,family="binomial")
M0.3<-glmer(Dead~1+(DBH2|SL)+(GR|SL),Dead_F_st2,family="binomial")
M0.4<-glmer(Dead~1+(DBH2|SL),Dead_F_st2,family="binomial")
M0.5<-glmer(Dead~1+(GR|SL),Dead_F_st2,family="binomial")
AICc(M0.1,M0.2,M0.3,M0.4,M0.5)


#now put in fixed effects


M1<-glmer(Dead~DBH2*GR+DBH2_sq+Dead_No+(DBH2|SL)+(GR|SL),Dead_F_st2,family="binomial")

#now do model averaging
options(na.action = "na.fail")
models<-dredge(M1,trace = T,subset=dc(DBH2,DBH2_sq))

#produce model selection table
MS<-model.sel(models)

#produce le Cessie-van Houwelingen normal test statistic for these models 
M1_sel<-glmer(Dead~DBH2+GR+(DBH2|SL)+(GR|SL),Dead_F_st2,family="binomial")
HLgof.test(fit = fitted(M1_sel), obs=Dead_F_st2$Dead,X=model.matrix(Dead ~ DBH2+GR,data=Dead_F_st2))
#calculate ROC
fit<-fitted(M1_sel)
fit.pos <- fit[Dead_F_st2$Dead==1]
length(fit.pos)
fit.neg <- fit[Dead_F_st2$Dead==0]
wilcox.test(x=fit.pos, y=fit.neg)
283362/(length(fit.pos)*length(fit.neg))





str(MS$DBH2)


Avs<-model.avg(MS,fit = T,subset =cumsum(weight) <= 0.95)

#produce le Cessie-van Houwelingen normal test statistic for these models 
HLgof.test(fit = fitted(M1_sel), obs=Dead_F_st2$Dead,X=model.matrix(Dead ~ DBH2+GR,data=Dead_F_st2))
HLgof.test(fit = fitted(M1), obs=Dead_F_st2$Dead,X=model.matrix(Dead ~ DBH2+GR+DBH2_sq+Dead_No,data=Dead_F_st2))


Preds<-predict(Avs)

#look at partial residuals for this
DBH_res<-(Dead_F_st2$Dead-plogis(Preds))/
          ((plogis(Preds))*(1-plogis(Preds)))+
          Dead_F_st2$DBH2*coef(Avs)[2]



GR_res<-((Dead_F_st2$Dead-predict(Avs))/(predict(Avs)*(1-predict(Avs))))+Dead_F_st2$GR*coef(Avs)[3]
Dead_res<-((Dead_F_st2$Dead-predict(Avs))/(predict(Avs)*(1-predict(Avs))))+Dead_F_st2$Dead_No*coef(Avs)[3]

par(mfrow=c(1,2))

visreg(M2,xvar = c("DBH2","GR"),)
?visreg


qplot(Dead_F_st2$DBH2,DBH_res)+geom_smooth()
qplot(Dead_F_st2$GR,GR_res)+geom_smooth()
qplot(Dead_F_st2$Dead_No,Dead_res)+geom_smooth()



#create predictions for growth rate
GR<-expand.grid(GR=seq(min(Dead_F$GR),max(Dead_F$GR),0.1),DBH2=mean(Dead_F$DBH2),Dead_BA=mean(Dead_F$Dead_BA))
GR$Preds<-predict(Avs,newdata = GR)
GR$SE<-predict(Avs,newdata = GR,se.fit = T)$se.fit
GR$Model<-"Growth rate"

plot(Dead_F$GR,Dead_F$Dead)
points(GR$GR,plogis(Preds))

#create predictions for DBH
DBH<-expand.grid(DBH2=seq(min(Dead_F$DBH2),max(Dead_F$DBH2),0.01),GR=mean(Dead_F$GR),Dead_BA=mean(Dead_F$Dead_BA))
DBH$Preds<-predict(Avs,newdata = DBH)
DBH$SE<-predict(Avs,newdata = DBH,se.fit = T)$se.fit
DBH$Model<-"DBH"

plot(Dead_F$DBH2,Dead_F$Dead)
points(DBH$DBH2,plogis(DBH$Preds))


#create predictions for dead tree BA
DT<-expand.grid(GR=mean(Dead_F$GR),DBH2=mean(Dead_F$DBH2),Dead_BA=seq(min(Dead_F$Dead_BA),max(Dead_F$Dead_BA),0.01))
DT$Preds<-predict(Avs,newdata = DT)
DT$SE<-predict(Avs,newdata = DT,se.fit = T)$se.fit
DT$Model<-"Distance to dead tree"

plot(Dead_F$Dead_BA,Dead_F$Dead)
points(DT$Dead_BA,plogis(DT$Preds))

#save_predictions to a csv
Preds<-rbind(GR,DBH,DT)
head(Preds)
write.csv(Preds,"Data/Beech_mort_preds.csv",row.names=F)


Dead_F$Resid<-resid(M3)

#look at spatial autocorrelation of residuals
ggplot(Dead_F,aes(x=Easting,y=Northing,size=Resid^2,colour=Resid))+geom_point()+facet_wrap(~Year)+scale_colour_gradient2()

library(ncf)
ncf.cor <- correlog(Dead_F$Easting, Dead_F$Northing, Dead_F$Resid,
                    increment=2, resamp=500)

str(ncf.cor)

Resid_correl<-data.frame(n=ncf.cor$n,dist=ncf.cor$mean.of.class,corr=ncf.cor$correlation,p=ncf.cor$p)

ggplot(Resid_correl,aes(x=dist,y=corr,size=n))+geom_point()+geom_line(size=1)
