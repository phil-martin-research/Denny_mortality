#script to calculate the probability of mortality for 
#beech trees based on:
#1. Size, 2. Growth rate, 3. Distance to nearest dead tree
#4. Distance to nearest live tree

#import data
rm(list=ls(all=TRUE))
Dead<-read.csv("Data/Dead_size.csv")

#subset to remove dada prior to 1984
Dead<-subset(Dead,Year>1984)
#filter out trees that ahave unrealistic growth rates
Dead<-subset(Dead,GR>-10&GR<20&relBAGR<4)
Dead<-subset(Dead,!is.na(Dead))

#load packages
library(ggplot2)
library(GGally)
library(lme4)
library(lattice)
library(MuMIn)
library(ResourceSelection)
str(Dead)


#subset data into individual species
Dead_F<-subset(Dead,Species=="F")
Dead_Q<-subset(Dead,Species=="Q")
Dead_I<-subset(Dead,Species=="I")


head(Dead_F)

#candidates for growth rate variables
ggplot(Dead_F,aes(x=GR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F,aes(x=BAGR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F,aes(x=relGR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F,aes(x=relBAGR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")

#now build glmms of these
M0<-glmer(Dead~1+(1|SL),Dead_F,family="binomial")
M_GR<-glmer(Dead~GR+(GR|SL),Dead_F,family="binomial")
M_BAGR<-glmer(Dead~BAGR+(BAGR|SL),Dead_F,family="binomial")
M_relGR<-glmer(Dead~relGR+(relGR|SL),Dead_F,family="binomial")
M_relBAGR<-glmer(Dead~relBAGR+(relBAGR|SL),Dead_F,family="binomial")

AICc(M0,M_GR,M_BAGR,M_relGR,M_relBAGR)

plot(Dead_F$GR,plogis(predict(M_GR,re.form=NA)))
points(Dead_F$GR,plogis(predict(M_GR)),col="red")

#growth rate in DBH looks best - slow growing trees are more likley to die

#candidates for size variables
ggplot(Dead_F,aes(x=DBH2,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F,aes(x=BA2,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")


#now size
M0<-glmer(Dead~1+(1|SL),Dead_F,family="binomial")
M_DBH<-glmer(Dead~DBH2+(DBH2|SL),Dead_F,family="binomial")
M_DBH2<-glmer(Dead~DBH2+I(DBH2^2)+(DBH2+I(DBH2^2)|SL),Dead_F,family="binomial")
M_BA<-glmer(Dead~BA2+(BA2|SL),Dead_F,family="binomial")
M_BA2<-glmer(Dead~BA2+I(BA2^2)+(BA2+I(BA2^2)|SL),Dead_F,family="binomial")

AICc(M0,M_DBH,M_DBH2,M_BA,M_BA2)
plot(Dead_F$DBH2,plogis(predict(M_DBH,re.form=NA)))
points(Dead_F$DBH2,plogis(predict(M_DBH)),col="red")

#bigger trees are more likley to die - DBH is the best predictor of this


#candidates for spatial relationships with dead trees
ggplot(Dead_F,aes(x=Dead_dist,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F,aes(x=Dead_No,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F,aes(x=Dead_BA,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")


#now spatial relationships with dead trees
M0<-glmer(Dead~1+(1|SL),Dead_F,family="binomial")
M_D_dist<-glmer(Dead~Dead_dist+(Dead_dist|SL),Dead_F,family="binomial")
M_D_no<-glmer(Dead~Dead_No+(Dead_No|SL),Dead_F,family="binomial")
M_D_BA<-glmer(Dead~Dead_BA+(Dead_BA|SL),Dead_F,family="binomial")
M_D_BA2<-glmer(Dead~Dead_BA+(1|SL),Dead_F,family="binomial")

AICc(M0,M_D_dist,M_D_no,M_D_BA,M_D_BA2)

plot(Dead_F$Dead_BA,plogis(predict(M_D_BA2,re.form=NA)))
points(Dead_F$Dead_BA,plogis(predict(M_D_BA2)),col="red")

#so BA of dead trees seems best

#candidates for spatial relationships with live trees
ggplot(Dead_F,aes(x=Live_No,y=Dead,colour=factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_F,aes(x=Live_BA,y=Dead,colour=factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")

#now spatial relationships with dead trees
M0<-glm(Dead~1+(1|SL),Dead_F,family="binomial")
M_L_BA<-glmer(Dead~Live_BA+(Live_BA|SL),Dead_F,family="binomial")
M_L_no<-glmer(Dead~Live_No+(Live_No|SL),Dead_F,family="binomial")

AICc(M0,M_L_BA,M_L_no)
#looks like live numbers is best

#now a model of DBH, growth, distance to dead trees, number of live trees
M0.1<-glmer(Dead~1+(DBH2|SL)+(GR|SL)+(Dead_BA|SL)+(Live_No|SL),Dead_F,family="binomial")
M0.2<-glmer(Dead~1+(DBH2|SL)+(GR|SL)+(Dead_BA|SL),Dead_F,family="binomial")
M0.3<-glmer(Dead~1+(DBH2|SL)+(GR|SL),Dead_F,family="binomial")
M0.4<-glmer(Dead~1+(DBH2|SL),Dead_F,family="binomial")
M0.5<-glmer(Dead~1+(GR|SL),Dead_F,family="binomial")
AICc(M0.1,M0.2,M0.3,M0.4,M0.5)




M1<-glmer(Dead~DBH2+GR+Dead_BA+(DBH2|SL)+(GR|SL),Dead_F,family="binomial")
M2<-glmer(Dead~DBH2+GR+Dead_BA+(DBH2|SL)+(GR|SL)+(Dead_BA|SL),Dead_F,family="binomial")
M3<-glmer(Dead~DBH2+GR+(DBH2|SL)+(GR|SL),Dead_F,family="binomial")
M4<-glmer(Dead~GR+(GR|SL),Dead_F,family="binomial")

M5<-glmer(Dead~GR+Live_No+(GR|SL)+(Live_No|SL),Dead_F,family="binomial")

options(na.action = "na.fail")
models<-dredge(M1,trace = T)

MS<-model.sel(models)

Avs<-model.avg(MS,fit = T)


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
