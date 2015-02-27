#script to calculate the probability of mortality for 
#holly trees based on:
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


#subset data to give only oaks
Dead_I<-subset(Dead,Species=="I")

#candidates for growth rate variables
ggplot(Dead_I,aes(x=GR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_I,aes(x=BAGR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_I,aes(x=relGR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_I,aes(x=relBAGR,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")

#now build glmms of these
M0<-glmer(Dead~1+(1|SL),Dead_I,family="binomial")
M_GR<-glmer(Dead~GR+(GR|SL),Dead_I,family="binomial")
M_BAGR<-glmer(Dead~BAGR+(BAGR|SL),Dead_I,family="binomial")
M_relGR<-glmer(Dead~relGR+(relGR|SL),Dead_I,family="binomial")
M_relBAGR<-glmer(Dead~relBAGR+(relBAGR|SL),Dead_I,family="binomial")

AICc(M0,M_GR,M_BAGR,M_relGR,M_relBAGR)

plot(Dead_I$relGR,plogis(predict(M_relGR,re.form=NA)))
points(Dead_I$relGR,plogis(predict(M_relGR)),col="red")

#relative growth rate in DBH looks best - slow growing trees are more likley to die

#candidates for size variables
ggplot(Dead_I,aes(x=DBH2,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_I,aes(x=BA2,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
Dead_I$DBH2_stand<-Dead_I$DBH2/max(Dead_I$DBH2)
Dead_I$BA2_stand<-Dead_I$BA2/max(Dead_I$BA2)

#now size
M0<-glmer(Dead~1+(1|SL),Dead_I,family="binomial")
M_DBH<-glmer(Dead~DBH2_stand+(1|SL),Dead_I,family="binomial")
M_DBH2<-glmer(Dead~DBH2_stand+I(DBH2_stand^2)+(1|SL),Dead_I,family="binomial")
M_BA<-glmer(Dead~BA2_stand+(1|SL),Dead_I,family="binomial")
M_BA2<-glmer(Dead~BA2_stand+I(BA2_stand^2)+(1|SL),Dead_I,family="binomial")

AICc(M0,M_DBH,M_DBH2,M_BA,M_BA2)
plot(Dead_I$DBH2,plogis(predict(M_DBH,re.form=NA)))
points(Dead_I$DBH2,plogis(predict(M_DBH)),col="red")

#smaller or larger trees are more likley to die than intermediate trees


#candidates for spatial relationships with dead trees
ggplot(Dead_I,aes(x=Dead_dist,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_I,aes(x=Dead_No,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")
ggplot(Dead_I,aes(x=Dead_BA,y=Dead,colour=as.factor(SL)))+geom_point()+geom_smooth(method=glm,family="binomial")


#now spatial relationships with dead trees
M0<-glmer(Dead~1+(1|SL),Dead_I,family="binomial")
M_D_dist<-glmer(Dead~Dead_dist+(Dead_dist|SL),Dead_I,family="binomial")
M_D_no<-glmer(Dead~Dead_No+(Dead_No|SL),Dead_I,family="binomial")
M_D_BA<-glmer(Dead~Dead_BA+(Dead_BA|SL),Dead_I,family="binomial")
M_D_BA2<-glmer(Dead~Dead_BA+(1|SL),Dead_I,family="binomial")

AICc(M0,M_D_dist,M_D_no,M_D_BA,M_D_BA2)

plot(Dead_I$Dead_BA,plogis(predict(M_D_BA2,re.form=NA)))
points(Dead_I$Dead_BA,plogis(predict(M_D_BA2)),col="red")

#so distance to dead trees seems best

#now a model of DBH, growth, BA of dead trees
#come up with a standardised variable for DBH

M0.1<-glmer(Dead~1+(relGR|SL)+(Dead_dist|SL),Dead_I,family="binomial")

M1<-glmer(Dead~DBH2_stand+relGR+Dead_dist+(relGR|SL)+(Dead_dist|SL),Dead_I,family="binomial")
options(na.action = "na.fail")

#model averaging
models<-dredge(M1,trace = T)

MS<-model.sel(models)

Avs<-model.avg(MS,fit = T)

#create predictions for growth rate
GR<-expand.grid(relGR=seq(min(Dead_I$relGR),max(Dead_I$relGR),0.1),DBH2_stand=mean(Dead_I$DBH2)/max(Dead_I$DBH2),DBH2=mean(Dead_I$DBH2),Dead_dist=mean(Dead_I$Dead_dist))
GR$Preds<-predict(Avs,newdata = GR)
GR$SE<-predict(Avs,newdata = GR,se.fit = T)$se.fit
GR$Model<-"Growth rate"

plot(Dead_I$relGR,Dead_I$Dead)
points(GR$relGR,plogis(GR$Preds))

#create predictions for DBH
DBH<-expand.grid(DBH2=seq(min(Dead_I$DBH2),max(Dead_I$DBH2),0.01),relGR=mean(Dead_I$relGR),Dead_dist=mean(Dead_I$Dead_dist))
DBH$DBH2_stand<-DBH$DBH2/max(Dead_I$DBH2)
DBH$Preds<-predict(Avs,newdata = DBH)
DBH$SE<-predict(Avs,newdata = DBH,se.fit = T)$se.fit
DBH$Model<-"DBH"

plot(Dead_I$DBH2,Dead_I$Dead)
points(DBH$DBH2,plogis(DBH$Preds))


#create predictions for dead tree BA
DT<-expand.grid(relGR=mean(Dead_I$relGR),DBH2=mean(Dead_I$DBH2),DBH2_stand=mean(Dead_I$DBH2)/max(Dead_I$DBH2),Dead_dist=seq(min(Dead_I$Dead_dist),max(Dead_I$Dead_dist),1))
DT$Preds<-predict(Avs,newdata = DT)
DT$SE<-predict(Avs,newdata = DT,se.fit = T)$se.fit
DT$Model<-"Distance to dead tree"

plot(Dead_I$Dead_dist,Dead_I$Dead)
points(DT$Dead_dist,plogis(DT$Preds))

#save_predictions to a csv
Preds<-rbind(GR,DBH,DT)
head(Preds)
write.csv(Preds,"Data/Holy_mort_preds.csv",row.names=F)

