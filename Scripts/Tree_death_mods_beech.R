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

#import data
rm(list=ls(all=TRUE))
Dead<-read.csv("Data/Dead_size.csv")

#subset to remove data prior to 1984
Dead<-subset(Dead,Year>1984)
#filter out trees that have unrealistic growth rates
Dead<-subset(Dead,!is.na(Dead))
Dead<-subset(Dead,GR>-20&GR<20&relBAGR<4)
par(mfrow=c(1,1))
hist(Dead$GR)
hist(Dead$relBAGR)



#subset data into individual species
Dead_F<-subset(Dead,Species=="F")
Dead_F<-subset(Dead_F,GR>-20&GR<20&relBAGR<4)
keeps<-c("Dead","Easting","Northing","SL","GR","BAGR","relGR","relBAGR","DBH2","BA2","relSize","Dead_dist","Dead_No")
Dead_F<-Dead_F[keeps]
str(Dead_F)

#standardise variables following Zuur et al recommendations
Dead_F_st<-cbind(Dead_F[,1:4],apply(X=Dead_F[,5:ncol(Dead_F)],MARGIN=2,FUN=function(x) {(x-mean(x))/sd(x)}))

#candidates for growth rate variables
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
M0<-glmer(Dead~1+(1|SL),Dead_F_st,family="binomial")
M_DBH<-glmer(Dead~DBH2+(DBH2|SL),Dead_F_st,family="binomial")
M_DBH2<-glmer(Dead~DBH2+I(DBH2^2)+(DBH2|SL),Dead_F_st,family="binomial")
M_BA<-glmer(Dead~BA2+(BA2|SL),Dead_F_st,family="binomial")
M_BA2<-glmer(Dead~BA2+I(BA2^2)+(BA2|SL),Dead_F_st,family="binomial")

AICc(M0,M_DBH,M_DBH2,M_BA,M_BA2)
plot(Dead_F_st$DBH2,plogis(predict(M_DBH2,re.form=NA)))
points(Dead_F_st$DBH2,plogis(predict(M_DBH2)),col="red")

#bigger trees are more likley to die - DBH is the best predictor of this

#candidates for spatial relationships with dead trees
M0<-glmer(Dead~1+(1|SL),Dead_F_st,family="binomial")
M_D_dist<-glmer(Dead~Dead_dist+(Dead_dist|SL),Dead_F_st,family="binomial")
M_D_no<-glmer(Dead~Dead_No+(Dead_No|SL),Dead_F_st,family="binomial")

AICc(M0,M_D_dist,M_D_no)

plot(Dead_F_st$Dead_dist,plogis(predict(M_D_dist,re.form=NA)))
points(Dead_F_st$Dead_dist,plogis(predict(M_D_dist)),col="red")

#so BA of dead trees seems best
#remove variables that I'm not going to use in the analyses
Dead_F_st$DBH2_sq<-Dead_F_st$DBH2^2
keeps<-c("Dead","DBH2","DBH2_sq","GR","Dead_dist","SL")
Dead_F_st2<-Dead_F_st[keeps]


ggpairs(Dead_F_st2[,2:5])

#now a model of DBH, growth, distance to dead trees, number of live trees
M0.1<-glmer(Dead~1+(DBH2|SL)+(GR|SL)+(Dead_dist|SL),Dead_F_st2,family="binomial")
M0.2<-glmer(Dead~1+(DBH2|SL)+(GR|SL),Dead_F_st2,family="binomial")
M0.3<-glmer(Dead~1+(DBH2|SL),Dead_F_st2,family="binomial")
M0.4<-glmer(Dead~1+(GR|SL),Dead_F_st2,family="binomial")
AICc(M0.1,M0.2,M0.3,M0.4)


#now put in fixed effects
Dead_F_st2$SL2<-as.factor(Dead_F_st2$SL)
M1<-glmer(Dead~GR+DBH2_sq+DBH2+Dead_dist+(GR|SL2),data=Dead_F_st2,family="binomial")

summary(M1)
dotplot(ranef(M1,condVar=TRUE),
        lattice.options=list(layout=c(1,2)))

#now do model averaging
options(na.action = "na.fail")
models<-dredge(M1,trace = T,subset=dc(DBH2,DBH2_sq))

#produce model selection table
MS<-model.sel(models)

importance(MS)

Avs<-model.avg(MS,fit = T,subset =cumsum(weight) <= 0.95)
summary(Avs)
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



#produce le Cessie-van Houwelingen normal test statistic for these models 
HLgof.test(fit = fitted(M1_sel), obs=Dead_F_st2$Dead,X=model.matrix(Dead ~ DBH2+GR,data=Dead_F_st2))


Preds<-predict(Avs)

#look at partial residuals for this
DBH_res<-(Dead_F_st2$Dead-plogis(Preds))/
          ((plogis(Preds))*(1-plogis(Preds)))+
          Dead_F_st2$DBH2*coef(Avs)[2]







