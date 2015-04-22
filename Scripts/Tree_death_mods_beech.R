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
Dead_F<-subset(Dead)
keeps<-c("ID2","Dead","Easting","Northing","SL","Species","GR","BAGR","relGR","relBAGR","DBH2","BA2","relSize","Dead_dist","Dead_No")
Dead_F<-Dead_F[keeps]
str(Dead_F)


#standardise variables following Zuur et al recommendations
Dead_F_st<-cbind(Dead_F[,1:6],apply(X=Dead_F[,7:ncol(Dead_F)],MARGIN=2,FUN=function(x) {(x-mean(x))/sd(x)}))
head(Dead_F_st)

#candidates for growth rate variables
#now build glmms of these
M0<-glm(Dead~1,Dead_F_st,family="binomial")
M1<-glm(Dead~DBH2,Dead_F_st,family="binomial")
M2<-glmer(I((1+Dead_F_st$Dead)^(-Dead_F$SL))~DBH2+I(DBH2^2)+(1|ID2),Dead_F_st,family=binomial(link=logit))

plot(Dead_F_st$Dead,((1+Dead_F_st$Dead)^(2)))


qplot(Dead_F$DBH2,predict(M2,re.form=NA),colour=as.factor(Dead_F$SL))

Dead_F_st2<-data.frame(Dead_F_st,resid=resid(M0))
head(Dead_F_st2)
qplot(data=Dead_F_st2,x=Easting,y=Northing,size=abs(resid),colour=resid,alpha=0.3)+facet_wrap(~SL)+scale_colour_gradient2()

M_GR<-glmer(Dead~GR*SL+(1|SL),Dead_F_st,family="binomial")
M_BAGR<-glmer(Dead~BAGR*SL+(1|SL),Dead_F_st,family="binomial")
M_relGR<-glmer(Dead~relGR*SL+(1|SL),Dead_F_st,family="binomial")
M_relBAGR<-glmer(Dead~relBAGR*SL+(1|SL),Dead_F_st,family="binomial")

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
plot(Dead_F_st$BA2,plogis(predict(M_BA2,re.form=NA)))
points(Dead_F_st$BA2,plogis(predict(M_BA2)),col="red")

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
Dead_F_st$BA2_sq<-Dead_F_st$BA2^2
head(Dead_F_st)
keeps<-c("Dead","BA2","BA2_sq","GR","Dead_dist","SL")
Dead_F_st2<-Dead_F_st[keeps]


ggpairs(Dead_F_st2[,2:5])

#now a model of DBH, growth, distance to dead trees, number of live trees
M0.1<-glmer(Dead~1+(BA2|SL)+(GR|SL)+(Dead_dist|SL),Dead_F_st2,family="binomial")
M0.2<-glmer(Dead~1+(BA2|SL)+(GR|SL),Dead_F_st2,family="binomial")
M0.3<-glmer(Dead~1+(BA2|SL),Dead_F_st2,family="binomial")
M0.4<-glmer(Dead~1+(GR|SL),Dead_F_st2,family="binomial")
AICc(M0.1,M0.2,M0.3,M0.4)


#now put in fixed effects
Dead_F_st$SL2<-as.factor(Dead_F_st$SL)

M1.1<-glmer(Dead~GR*Species+BA2_sq*Species+BA2*Species+Dead_dist*Species+(GR|SL2)+(BA2|SL2),data=Dead_F_st,family="binomial")
M1.2<-glmer(Dead~GR+BA2_sq+BA2+Dead_dist+(BA2|SL2),data=Dead_F_st2,family="binomial")
M1.3<-glmer(Dead~GR+BA2_sq+BA2+Dead_dist+(GR|SL2),data=Dead_F_st2,family="binomial")
M1.4<-glmer(Dead~GR+BA2_sq+BA2+Dead_dist+(1|SL2),data=Dead_F_st2,family="binomial")
summary(M1.1)

AICc(M1.1,M1.2,M1.3,M1.4)

dotplot(ranef(M1,condVar=TRUE),
        lattice.options=list(layout=c(1,2)))

#now do model averaging
options(na.action = "na.fail")
models<-dredge(M1.1,trace = T,subset=dc(BA2,BA2_sq))

#produce model selection table
MS<-model.sel(models)

importance(MS)

Avs<-model.avg(MS,fit = T,subset =cumsum(weight) <= 0.95)
summary(Avs)
importance(Avs)

#produce predictions from the model averaged coefficients
summary(Dead_F_st2)

ddply(Dead_F_st,.(Species),summarise,max_BA=max(BA2),min_ba=min(BA2))
new.data.BA<-rbind(
  data.frame(BA2=seq(-0.5849468,6.8419631,0.01),GR=mean(Dead_F_st2$GR),Dead_dist=mean(Dead_F_st2$Dead_dist),Species="F"),
  data.frame(BA2=seq(-0.4493203,5.7353880,0.01),GR=mean(Dead_F_st2$GR),Dead_dist=mean(Dead_F_st2$Dead_dist),Species="Q"),
  data.frame(BA2=seq(-0.5863576,0.2103882,0.01),GR=mean(Dead_F_st2$GR),Dead_dist=mean(Dead_F_st2$Dead_dist),Species="I")
)
  new.data.BA$BA2_sq<-new.data.BA$BA2^2
new.data.BA$Dead<-plogis(predict(Avs,newdata =new.data.BA,re.form=NA))

#now produce binned observations
keeps<-c("Species","Dead","BA2")
Dead_obs<-Dead_F[keeps]


ddply(Dead_obs, .(Species), function(x) quantile(x$BA2,probs=seq(0,1,0.1)))

Dead_obs$BAbin<- cut(Dead_obs$BA2,seq(0,max(Dead_obs$BA2),0.01))
Mort_bin <- ddply(Dead_obs,.(Species,BAbin), function(DF) {
  data.frame(mean=numcolwise(mean)(DF), length=numcolwise(length)(DF))
})


ddply(Dead_F,.(Species,BA))
ggplot(new.data.BA,aes(x=(BA2*sd(Dead_F$BA2))+mean(Dead_F$BA2),y=Dead))+geom_line()+facet_wrap(~Species,scales="free_x")






