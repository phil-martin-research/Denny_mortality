Dead<-read.csv("Data/Dead_size.csv")

Dead<-subset(Dead,Year>1984)
Dead<-subset(Dead,GR>-10&GR<20&relBAGR<4)
Dead<-subset(Dead,!is.na(Dead))

head(Dead)

library(ggplot2)
library(GGally)
library(lme4)
library(lattice)
library(MuMIn)
ggpairs(Dead[, 7:13])
Dead_F<-subset(Dead,Species=="F")
Dead_Q<-subset(Dead,Species=="Q")

ggpairs(Dead_Q[, 7:13])

head(Dead_F)

#candidates for growth rate variables
ggplot(Dead_F,aes(x=GR,y=Dead))+geom_point()+geom_smooth(method=glm,family="binomial")+facet_wrap(~SL)
ggplot(Dead_F,aes(x=BAGR,y=Dead))+geom_point()+geom_smooth(method=glm,family="binomial")+facet_wrap(~SL)
ggplot(Dead_F,aes(x=relGR,y=Dead))+geom_point()+geom_smooth(method=glm,family="binomial")+facet_wrap(~SL)
ggplot(Dead_F,aes(x=relBAGR,y=Dead))+geom_point()+geom_smooth(method=glm,family="binomial")+facet_wrap(~SL)



#look at different models of mortality related to growth
#first a null model
glmerControl(tolPwrss=1e-3)
M0<-glmer(Dead~1+(1|ID2/SL),data=Dead_F,family=binomial)
M_GR<-glmer(Dead~GR+(1|ID2/SL),data=Dead_F,family=binomial)
M_BAGR<-glmer(Dead~BAGR+(1|ID2/SL),data=Dead_F,family=binomial)
M_relGR<-glmer(Dead~relGR+(1|ID2/SL),data=Dead_F,family=binomial)
M_relBAGR<-glmer(Dead~relBAGR+(1|ID2/SL),data=Dead_F,family=binomial)

AICc(M_GR,M_BAGR,M_relGR,M_relBAGR)
#seems like DBH growth rate is best

#candidates for size
ggplot(Dead_F,aes(x=DBH2,y=Dead))+geom_point()+geom_smooth(method=glm,family="binomial")+facet_wrap(~SL)
ggplot(Dead_F,aes(x=BA2,y=Dead))+geom_point()+geom_smooth(method=glm,family="binomial")+facet_wrap(~SL)
ggplot(Dead_F,aes(x=relSize,y=Dead))+geom_point()+geom_smooth(method=glm,family="binomial")+facet_wrap(~SL)

#look at different models of mortality related to size
#first a null model
glmerControl(tolPwrss=1e-3)
M0<-glmer(Dead~1+(1|ID2/SL),data=Dead_F,family=binomial)
M_DBH<-glmer(Dead~DBH+(1|ID2/SL),data=Dead_F,family=binomial)
M_DBH2<-glmer(Dead~DBH+I(DBH^2)+(1|ID2/SL),data=Dead_F,family=binomial)
M_BA<-glmer(Dead~BA+(1|ID2/SL),data=Dead_F,family=binomial)
M_BA2<-glmer(Dead~BA+I(BA^2)+(1|ID2/SL),data=Dead_F,family=binomial)
M_relSize<-glmer(Dead~relSize+(1|ID2/SL),data=Dead_F,family=binomial)

AICc(M_DBH,M_DBH2,M_BA,M_BA2,M_relSize)
#looks like BA is best for this

head(Dead_F)


#produce different potential models

str(Dead_F$SL)


M0<-lmer(logDeath~1+(1|ID2),data=Dead_F)
M1<-lmer(logDeath~DBH+(1|ID2),data=Dead_F)
M2<-lmer(logDeath~GR+(1|ID2),data=Dead_F)
M3<-lmer(logDeath~BA+GR+(1|ID2),data=Dead_F)
M4<-lmer(logDeath~BA+I(BA^2)+GR+(1|ID2),data=Dead_F)

plogis(predict(M4,re.form=NA))

library(car)

Dead_F$logDeath<-logit((Dead_F$Dead/Dead_F$SL))

summary(M3)

dotplot(ranef(M3,condVar=TRUE),
        lattice.options=list(layout=c(1,2)))

summary(M3)


Dead_1988<-subset(Dead_F,Year==1988)


GLM1<-glm(Dead~BA2+GR,data=Dead_1988,family=binomial)
GLM2<-glm(Dead~BA2+I(BA2^2)+GR,data=Dead_1988,family=binomial)
GLM3<-glm(Dead~BA2+I(BA2^2),data=Dead_1988,family=binomial)
GLM4<-glm(Dead~BA2,data=Dead_1988,family=binomial)

summary(GLM1)
summary(GLM2)
summary(GLM3)
summary(GLM4)


AICc(GLM1,GLM2,GLM3,GLM4)

plot(Dead_1988$DBH2,plogis(predict(GLM4)))
