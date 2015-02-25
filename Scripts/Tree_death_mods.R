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
library(ResourceSelection)

ggpairs(Dead[, 7:13])
Dead_F<-subset(Dead,Species=="F")
Dead_Q<-subset(Dead,Species=="Q")
Dead_I<-subset(Dead,Species=="I")

summary(Dead)

ggpairs(Dead_Q[, 7:13])

head(Dead_F)

#candidates for growth rate variables
ggplot(Dead,aes(x=GR,y=Dead))+geom_point()+geom_smooth(method=glm,family="binomial")+facet_grid(Species~Year)
ggplot(Dead,aes(x=BAGR,y=Dead))+geom_point()+geom_smooth(method=glm,family="binomial")+facet_grid(Species~Year)
ggplot(Dead,aes(x=relGR,y=Dead))+geom_point()+geom_smooth(method=glm,family="binomial")+facet_grid(Species~Year)
ggplot(Dead,aes(x=relBAGR,y=Dead))+geom_point()+geom_smooth(method=glm,family="binomial")+facet_grid(Species~Year)

#first look at death rate for beech for 1988, 1996, 2014
Dead_F_88<-subset(Dead_F,Year==1988)
Dead_F_96<-subset(Dead_F,Year==1996)
Dead_F_14<-subset(Dead_F,Year==2014)

#now build glms of these
M0<-glm(Dead~1,Dead_F_88,family="binomial")
M_GR<-glm(Dead~GR,Dead_F_88,family="binomial")
M_BAGR<-glm(Dead~BAGR,Dead_F_88,family="binomial")
M_relGR<-glm(Dead~relGR,Dead_F_88,family="binomial")
M_relBAGR<-glm(Dead~relBAGR,Dead_F_88,family="binomial")

AICc(M0,M_GR,M_BAGR,M_relGR,M_relBAGR)

plot(Dead_F_88$relGR,plogis(predict(M_relGR)))
#relative growth rate looks best

#now size
M0<-glm(Dead~1,Dead_F_88,family="binomial")
M_DBH<-glm(Dead~DBH2,Dead_F_88,family="binomial")
M_DBH2<-glm(Dead~DBH2+I(DBH2^2),Dead_F_88,family="binomial")
M_BA<-glm(Dead~BA2,Dead_F_88,family="binomial")

AICc(M0,M_DBH,M_DBH2,M_BA)
plot(Dead_F_88$BA2,plogis(predict(M_BA)))

#now a model of both BA and relate growth rate
M1<-glm(Dead~BA2,Dead_F_88,family="binomial")
M2<-glm(Dead~relGR,Dead_F_88,family="binomial")
M3<-glm(Dead~BA2+relGR,Dead_F_88,family="binomial")
M3<-glm(Dead~BA2+relGR,Dead_F_88,family="binomial")
M4<-glm(Dead~BA2+I(BA^2),Dead_F_88,family="binomial")


hl <- hoslem.test(Dead_F_88$Dead, fitted(M1), g=10)
hl






#create some predictions
predict(M_BA,se.fit = T)$


limits<-as.vector(quantile(Dead_F_88$DBH2, prob = seq(0, 1, length = 11), type = 5))


Dead_F_88$BA_class<-NA
Dead_F_882<-NULL
for (i in 2:length(limits)){
  Dead_sub<-subset(Dead_F_88,BA2>=limits[i-1]&BA2<limits[i])
  Dead_sub$BA_class<-limits[i-1]
  Dead_F_882<-rbind(Dead_sub,Dead_F_882)
}

head(Dead_F_882)


BA_class<-ddply(Dead_F_882,.(BA_class),summarize,Death=mean(Dead))
BA_class$BA2<-BA_class$BA_class

BA_class$Pred<-plogis(predict(M1,newdata=BA_class))

plot(M1)

ggplot(BA_class,aes(x=as.factor(BA_class),y=Death))+geom_point()+geom_point(aes(y=Pred),colour="red")


