#seedling analysis
rm(list=ls(all=TRUE))
Seedlings<-read.csv("Data/Seedlings_denny.csv",header = T,stringsAsFactors=F)
Trees<-read.csv("Data/Denny_plots_trees_M.csv",header = T,stringsAsFactors=F)
Trees<-subset(Trees,Year==2014)

#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)
library(reshape2)
library(tidyr)
library(dplyr)


#tidy data
Seedlings$Plot<-sapply(strsplit(Seedlings$Plot2,"-"),'[',2)
drop<-"Plot2"
Seedlings<-Seedlings[,!(names(Seedlings) %in% drop)]
head(Seedlings)
Seedlings2<-subset(Seedlings,Species=="Fagus sylvatica"|Species=="Quercus spp"|Species=="Ilex aquifolium")
Seed_melt<-melt(Seedlings2,id.vars = c("Plot","Count"))
Seed_melt2<-ddply(Seed_melt,.(Plot,value),summarize,Count=sum(Count)*100)
Seed_melt3<-spread(Seed_melt2, value, Count)
Seed_melt3[is.na(Seed_melt3)] <- 0
colnames(Seed_melt3)<-c("Block","Beech","Holly","Oak")
Seed_melt4<-merge(Seed_melt3,Trees,by="Block",all=T)
Seed_melt4<-subset(Seed_melt4,!is.na(Year))
Seed_melt4<-subset(Seed_melt4,!is.na(Beech))

#look at relationships between tree density >10cm and seedling density
#beech
ggplot(Seed_melt4,aes(x=F_BA,y=Beech))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)
ggplot(Seed_melt4,aes(x=FM,y=Beech))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)
ggplot(Seed_melt4,aes(x=SDM,y=Beech))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)


#holly
ggplot(Seed_melt4,aes(x=I_BA,y=Holly))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)
ggplot(Seed_melt4,aes(x=IM,y=Holly))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)
ggplot(Seed_melt4,aes(x=SDM,y=Holly))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)

#oak
ggplot(Seed_melt4,aes(x=Q_BA,y=Oak))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)
ggplot(Seed_melt4,aes(x=QM,y=Oak))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)
ggplot(Seed_melt4,aes(x=SDM,y=Oak))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)

#function to work out quasi-AIC values since the data is overdispersed
#and we cannot use AICc values with quasi-poisson models

x.quasipoisson <- function(...) {
  res <- quasipoisson(...)
  res$aic <- poisson(...)$aic
  res
}

dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}

#now do statistical analysis of this relationship for beech
par(mfrow=c(2,2))
M1_F<-glm(Beech~F_BA+I(F_BA^2)+FM+I(FM^2),family="x.quasipoisson",data=Seed_melt4,na.action=na.fail)
plot(M1_F)

Seed_F_Sel<-dredge(M1_F,rank="QAICc",chat=dfun(M1_F),subset=!(F_BA && FM)&dc(F_BA,I(F_BA^2))&dc(FM,I(FM^2)))
Model_av_seed<-model.avg(Seed_F_Sel,subset = delta<7)
Coef_seedlings<-summary(Model_av_seed)$coefmat.full
importance(Model_av_seed)

write.csv(Seed_F_Sel,"Tables/Seedling_dens_sel.csv",row.names=F)
write.csv(Coef_seedlings,"Tables/Seedling_dens_coefs.csv")


