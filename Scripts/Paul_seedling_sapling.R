#script to look at seedling density related to canopy in Paul's plots

library(ggplot2)
library(lme4)
library(MuMIn)

#load in data
Seedlings<-read.csv("Data/Phil_Data_Seedlings_Saplings.csv")

#models of seedling abundance over gradient of canopy openness
Seed0<-glmer(FagusSeedlings~1+(1|Site),family="poisson",data=Seedlings)
Seed1<-glmer(FagusSeedlings~Canopy.Openness.Total+(1|Site),family="poisson",data=Seedlings)

plot(Seedlings$Canopy.Openness.Total,exp(predict(Seed1,re.form=NA)))

#models of sapling abundance over gradient of canopy openness
Sap0<-glmer(FagusSaplings~1+(1|Site),family="poisson",data=Seedlings)
Sap1<-glmer(FagusSaplings~Canopy.Openness.Total+(1|Site),family="poisson",data=Seedlings)


plot(Seedlings$Canopy.Openness.Total,exp(predict(Sap1,re.form=NA)))


ggplot(Seedlings, aes(x=Canopy.Openness.Total,y=FagusSeedlings,group=Site,colour=Site))+geom_point()+geom_smooth(se=F,method="glm",family="poisson",aes(group=NULL),colour="black")
ggplot(Seedlings, aes(x=Canopy.Openness.Total,y=FagusSaplings,group=Site,colour=Site))+geom_point()+geom_smooth(se=F,method="lm",family="poisson",aes(group=NULL),colour="black")
