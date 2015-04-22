#script to predict density of saplings (<10cm DBH) of
#beech trees based on:
#1. Tree density/BA and browsing pressure

#import data
rm(list=ls(all=TRUE))
Saplings<-read.csv("Data/Denny_plots.csv")
Saplings$Year<-ifelse(Saplings$Year==1999,1996,Saplings$Year)

#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)

#explore data
head(Saplings)
ggplot(Saplings,aes(x=Easting,y=Northing,colour=SDS))+geom_point(shape=15)+facet_wrap(~Year)+scale_colour_gradient(low ="grey",high="red")
ggplot(Saplings,aes(x=Easting,y=Northing,colour=FS))+geom_point(shape=15)+facet_wrap(~Year)+scale_colour_gradient(low ="grey",high="red")
ggplot(Saplings,aes(x=Easting,y=Northing,colour=QS))+geom_point(shape=15)+facet_wrap(~Year)+scale_colour_gradient(low ="grey",high="red")
ggplot(Saplings,aes(x=Easting,y=Northing,colour=IS))+geom_point(shape=15)+facet_wrap(~Year)+scale_colour_gradient(low ="grey",high="red")


#plot against possible predictors, SD of mature trees, BA of mature trees
#first SD
ggplot(Saplings,aes(x=SDM,y=SDS,colour=as.factor(Year)))+geom_point(shape=15)+geom_smooth(method="glm",family="poisson")
ggplot(Saplings,aes(x=IM,y=IS,colour=as.factor(Year)))+geom_point(shape=15)+geom_smooth(method="glm",family="poisson")
ggplot(Saplings,aes(x=FM,y=FS,colour=as.factor(Year)))+geom_point(shape=15)+geom_smooth(method="glm",family="poisson")
ggplot(Saplings,aes(x=QM,y=QS,colour=as.factor(Year)))+geom_point(shape=15)+geom_smooth(method="glm",family="poisson")
#then BD
ggplot(Saplings,aes(x=BAM,y=SDS,colour=as.factor(Year)))+geom_point(shape=15)+geom_smooth(method="glm",family="poisson")
ggplot(Saplings,aes(x=BAQM,y=FS,colour=as.factor(Year)))+geom_point(shape=15)+geom_smooth(method="glm",family="poisson")
ggplot(Saplings,aes(x=BAFM,y=FS,colour=as.factor(Year)))+geom_point(shape=15)+geom_smooth(method="glm",family="poisson")

#look at change over time of saplings
ggplot(Saplings,aes(x=Year,y=SDS,group=as.factor(Block)))+geom_point(shape=15)+geom_line()+geom_smooth(method="glm",family="poisson",aes(group=NULL),se=F,size=3)
ggplot(Saplings,aes(x=Year,y=IS,group=as.factor(Block)))+geom_point(shape=15)+geom_line()+geom_smooth(method="glm",family="poisson",aes(group=NULL),se=F,size=3)
ggplot(Saplings,aes(x=Year,y=QS,group=as.factor(Block)))+geom_point(shape=15)+geom_line()+geom_smooth(method="glm",family="poisson",aes(group=NULL),se=F,size=3)
ggplot(Saplings,aes(x=Year,y=FS,group=as.factor(Block)))+geom_point(shape=15)+geom_line()+geom_smooth(method="glm",family="poisson",aes(group=NULL),se=F,size=3)

#model of sapling density for beech over time
Saplings$Year2<-Saplings$Year-1964
#null model
MTime0<-glmer(FS~1+(1|Block),family="poisson",data=Saplings)
#now by time
MTime1<-glmer(FS~Year2+(Block|Year2),family="poisson",data=Saplings)
plot(MTime1)
AICc(MTime0,MTime1)
r.squaredGLMM(MTime1)

#now model sapling density for beech as a function of adult tree density
MDens0<-glmer(FS~1+(Block|Year),family="poisson",data=Saplings)
MDens0.1<-glmer(FS~1+(1|Block),family="poisson",data=Saplings)
AICc(MDens0,MDens0.1)
#now by time
MDens1<-glmer(FS~FM*Year+(Block|Year),family="poisson",data=Saplings)
MDens2<-glmer(FS~FM+Year+(Block|Year),family="poisson",data=Saplings)
MDens3<-glmer(FS~FM+(Block|Year),family="poisson",data=Saplings)
AICc(MDens1,MDens2,MDens3,MDens0)

#produce a model list and selection table
Mod_list<-list(MDens0,MDens1,MDens2,MDens3)
Model_sel<-model.sel(Mod_list)
Model_sel$R2<-c(r.squaredGLMM(MDens1)[1],r.squaredGLMM(MDens2)[1],r.squaredGLMM(MDens3)[1],r.squaredGLMM(MDens0)[1])
write.csv(Model_sel,"Tables/Sapling_model_sel.csv",row.names=F)

#also produce a coefficient table for sapling model
Model_coefs<-coef(summary(MDens1))
write.csv(Model_coefs,"Tables/Sapling_model_coefs.csv",row.names=F)


#now create plots of this
newdat<-rbind(data.frame(FM=seq(1,max(Saplings$FM),1),Year=1964),
              data.frame(FM=seq(0,18,1),Year=1984),
              data.frame(FM=seq(0,17,1),Year=1988),
              data.frame(FM=seq(0,17,1),Year=1996),
              data.frame(FM=seq(0,15,1),Year=2014))
newdat$FS<-0
mm <- model.matrix(terms(MDens1),newdat)
newdat$FS <- predict(MDens1,newdat,re.form=NA)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(MDens1),mm))
tvar1 <- pvar1+VarCorr(MDens1)$Year[1]  ## must be adapted for more complex models
tvar1 <- 
  newdat <- data.frame(
    newdat
    , plo = newdat$FS-2*sqrt(pvar1)
    , phi = newdat$FS+2*sqrt(pvar1)
    , tlo = newdat$FS-2*sqrt(tvar1)
    , thi = newdat$FS+2*sqrt(tvar1)
  )

#now plot this
Saplings2<-ddply(Saplings,.(FM,FS,Year),summarise,number=length(FM))
head(Saplings2)
theme_set(theme_bw(base_size=12))
SD_plot1<-ggplot(newdat,aes(x=FM,y=exp(FS),ymin=exp(plo),ymax=exp(phi),colour=as.factor(Year),group=as.factor(Year),fill=as.factor(Year)))+geom_ribbon(alpha=0.2,colour=NA)+geom_ribbon(aes(ymax=exp(thi),ymin=exp(tlo)),alpha=0.2,colour=NA)+geom_line()+geom_point(data=Saplings2,aes(x=FM,y=FS,size=number,ymin=NULL,ymax=NULL),shape=1)+facet_wrap(~Year)
SD_plot2<-SD_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")                                                                                                                                            
SD_plot2+ylab("Density of saplings per 0.04ha plot")+xlab("Density of trees >10cm DBH per 0.04ha plot")
ggsave("Figures/Beech_saplings.png",height=6,width=8,dpi=600,units="in")

