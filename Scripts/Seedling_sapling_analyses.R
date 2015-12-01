#script to look at seedling density related to canopy cover in Paul and Denny plots

#get rid of all objects
rm(list=ls(all=TRUE))

#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(plotrix)

#load in data
Seedlings<-read.csv("Data/Phil_Data_Seedlings_Saplings.csv")
#standardise the canopy openness values
Seedlings$Can_Std<-(Seedlings$Canopy.Openness.Total-mean(Seedlings$Canopy.Openness.Total))/sd(Seedlings$Canopy.Openness.Total)


#models of seedling abundance over gradient of canopy openness
Seed0<-glmer(FagusSeedlings~1+(1|Site),family="poisson",data=Seedlings)
Seed1<-glmer(FagusSeedlings~Can_Std+(1|Site),family="poisson",data=Seedlings)
Seed_model_sel<-model.sel(Seed0,Seed1)
Seed_model_sel$R2<-c(r.squaredGLMM(Seed1)[1],r.squaredGLMM(Seed0)[1])
Seed_avg<-model.avg(Seed_model_sel,fit = T)
Seed_coefs<-summary(model.avg(Seed0,Seed1))$coefmat.subset
Seedlings$seed_pred<-exp(predict(Seed_avg,re.form=NA))


#models of sapling abundance over gradient of canopy openness
Sap0<-glmer(FagusSaplings~1+(1|Site),family="poisson",data=Seedlings)
Sap1<-glmer(FagusSaplings~Can_Std+(1|Site),family="poisson",data=Seedlings)
Sap_model_sel<-model.sel(Sap0,Sap1)
Sap_model_sel$R2<-c(r.squaredGLMM(Sap1)[1],r.squaredGLMM(Sap0)[1])
Sap_avg<-model.avg(Sap_model_sel,fit = T)
Sap_coefs<-summary(model.avg(Sap0,Sap1))$coefmat.subset
Seedlings$sap_pred<-exp(predict(Sap_avg,re.form=NA))

############################################################
#Do the same using Denny data###############################
############################################################

#load data
Seed_Denny<-read.csv("Data/Seedlings_denny.csv",stringsAsFactors=F)
Plots<-read.csv("Data/Denny_plots_trees_M.csv")
Browsing<-read.csv("Data/Browsing_Denny.csv",,stringsAsFactors=F)

#tidy seedlings data
Seed_Denny$Plot<-sapply(strsplit(Seed_Denny$Plot2,"-"),'[',2)
drop<-"Plot2"
Seed_Denny<-Seed_Denny[,!(names(Seed_Denny) %in% drop)]
Seed_Denny2<-subset(Seed_Denny,Species=="Fagus sylvatica"|Species=="Quercus spp"|Species=="Ilex aquifolium")
Seed_melt<-melt(Seed_Denny2,id.vars = c("Plot","Count"))
Seed_melt2<-ddply(Seed_melt,.(Plot,value),summarize,Count=sum(Count))
Seed_melt3<-spread(Seed_melt2, value, Count)
Seed_melt3[is.na(Seed_melt3)] <- 0
colnames(Seed_melt3)<-c("Plot","Beech","Holly","Oak")

#tidy browsing data
Browsing$Plot2<-sapply(strsplit(Browsing$Plot,"-"),'[',2)
drop<-"Plot"
Browsing<-Browsing[,!(names(Browsing) %in% drop)]
colnames(Browsing)[7]<-"Plot"
Browsing<-Browsing[complete.cases(Browsing),]

#merge browsing data and seedling data
Seed_browsing<-merge(Browsing,Seed_melt3,by="Plot")


#before analysis produce mean +/- SE for each species
#and put in a table for later
Seedling_density<-data.frame(Species=c("Beech","Oak","Holly"),
                             Mean_seedling_density=c(mean(Seed_browsing$Beech*100),mean(Seed_browsing$Oak*100),mean(Seed_browsing$Holly*100)),
                             SE=c(std.error(Seed_browsing$Beech*100),std.error(Seed_browsing$Oak*100),std.error(Seed_browsing$Holly*100)))
write.csv(Seedling_density,"Tables/Seedling_density.csv",row.names=F)

#explore the relationships between seedling density and
#browsing pressure/canopy openness
#explore in more detail
#canopy relationship with beech Seed_Denny
ggplot(Seed_browsing,aes(x=Canopy_open,y=Beech))+geom_point()+geom_smooth(method="glm",family="poisson")+xlim(0,40)
#canopy openness and holly
ggplot(Seed_browsing,aes(x=Canopy_open,y=Holly))+geom_point()+geom_smooth(method="glm",family="poisson")+xlim(0,40)
#canopy openness and Oak
ggplot(Seed_browsing,aes(x=Canopy_open,y=Oak))+geom_point()+geom_smooth(method="glm",family="poisson")+xlim(0,40)


#there is little evidence that there is a relationship between browsing pressure in denny and the density of beech
#seedlings - this could possibly be becuase browsing pressure is high everywhere (?!)
#in addition there is no relationship between canopy openness and browsing pressure

Seed_browsing2<-data.frame(Canopy=Seed_browsing$Canopy,Canopy_std=(Seed_browsing$Canopy_open-mean(Seed_browsing$Canopy_open))/sd(Seed_browsing$Canopy_open),Seed_browsing[,8:10])
head(Seed_browsing2)

#now test to see if there is a relationship between canopy openness and beech seedling density
M0<-glm(Beech~1,data=Seed_browsing2,family="poisson")
M1<-glm(Beech~Canopy_std,data=Seed_browsing2,family="poisson")
Seed_sel2<-model.sel(M0,M1)
Seed_sel2$R2<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M0)[1])
Seed_coefs2<-summary(model.avg(Seed_sel2))$coefmat.subset
Seed_avg2<-model.avg(Seed_sel2,fit = T)
df<-data.frame(Canopy=seq(min(Seed_browsing2$Canopy),max(Seed_browsing2$Canopy),0.1))
df$Canopy_std<-(df$Canopy-mean(Seed_browsing$Canopy_open))/sd(Seed_browsing$Canopy_open)
df$Seed_pred<-predict(Seed_avg2,newdata = df, backtransform = T)

#now do the same for saplings
Saplings<-read.csv("Data/Denny_plots.csv",stringsAsFactors=F)
Browsing<-read.csv("Data/Browsing_Denny.csv",stringsAsFactors=F)

#tidy saplings data
Saplings<-subset(Saplings,Year==2014)
Saplings2<-ddply(Saplings,.(Block),summarize,Count=sum(FS))

#tidy browsing data
Browsing$Plot2<-sapply(strsplit(Browsing$Plot,"-"),'[',2)
drop<-"Plot"
Browsing<-Browsing[,!(names(Browsing) %in% drop)]
colnames(Browsing)[7]<-"Plot"
Browsing<-Browsing[complete.cases(Browsing),]

#merge browsing data and seedling data
Sapling_browsing<-merge(Browsing,Saplings2,by.x="Plot",by.y="Block")
str(Sapling_browsing)

#standardise canopy openness
Sapling_browsing$Canopy_Std<-(Sapling_browsing$Canopy_open-mean(Sapling_browsing$Canopy_open))/sd(Sapling_browsing$Canopy_open)

#now test a model relating canopy openness to sapling density
M0<-glm(Count~1,family="poisson",data=Sapling_browsing)
M1<-glm(Count~Canopy_Std,family="poisson",data=Sapling_browsing)
Sap_sel2<-model.sel(M0,M1)
Sap_sel2$R2<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M0)[1])
Sap_coefs2<-summary(model.avg(Sap_sel2))$coefmat.subset
Sap_avg2<-model.avg(Sap_sel2,fit = T)
Sapling_browsing$Sap_pred<-predict(Sap_avg2,backtransform = T)


#now produce plots for these models

#set up labelling function so that labels have no decimal places
fmt<-function(){
  f<-function(x) as.character(round(x,0))
  f
}

#first seedlings from gradient plots
theme_set(theme_bw(base_size=12))
Seed_grad_P1<-ggplot(Seedlings, aes(x=Canopy.Openness.Total,y=FagusSeedlings))+geom_point(shape=1)+geom_line(aes(group=NULL,y=seed_pred),colour="black")
Seed_grad_P2<-Seed_grad_P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Seed_grad_P3<-Seed_grad_P2+ylab(expression(paste("Beech seedling density ", subplot^-1,sep=" ")))+xlab("")+ annotate("text", x = 0, y = 70, label = "(a)")+theme(plot.margin=unit(c(0,0,0,0), "cm"))

#now saplings from gradient plots
Sap_grad_P1<-ggplot(Seedlings, aes(x=Canopy.Openness.Total,y=FagusSaplings))+geom_point(shape=1)
Sap_grad_P2<-Sap_grad_P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Sap_grad_P3<-Sap_grad_P2+ylab(expression(paste("Beech sapling density ", subplot^-1,sep=" ")))+xlab("")+ annotate("text", x = 0, y = 6, label = "(b)")+theme(plot.margin=unit(c(0,0,0,0), "cm"))

#now seedlings from Denny
Seed_D_P1<-ggplot(Seed_browsing2, aes(x=Canopy,y=Beech))+geom_point(shape=1)+geom_line(data=df,aes(group=NULL,y=Seed_pred),colour="black")
Seed_D_P2<-Seed_D_P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Seed_D_P3<-Seed_D_P2+ylab(expression(paste("Beech seedling density ", subplot^-1,sep=" ")))+xlab("")+ annotate("text", x = 0, y = 15, label = "(c)")+theme(plot.margin=unit(c(0,0,0,0), "cm"))+scale_y_continuous(breaks=c(0,5,10,15))

#now saplings from Denny
Sap_D_P1<-ggplot(Sapling_browsing, aes(x=Canopy_open,y=Count))+geom_point(shape=1)
Sap_D_P2<-Sap_D_P1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Sap_D_P3<-Sap_D_P2+ylab(expression(paste("Beech sapling density ", subplot^-1,sep=" ")))+xlab("")+ annotate("text", x = 0, y = 3, label = "(d)")+theme(plot.margin=unit(c(0,0,0,0), "cm"))




png("Figures/Seedling_sapling.png",height=6,width=8,res = 800,units = "in")
grid.arrange(arrangeGrob(Seed_grad_P3,
                         Sap_grad_P3,
                         Seed_D_P3,
                         Sap_D_P3,
                         ncol=2,
                         bottom= textGrob("Canopy openness (%)", vjust = -1) 
))
dev.off()
