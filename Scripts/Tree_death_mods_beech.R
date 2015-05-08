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
library(plyr)
library(MKmisc)

#import data
rm(list=ls(all=TRUE))
Dead<-read.csv("Data/Dead_size.csv")
head(Dead)

#subset to remove data prior to 1984
Dead<-subset(Dead,Year>1984)
#filter out trees that have unrealistic growth rates
Dead<-subset(Dead,!is.na(Dead))
Dead<-subset(Dead,GR>-20&GR<20&relBAGR<4)

#subset data into individual species
Dead_F<-subset(Dead,Species=="F")
keeps<-c("ID2","Block","Dead","Easting","Northing","SL","Species","GR","BAGR","relGR","relBAGR","DBH2","BA2","relSize","Dead_dist","Dead_No")
Dead_F<-Dead_F[keeps]

#standardise variables following Zuur et al recommendations
head(Dead_F)
Dead_F_st<-cbind(Dead_F[,1:7],apply(X=Dead_F[,8:ncol(Dead_F)],MARGIN=2,FUN=function(x) {(x-mean(x))/sd(x)}))
head(Dead_F_st)

#candidates for growth rate variables
M0<-glmer(Dead~1+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))        
M_GR<-glmer(Dead~GR+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_BAGR<-glmer(Dead~BAGR+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_relGR<-glmer(Dead~relGR+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_relBAGR<-glmer(Dead~relBAGR+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))

AICc(M0,M_GR,M_BAGR,M_relGR,M_relBAGR)

new.dat<-data.frame(GR=seq(min(Dead_F$GR),max(Dead_F$GR),length=500),SL=1)
plot(Dead_F_st$GR,Dead_F_st$Dead)
points(new.dat$GR,plogis(predict(M_GR,newdata=new.dat,re.form=NA)),col="red")

#growth rate in DBH looks best - slow growing trees are more likley to die

#candidates for size variables
M0<-glmer(Dead~1+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))        
M_DBH<-glmer(Dead~DBH2+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_DBH2<-glmer(Dead~DBH2+I(DBH2^2)+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_BA<-glmer(Dead~BA2+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_BA2<-glmer(Dead~BA2+I(BA2^2)+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))

AICc(M0,M_DBH,M_DBH2,M_BA,M_BA2)
new.dat<-data.frame(DBH2=seq(min(Dead_F$DBH2),max(Dead_F$DBH2),length=500),SL=1)
plot(Dead_F_st$DBH2,Dead_F_st$Dead)
plot(new.dat$DBH2,plogis(predict(M_DBH,newdata=new.dat,re.form=NA)),col="red")
#bigger trees are more likley to die - DBH is the best predictor of this

#candidates for spatial relationships with dead trees
M0<-glmer(Dead~1+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))        
M_D_dist<-glmer(Dead~Dead_dist+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_D_dist2<-glmer(Dead~Dead_dist+I(Dead_dist^2)+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_D_no<-glmer(Dead~Dead_No+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
AICc(M0,M_D_dist,M_D_dist2,M_D_no)

#so distance to dead trees is best

#now remove variables that I'm not going to use in the analyses
Dead_F_st$DBH2_sq<-Dead_F_st$DBH2^2
head(Dead_F_st)
keeps<-c("Dead","DBH2","DBH2_sq","GR","Dead_dist","SL","Block")
Dead_F_st2<-Dead_F_st[keeps]

ggpairs(Dead_F_st2[,2:5]) #not much corrrelation between variables so should be safe to use them all

#now a model of DBH, growth, distance to dead trees, number of live trees
M1<-glmer(Dead~Dead_dist+DBH2+GR+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))

dotplot(ranef(M1,condVar=TRUE),
        lattice.options=list(layout=c(1,2)))

#now do model averaging
options(na.action = "na.fail")
models<-dredge(M1,trace = T,fixed="offset(log(SL))")

#produce model selection table
MS<-model.sel(models)

#now produce le Cessie-van Houwelingen-Copas-Hosmer global goodness of fit test statistics
M1<-glmer(Dead~Dead_dist+DBH2+GR+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M2<-glmer(Dead~DBH2+GR+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M3<-glmer(Dead~GR+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M4<-glmer(Dead~GR+Dead_dist+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M5<-glmer(Dead~DBH2+Dead_dist+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M6<-glmer(Dead~DBH2+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M7<-glmer(Dead~Dead_dist+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M8<-glmer(Dead~1+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))

Dead<-Dead_F_st$Dead
DBH2<-Dead_F_st$DBH2
GR<-Dead_F_st$GR
Dead_dist<-Dead_F_st$Dead_dist

Model_list<-list(M1,M2,M3,M4,M5,M6,M7,M8)
MS$GF_stat<-NA
MS$GF_pval<-NA
for (i in 1:8){
  get(paste("M",i,sep=""))->Model
  MS$GF_stat[i]<-HLgof.test(fit = fitted(Model), obs = Dead_F_st$Dead,X=model.matrix(Model))$gof$statistic
  MS$GF_pval[i]<-HLgof.test(fit = fitted(Model), obs = Dead_F_st$Dead,X=model.matrix(Model))$gof$p.value            
}

#save model selection table
write.csv(MS,"Tables/Beech_death_MS.csv",row.names=F)

#now save model averaged selction
Avs<-model.avg(MS,fit = T,subset =delta<=7)
write.csv(Avs$coefTable,"Tables/Beech_death_moav.csv")
importance(Avs)

#produce predictions from the model averaged coefficients
#first for growth rate
new.data.GR<-data.frame(GR=seq(min(Dead_F_st$GR),max(Dead_F_st$GR),length.out=500),SL=1,Dead_dist=mean(Dead_F_st$Dead_dist),DBH2=mean(Dead_F_st$DBH2),Type="Growth rate")
new.data.GR$DBH2_sq<-new.data.GR$DBH2^2
new.data.GR$Dead<-predict(Avs,newdata =new.data.GR,re.form=NA,type = "response")
new.data.GR$GR<-(new.data.GR$GR*sd(Dead_F$GR))+mean(Dead_F$GR)
plot(new.data.GR$GR,new.data.GR$Dead)

#next for DBH
new.data.DBH<-data.frame(GR=mean(Dead_F_st$GR),SL=1,Dead_dist=mean(Dead_F_st$Dead_dist),DBH2=seq(min(Dead_F_st$DBH2),max(Dead_F_st$DBH2),length.out=500),Type="DBH")
new.data.DBH$DBH2_sq<-new.data.GR$DBH2^2
new.data.DBH$Dead<-predict(Avs,newdata =new.data.DBH,re.form=NA,type = "response")
new.data.DBH$DBH2<-(new.data.DBH$DBH2*sd(Dead_F$DBH2))+mean(Dead_F$DBH2)
plot(new.data.DBH$DBH2,new.data.DBH$Dead)

#next for distance to dead tree
new.data.Dead<-data.frame(GR=mean(Dead_F_st$GR),SL=1,Dead_dist=seq(min(Dead_F_st$Dead_dist),max(Dead_F_st$Dead_dist),length.out=500),DBH2=mean(Dead_F_st$DBH2),Type="Distance to dead tree")
new.data.Dead$DBH2_sq<-new.data.GR$DBH2^2
new.data.Dead$Dead<-predict(Avs,newdata =new.data.Dead,re.form=NA,type = "response")
new.data.Dead$Dead_dist<-(new.data.Dead$Dead_dist*sd(Dead_F$Dead_dist))+mean(Dead_F$Dead_dist)
plot(new.data.Dead$Dead_dist,new.data.Dead$Dead)


#now create a figure of this
theme_set(theme_bw(base_size=12))
#growth rate
GR_P1<-ggplot(data=new.data.GR,aes(x=GR,y=Dead))+geom_line()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")                                                                                                                                            
GR_P2<-GR_P1+xlab("Growth rate (mm per year)")+ylab("Annual probability of death")

#DBH
DBH_P1<-ggplot(data=new.data.DBH,aes(x=DBH2,y=Dead))+geom_line()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")                                                                                                                                            
DBH_P2<-DBH_P1+xlab("Diameter at breast height (cm)")+ylab("Annual probability of death")

#distance from dead tree
Dead_P1<-ggplot(data=new.data.Dead,aes(x=Dead_dist,y=Dead))+geom_line()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")                                                                                                                                            
Dead_P2<-Dead_P1+xlab("Distance to nearest dead tree (m)")+ylab("Annual probability of death")

#put all figures together into one
png("Figures/Tree_death.png",height=4,width=12,res=600,units="in")
grid.arrange(GR_P2,DBH_P2,Dead_P2,ncol=3)
dev.off()
