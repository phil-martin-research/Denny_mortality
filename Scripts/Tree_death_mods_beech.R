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
library(gridExtra)

#import data
rm(list=ls(all=TRUE))
Dead<-read.csv("Data/Dead_size.csv")
Soils<-read.csv("Data/Soil_type.csv")

#subset to remove data prior to 1984
Dead<-subset(Dead,Year>1984)
summary(Dead)
#filter out trees that have unrealistic growth rates
Dead<-subset(Dead,!is.na(Dead))
Dead<-subset(Dead,GR>-20&GR<20&relBAGR<4)
Dead<-merge(Dead,Soils,by.x="Block",by.y="Plot")


#subset data into individual species
Dead_F<-subset(Dead,Species=="F")
keeps<-c("ID2","Block","Dead","Easting","Northing","SL","Species","GR","BAGR","relGR","relBAGR","DBH2","BA2","relSize","Dead_dist","Dead_No","Clay","Silt","Sand")
Dead_F<-Dead_F[keeps]

#standardise variables following Zuur et al recommendations
Dead_F_st<-cbind(Dead_F[,1:7],apply(X=Dead_F[,8:ncol(Dead_F)],MARGIN=2,FUN=function(x) {(x-mean(x))/sd(x)}))

#candidates for growth rate variables
M0<-glmer(Dead~1+offset(log(SL))+(1|Block),Dead_F_st,family=binomial(link="cloglog"))        
M_GR<-glmer(Dead~GR+offset(log(SL))+(1|Block),Dead_F_st,family=binomial(link="cloglog"))
M_BAGR<-glmer(Dead~BAGR+offset(log(SL))+(1|Block),Dead_F_st,family=binomial(link="cloglog"))
M_relGR<-glmer(Dead~relGR+offset(log(SL))+(1|Block),Dead_F_st,family=binomial(link="cloglog"))
M_relBAGR<-glmer(Dead~relBAGR+offset(log(SL))+(1|Block),Dead_F_st,family=binomial(link="cloglog"))
#growth rate in DBH is best - slow growing trees are more likley to die

#candidates for size variables
M0<-glmer(Dead~1+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))        
M_DBH<-glmer(Dead~DBH2+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_DBH2<-glmer(Dead~DBH2+I(DBH2^2)+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_BA<-glmer(Dead~BA2+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_BA2<-glmer(Dead~BA2+I(BA2^2)+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
AICc(M0,M_DBH,M_DBH2,M_BA,M_BA2)
#bigger trees are more likley to die - DBH is the best predictor of this

#candidates for spatial relationships with dead trees
M0<-glmer(Dead~1+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))        
M_D_dist<-glmer(Dead~Dead_dist+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_D_dist2<-glmer(Dead~Dead_dist+I(Dead_dist^2)+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_D_no<-glmer(Dead~Dead_No+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
AICc(M0,M_D_dist,M_D_dist2,M_D_no)
# distance to dead trees is best predictor

#candidates for  relationships with soil 
M0<-glmer(Dead~1+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))        
M_Clay<-glmer(Dead~Clay+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_Silt<-glmer(Dead~Silt+I(Dead_dist^2)+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
M_Sand<-glmer(Dead~Sand+offset(log(SL))+(1|Block),Dead_F,family=binomial(link="cloglog"))
AICc(M0,M_Clay,M_Silt,M_Sand)
# distance to dead trees is best predictor



#now remove variables that I'm not going to use in the analyses
Dead_F_st$DBH2_sq<-Dead_F_st$DBH2^2
keeps<-c("Dead","DBH2","DBH2_sq","GR","Dead_dist","Clay","SL","Block")
Dead_F_st2<-Dead_F_st[keeps]
ggpairs(Dead_F_st2[,2:6]) #not much corrrelation between variables so should be safe to use them all

#now a model of DBH, growth, distance to dead trees, number of live trees
M1<-glmer(Dead~Dead_dist+DBH2+GR+Clay+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))

#now do model averaging
options(na.action = "na.fail")
models<-dredge(M1,trace = T,fixed="offset(log(SL))")

#produce model selection table
MS<-model.sel(models,subset=delta<=7)
MS<-subset(MS,delta<=7)

#now produce le Cessie-van Houwelingen-Copas-Hosmer global goodness of fit test statistics
M1<-glmer(Dead~Dead_dist+DBH2+GR+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M2<-glmer(Dead~DBH2+GR+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M3<-glmer(Dead~DBH2+GR+Clay+Dead_dist+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M4<-glmer(Dead~Clay+DBH2+GR+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M5<-glmer(Dead~GR+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M6<-glmer(Dead~Dead_dist+GR+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M7<-glmer(Dead~Dead_dist+GR+Clay+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))
M8<-glmer(Dead~Clay+GR+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))


Model_list<-list(M1,M2,M3,M4,M5,M6,M7,M8)
MS$GF_stat<-NA
MS$GF_pval<-NA
for (i in 1:8){
  get(paste("M",i,sep=""))->Model
  MS$GF_stat[i]<-HLgof.test(fit = fitted(Model), obs = Dead_F_st$Dead,X=model.matrix(Model))$gof$statistic
  MS$GF_pval[i]<-HLgof.test(fit = fitted(Model), obs = Dead_F_st$Dead,X=model.matrix(Model))$gof$p.value            
}
MS1<-data.frame(MS)

#save model selection table
write.csv(MS1,"Tables/Beech_death_MS.csv",row.names=F)
importance(MS)


#now save model averaged selection
Avs<-model.avg(MS,fit = T,subset =delta<=7)
summary(Avs)

importance(Avs)


#produce predictions from the model averaged coefficients
Full_model<-glmer(Dead~Dead_dist+DBH2+GR+Clay+offset(log(SL))+(1|Block),Dead_F_st2,family=binomial(link="cloglog"))

#first for growth rate
new.data.GR<-data.frame(Dead=0,GR=seq(-2,max(Dead_F_st2$GR),length.out=500),SL=1,Dead_dist=mean(Dead_F_st2$Dead_dist),DBH2=mean(Dead_F_st2$DBH2),Clay=mean(Dead_F_st2$Clay),Type="Growth rate")
mm <- model.matrix(terms(Full_model),new.data.GR)
new.data.GR$GR<-(new.data.GR$GR*sd(Dead_F$GR))+mean(Dead_F$GR)
new.data.GR$Dead<-predict(Avs,newdata=new.data.GR,re.form=NA,type = "response")

#next for DBH
new.data.DBH<-data.frame(Dead=0,GR=mean(Dead_F_st2$GR),SL=1,Dead_dist=mean(Dead_F_st2$Dead_dist),DBH2=seq(min(Dead_F_st2$DBH2),max(Dead_F_st2$DBH2),length.out=500),Clay=mean(Dead_F_st2$Clay),Type="DBH")
new.data.DBH$DBH2<-(new.data.DBH$DBH2*sd(Dead_F$DBH2))+mean(Dead_F$DBH2)
new.data.DBH$Dead<-predict(Avs,newdata=new.data.DBH,re.form=NA,type = "response")

#next for distance to dead tree
new.data.Dead<-data.frame(Dead=0,GR=mean(Dead_F_st$GR),SL=1,Dead_dist=seq(min(Dead_F_st$Dead_dist),max(Dead_F_st$Dead_dist),length.out=500),DBH2=mean(Dead_F_st$DBH2),Clay=mean(Dead_F_st2$Clay),Type="Distance to dead tree")
new.data.Dead$Dead_dist<-(new.data.Dead$Dead_dist*sd(Dead_F$Dead_dist))+mean(Dead_F$Dead_dist)
new.data.Dead$Dead<-predict(Avs,newdata=new.data.Dead,re.form=NA,type = "response")

#next for soil type
new.data.Clay<-data.frame(GR=mean(Dead_F_st$GR),SL=1,Dead_dist=mean(Dead_F_st$Dead_dist),DBH2=mean(Dead_F_st$DBH2),Clay=seq(min(Dead_F_st$Sand),max(Dead_F_st$Sand),length.out=500),Type="Clay content")
new.data.Clay$Clay<-(new.data.Clay$Clay*sd(Dead_F$Clay))+mean(Dead_F$Clay)
new.data.Clay$Dead<-predict(Avs,newdata=new.data.Clay,re.form=NA,type = "response")

#now create a figure of this
theme_set(theme_bw(base_size=12))
#growth rate
GR_P1<-ggplot(data=new.data.GR,aes(x=GR,y=1-exp(-exp(Dead))))+geom_line()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")                                                                                                                                            
GR_P2<-GR_P1+xlab("Growth rate (mm per year)")+ylab("Annual probability of death")+ annotate("text", x = -1, y = 0.645, label = "(a)")

#DBH
DBH_P1<-ggplot(data=new.data.DBH,aes(x=DBH2,y=1-exp(-exp(Dead))))+geom_line()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")                                                                                                                                            
DBH_P2<-DBH_P1+xlab("Diameter at breast height (cm)")+ylab("Annual probability of death")+ annotate("text", x = 0, y = 1, label = "(b)")


#distance from dead tree
Dead_P1<-ggplot(data=new.data.Dead,aes(x=Dead_dist,y=1-exp(-exp(Dead))))+geom_line()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")                                                                                                                                            
Dead_P2<-Dead_P1+xlab("Distance to nearest dead tree (m)")+ylab("Annual probability of death")+ annotate("text", x = 0, y = 0.635, label = "(c)")

#sand content
Clay_P1<-ggplot(data=new.data.Clay,aes(x=Clay,y=1-exp(-exp(Dead))))+geom_line()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")                                                                                                                                            
Clay_P2<-Clay_P1+xlab("Clay content of soil (%)")+ylab("Annual probability of death")+ annotate("text", x = 0, y = 0.643, label = "(d)")

#put all figures together into one
png("Figures/Tree_death.png",height=6,width=10,res=300,units="in")
grid.arrange(GR_P2,DBH_P2,Dead_P2,Clay_P2,ncol=2)
dev.off()
