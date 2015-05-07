rm(list=ls(all=TRUE))

#script to produce model of tree growth over time

library(plyr)
library(ggplot2)
library(MuMIn)
library(sp)
library(spatstat)
library(dplyr)
library(lme4)
library(lattice)

#different measures of growth to include
#1. GR- mm growth per year
#2. BAGR - basal area growth per year

#import data
DBH<-read.csv("Data/Denny_trees_cleaned.csv",colClasses=c("Tree_ID"="character"))
DBH<-subset(DBH,In_out=="In") #subset trees to give only those inside plots
DBH<-subset(DBH,Status==1)
head(DBH)

IDs<-distinct(DBH[c("Tree_ID","Block")])
colnames(IDs)<-c("ID1","Block")
IDs$ID2<-as.numeric(row.names(IDs))

#now give the DBH dataframe a new ID for each tree
DBH_ID<-NULL
for (i in 1:nrow(IDs)){
  Tree_sub<-subset(DBH,Tree_ID==as.character(IDs$ID1[i]))
  Tree_sub$ID2<-IDs$ID2[i]
  DBH_ID<-rbind(Tree_sub,DBH_ID)
}

DBH_ID<-DBH_ID[with(DBH_ID, order(ID2,Year)), ]
DBH_ID$DBH_Inc<-NULL
DBH_ID$DBH2<-NULL
DBH_ID$Age<-NA
DBH_ID$Int<-NA
UID<-unique(DBH_ID$ID2)
DBH_sub2<-NULL
for (i in 1:length(UID)){
  DBH_sub<-subset(DBH_ID,ID2==UID[i])
if (nrow(DBH_sub)>1){
  for (y in 1:(nrow(DBH_sub)-1)){
    DBH_sub$DBH_Inc[y]<-(DBH_sub$DBH[y+1]-DBH_sub$DBH[y])/(DBH_sub$Year[y+1]-DBH_sub$Year[y])
    DBH_sub$DBH2[y]<-DBH_sub$DBH[y+1]
    DBH_sub$Age[y]<-DBH_sub$Year[y]-DBH_sub$Year[1]
    DBH_sub$Int[y+1]<-DBH_sub$Year[y+1]-DBH_sub$Year[y]
  }
}else{
  DBH_sub$DBH_Inc<-NA
  DBH_sub$DBH2<-NA
  DBH_sub$Age<-NA
  DBH_sub$Int<-NA
}
z<-nrow(DBH_sub)
DBH_sub$DBH_Inc[z]<-NA
DBH_sub$DBH2[z]<-NA
DBH_sub$Int[z]<-NA
DBH_sub2<-rbind(DBH_sub,DBH_sub2)
}
DBH_sub2<-DBH_sub2[with(DBH_sub2, order(ID2,Year)), ]

DBH_sub3<-subset(DBH_sub2,!is.na(DBH_Inc))

DBH_sub3$DBH_Inc<-ifelse(DBH_sub3$DBH_Inc<=-1,-1,DBH_sub3$DBH_Inc)

#subset data so that only Beech trees are included
Beech_growth<-subset(DBH_sub3,Species=="F"&DBH>5)
M0<-lmer(DBH_Inc~1+(1|Block/ID2),data=Beech_growth)

#looks like the random effects are worth including
d1 <- ranef(M0, condVar=TRUE)
dotplot(d1)[["Block"]]

#now look at fitted models
M1<-lmer(DBH_Inc~DBH+(1|Block/ID2),data=Beech_growth)
M2<-lmer(DBH_Inc~DBH+I(DBH^2)+(1|Block/ID2),data=Beech_growth)
M3<-lmer(DBH_Inc~DBH+I(DBH^2)+I(DBH^3)+(1|Block/ID2),data=Beech_growth)
d1 <- ranef(M2, condVar=TRUE)

summary(M2)
AICc(M0,M1,M2,M3)

#now create plots of this
newdat<-data.frame(DBH=seq(5,max(Beech_growth$DBH),length.out = 500))
newdat$DBH_Inc<-0
mm <- model.matrix(terms(M2),newdat)
newdat$DBH_Inc <- predict(M2,newdat,re.form=NA)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(M2),mm))
tvar1 <- pvar1+VarCorr(M2)$Block[1]  ## must be adapted for more complex models
newdat <- data.frame(
    newdat
    , plo = newdat$DBH_Inc-2*sqrt(pvar1)
    , phi = newdat$DBH_Inc+2*sqrt(pvar1)
    , tlo = newdat$DBH_Inc-2*sqrt(tvar1)
    , thi = newdat$DBH_Inc+2*sqrt(tvar1)
  )

theme_set(theme_bw(base_size=12))
P1<-ggplot(newdat,aes(x=DBH,y=DBH_Inc,ymax=phi,ymin=plo))+geom_ribbon(alpha=0.2)+geom_ribbon(aes(ymax=thi,ymin=tlo),alpha=0.1)+geom_line(size=2)
P1+geom_point(data=Beech_growth,aes(x=DBH,y=DBH_Inc,ymin=NULL,ymax=NULL),shape=1,alpha=0.1)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")+ylim(-1,1.5)                                                                                                                                            

summary(M2)


Final_tree<-NULL
for (y in 1:100){
  print(y)
  Tree2<-data.frame(DBH=5,Years=1,Tree=y)
for (i in 1:400){
  DBH_Inc<-abs(rnorm(1,1.859e-01,sd=5.609e-02*sqrt(476)))+
    abs((rnorm(1,8.735e-03,sd=1.349e-03*sqrt(476))*Tree2$DBH[i]))-
    abs(rnorm(1,-7.422e-05,sd=1.351e-05*sqrt(476))*Tree2$DBH[i]^2)
  DBH2<-Tree2$DBH[i]+DBH_Inc
  Tree3<-data.frame(DBH=DBH2,Years=i+1,Tree=y)
  Tree2<-rbind(Tree2,Tree3)
}
Final_tree<-rbind(Tree2,Final_tree)
}
tail(Final_tree,n=100)



ggplot(Final_tree,aes(x=Years,y=DBH,group=Tree))+geom_line(alpha=0.1)
