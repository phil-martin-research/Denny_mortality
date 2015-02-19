#script to produce clean data for analysis of mortality

#########################################
#########################################
#notes:
#1. Need to come up with a loop that classifies trees as dead/alive correctly
#this needs to identify them as being dead or alive for the years they were surveyed
#also need to work out how to deal with new trees


library(plyr)
library(ggplot2)
library(lme4)
library(MuMIn)

#different measures of growth to include
#1. GR- mm growth per year
#2. BAGR - basal area growth per year
#3. relGR - % growth rate per year ((GR/10)/DBH-t0)*100
#3. relBAGR - % BA growth rate per year ((BAGR/10)/BA-t0)*100

#import data
DBH<-read.csv("Data/Denny_trees_cleaned.csv",colClasses=c("Tree_ID"="character"))
#subset trees to give only those inside plots
DBH<-subset(DBH,In_out=="In")
DBH<-subset(DBH,Year>1960)#for the moment remove data from 1959 - but this may need fixing!
DBH<-subset(DBH,Block<51)

IDs<-data.frame(ID1=as.character(unique(DBH$Tree_ID)))
IDs$ID2<-as.numeric(row.names(IDs))

#now give the DBH dataframe a new ID for each tree
DBH_ID<-NULL
for (i in 1:nrow(Tree_ID)){
  Tree_sub<-subset(DBH,Tree_ID==as.character(IDs$ID1[i]))
  Tree_sub$ID2<-IDs$ID2[i]
  DBH_ID<-rbind(Tree_sub,DBH_ID)
}


#now create a grid that gives details of each tree in each year


Tree_grid<-data.frame(expand.grid(ID2=unique(DBH_ID$ID2),Year=unique(DBH_ID$Year),DBH=NA,BA=NA,Dead=NA))

head(Tree_grid)
Grid_bind<-NULL
for (i in 1:nrow(DBH_ID)){
Grid_sub<-subset(Tree_grid,ID2==DBH_ID$ID2[i])
Grid_sub<-subset(Grid_sub,Year==DBH_ID$Year[i])
Grid_sub$DBH<-DBH_ID$DBH[i]
Grid_sub$BA<-(Grid_sub$DBH^2)*0.00007854
Grid_sub$Dead<-DBH_ID$Status[i]
Grid_bind<-rbind(Grid_sub,Grid_bind)
}


Tree_grid2<-merge(Tree_grid,Grid_bind,by=c("ID2","Year"),all=T)
keeps<-c("ID2","Year","DBH.y","BA.y","Dead.y")
Tree_grid2<-Tree_grid2[keeps]
colnames(Tree_grid2)<-c("ID2","Year","DBH","BA","Dead")
Tree_grid2$Dead<-ifelse(Tree_grid2$Dead==1,0,1)

#work out whether trees are dead
#run a loop for each tree to look at rows above
#if any of the rows indicate that the tree is alive at
#a previous time point classify it as dead,
#otherwise classify it as alive
Tree_IDs<-unique(Tree_grid2$ID2)
Tree_dead<-NULL
for (i in 1:length(Tree_IDs)){
  Tree_sub2<-subset(Tree_grid2,ID2==Tree_IDs[i])
  Tree_sub2$Dead2<-NA
  Tree_sub2$Dead2[1]<-Tree_sub2$Dead[1]
  for (y in 2:nrow(Tree_sub2)){
    if (is.na(Tree_sub2$Dead[y])&&is.na(Tree_sub2$Dead[y-1])==T){
      Tree_sub2$Dead2[y]<-NA
    }
    else if (((sum(Tree_sub2$Dead[1:(y-1)],na.rm = T))>0)&&(is.na(Tree_sub2$Dead[y]))){
    Tree_sub2$Dead2[y]<-NA
    }else if (((sum(Tree_sub2$Dead[1:(y-1)],na.rm = T))==0)&&(is.na(Tree_sub2$Dead[y]))){
      Tree_sub2$Dead2[y]<-1
    } else {
    Tree_sub2$Dead2[y]<-0
    }
  }
  Tree_dead<-rbind(Tree_sub2,Tree_dead)
}

summary(Tree_dead)
head(Tree_dead)


#now calculate growth rates
Uni_Tree<-unique(Tree_dead$ID2)
head(Tree_dead)
Tree_dead$GR<-NA
Tree_dead$BAGR<-NA
Tree_dead$relGR<-NA
Tree_dead$relBAGR<-NA
Tree_dead$DBH2<-NA
Tree_dead$BA2<-NA
Tree_dead2<-NULL
for (i in 1:length(Uni_Tree)){
  Grid_sub<-subset(Tree_dead,ID2==Uni_Tree[i])
  for (y in 3:nrow(Grid_sub)){
  Grid_sub$GR[y]<-((Grid_sub$DBH[y-1]-Grid_sub$DBH[y-2])*10)/(Grid_sub$Year[y-1]-Grid_sub$Year[y-2])
  Grid_sub$BAGR[y]<-(((Grid_sub$BA[y-1]-Grid_sub$BA[y-2]))*10000)/(Grid_sub$Year[y-1]-Grid_sub$Year[y-2])
  Grid_sub$relGR[y]<-((Grid_sub$GR[y]/10)/Grid_sub$DBH[y-1])*100
  Grid_sub$relBAGR[y]<-(Grid_sub$BAGR[y]/10000)/Grid_sub$BA[y-2]
  }
  for (h in 2:nrow(Grid_sub)){
    Grid_sub$DBH2[h]<-Grid_sub$DBH[h-1]
    Grid_sub$BA2[h]<-Grid_sub$BA[h-1]
  }
  Tree_dead2<-rbind(Grid_sub,Tree_dead2)
}

head(Tree_dead2)

Tree_dead3<-subset(Tree_dead2,!is.na(Dead2))
Tree_dead3<-subset(Tree_dead3,Year>=1988)
Tree_dead3<-subset(Tree_dead3,GR>-10)
Tree_dead3<-subset(Tree_dead3,GR<10)

head(Tree_dead3)

summary(Tree_dead2)
ggplot(Tree_dead3,aes(x=GR))+geom_density()+facet_grid(Dead2~Year)
ggplot(Tree_dead3,aes(x=relGR))+geom_density()+facet_grid(Dead2~Year)
ggplot(Tree_dead3,aes(x=relBAGR))+geom_density()+facet_grid(Dead2~Year)

ggplot(Tree_dead3,aes(x=DBH2,y=relGR))+geom_point()+facet_wrap(~Year)+geom_smooth(method="lm")

ggplot(Tree_dead3,aes(x=BAGR,y=Dead2))+geom_point(shape=1)+facet_wrap(~Year)+geom_smooth(family="binomial",method=glm)

ggplot(Tree_dead3,aes(x=DBH2,y=Dead2))+geom_point(shape=1)+facet_wrap(~Year)+geom_smooth(family="binomial",method=glm)
ggplot(Tree_dead3,aes(x=BA2,y=Dead2))+geom_point(shape=1)+facet_wrap(~Year)+geom_smooth(family="binomial",method=glm)


head(Tree_dead3)

M1<-glmer(Dead2~BAGR+(1|ID2),data=Tree_dead3,family=binomial)
M2<-glmer(Dead2~DBH2+(1|ID2),data=Tree_dead3,family=binomial)
M3<-glmer(Dead2~DBH2+I(DBH2^2)+(1|ID2),data=Tree_dead3,family=binomial)
M4<-glmer(Dead2~DBH2+I(DBH2^2)+BAGR+(1|ID2),data=Tree_dead3,family=binomial)


AICc(M1,M2,M3,M4)
r.squaredGLMM(M1)

plot(Tree_dead3$DBH2,plogis(predict(M3,re.form=NA)))


Tree_dead3$DBH2<-round(Tree_dead3$DBH,-1)

Death_rate<-ddply(Tree_dead3,.(Year,DBH2),summarize,Rate=sum(Dead2)/length(Dead2))

Death_rate$Period<-NA
Death_rate$Period[1]<-20
for (i in 2:nrow(Death_rate)){
  Death_rate$Period[i]<-Death_rate$Year[i]-Death_rate$Year[i-1]
}

Death_rate$Cor_rate<-Death_rate$Rate/Death_rate$Period

ggplot(Death_rate,aes(x=DBH2,y=Rate))+geom_point()+facet_wrap(~Year)

summary(Tree_dead3$Dead2)

