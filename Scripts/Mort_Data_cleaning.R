rm(list=ls(all=TRUE))

#script to produce clean data for analysis of mortality

library(plyr)
library(ggplot2)
library(MuMIn)
library(sp)
library(spatstat)

#different measures of growth to include
#1. GR- mm growth per year
#2. BAGR - basal area growth per year
#3. relGR - % growth rate per year ((GR/10)/DBH-t0)*100
#3. relBAGR - % BA growth rate per year ((BAGR/10)/BA-t0)*100

#import data
DBH<-read.csv("Data/Denny_trees_cleaned.csv",colClasses=c("Tree_ID"="character"))
#subset trees to give only those inside plots
DBH<-subset(DBH,In_out=="In")
DBH<-subset(DBH,Year>1960)#for the moment remove data from 1959
DBH<-subset(DBH,Block<51)

IDs<-data.frame(ID1=as.character(unique(DBH$Tree_ID)))
IDs$ID2<-as.numeric(row.names(IDs))


#now give the DBH dataframe a new ID for each tree
DBH_ID<-NULL
for (i in 1:nrow(IDs)){
  Tree_sub<-subset(DBH,Tree_ID==as.character(IDs$ID1[i]))
  Tree_sub$ID2<-IDs$ID2[i]
  DBH_ID<-rbind(Tree_sub,DBH_ID)
}

#now create a grid that gives details of each tree in each year
Tree_grid<-data.frame(expand.grid(ID2=unique(DBH_ID$ID2),Year=unique(DBH_ID$Year),DBH=NA,BA=NA,Dead=NA))
Grid_bind<-NULL
for (i in 1:nrow(DBH_ID)){
Grid_sub<-subset(Tree_grid,ID2==DBH_ID$ID2[i])
Grid_sub<-subset(Grid_sub,Year==DBH_ID$Year[i])
Grid_sub$DBH<-DBH_ID$DBH[i]
Grid_sub$BA<-(Grid_sub$DBH^2)*0.00007854
Grid_sub$Dead<-DBH_ID$Status[i]
Grid_bind<-rbind(Grid_sub,Grid_bind)
}
head(Grid_bind)


Tree_grid2<-merge(Tree_grid,Grid_bind,by=c("ID2","Year"),all=T)
keeps<-c("ID2","Year","DBH.y","BA.y","Dead.y")
Tree_grid2<-Tree_grid2[keeps]
colnames(Tree_grid2)<-c("ID2","Year","DBH","BA","Dead")
Tree_grid2$Dead<-ifelse(Tree_grid2$Dead==1,0,1)
head(Tree_grid2)

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
      Tree_sub2$Dead2[y]<-NA#not present in dataset yet - not dead but not alive
    }
    else if (((sum(Tree_sub2$Dead[1:(y-1)],na.rm = T))>0)&&(is.na(Tree_sub2$Dead[y]))){
    Tree_sub2$Dead2[y]<-NA#died at previous time point
    }else if ((sum(Tree_sub2$Dead[1:(y-1)],na.rm = T)==0)&&(is.na(Tree_sub2$Dead[y]))){
      Tree_sub2$Dead2[y]<-1
    } else if (is.na(Tree_sub2$Dead[y-1])&&(Tree_sub2$Dead[y]==0)==T){
      Tree_sub2$Dead2[y]<-0
    }else if (Tree_sub2$Dead[y]==1){
      Tree_sub2$Dead2[y]<-1
    }else {
    Tree_sub2$Dead2[y]<-0
    }
  }
  Tree_dead<-rbind(Tree_sub2,Tree_dead)
}
Tree_dead<-Tree_dead[with(Tree_dead, order(ID2,Year)), ]
head(Tree_dead)

#now add a column to identify dead trees to work out
#cumulative number of dead trees over time and space

Tree_IDs<-unique(Tree_grid2$ID2)
Tree_dead2<-NULL
Tree_dead$Dead_cum<-NA
Tree_dead$Dead3<-Tree_dead$Dead2
Tree_dead$Dead3<-ifelse(Tree_dead$Dead2==0,0,Tree_dead$Dead2)
for (i in 1:length(Tree_IDs)){
  Tree_sub<-subset(Tree_dead,ID2==Tree_IDs[i])
  Tree_sub$Dead_cum<-Tree_sub$Dead3
for (j in 2:nrow(Tree_sub)){
  if (sum(Tree_sub$Dead3[1:j],na.rm = T)>=1){
    Tree_sub$Dead_cum[j]<-1
  }else{
    Tree_sub$Dead_cum[j]<-0
}
}
Tree_dead2<-rbind(Tree_sub,Tree_dead2)
}

Tree_dead2<-Tree_dead2[with(Tree_dead2, order(ID2,Year)), ]
head(Tree_dead2,n = 20)

#now calculate growth rates

Uni_Tree<-unique(Tree_dead2$ID2)
head(Tree_dead2)
Tree_dead2$GR<-NA
Tree_dead2$BAGR<-NA
Tree_dead2$relGR<-NA
Tree_dead2$relBAGR<-NA
Tree_dead2$DBH2<-NA
Tree_dead2$BA2<-NA
Tree_dead2$relSize<-NA

Tree_dead3<-NULL
for (i in 1:length(Uni_Tree)){
  Grid_sub<-subset(Tree_dead2,ID2==Uni_Tree[i])
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
  Tree_dead3<-rbind(Grid_sub,Tree_dead3)
}

Tree_dead3<-Tree_dead3[with(Tree_dead3, order(ID2,Year)), ]
head(Tree_dead3,n = 20)

Tree_dead4<-NULL
Tree_dead3$SL<-NA
Years<-data.frame(Year=unique(Tree_dead3$Year),SL=c(NA,20,4,12,18))
for (i in 1:nrow(Years)){
  Dead_sub<-subset(Tree_dead3,Year==Years$Year[i])
  Dead_sub$SL<-Years$SL[i]
  for (j in 1:nrow(Dead_sub)){
    BA_sub<-subset(Dead_sub,BA2<Dead_sub$BA2[j])
    BA_sum<-sum(BA_sub$BA2,na.rm = T)
    BA_sum2<-sum(Dead_sub$BA2,na.rm = T)
    Dead_sub$relSize[j]<-BA_sum/BA_sum2
  }
  Tree_dead4<-rbind(Dead_sub,Tree_dead4)
}

Tree_dead4<-Tree_dead4[with(Tree_dead4, order(ID2,Year)), ]
head(Tree_dead4,n = 20)

write.csv(Tree_dead4,"Data/Dead.csv",row.names=F)

#add location to mortality data
keeps<-c("ID2","Easting","Northing","Species")
DBH_Loc<-DBH_ID[keeps]
DBH_Loc<-unique(DBH_Loc)
DBH_Loc<-DBH_Loc[with(DBH_Loc, order(ID2)), ]
Dead_loc<-merge(Tree_dead4,DBH_Loc,by="ID2",all.x=T)
Dead_loc2<-subset(Dead_loc,Species=="Q"|Species=="F"|Species=="I")
head(Dead_loc2)
Dead_loc2$Dead_cum<-ifelse(Dead_loc2$Dead==1,0,Dead_loc2$Dead_cum)

############################################
#test to work out distance to nearest dead 
#tree of same species in previous time point

library(rgeos)
library(dismo)
library(raster)
library(rgeos)

#loop to test distance to nearest dead tree
#needs changing to get cumulative number of dead
#trees

SU<-unique(Dead_loc2$Species)
Yr<-unique(Dead_loc2$Year)
Years<-NULL
Years2<-NULL
for (i in 1:length(SU)){
  Sub_sp<-subset(Dead_loc2,Species==SU[i])
  for (j in 3:length(Yr)){
    if (nrow(subset(Sub_sp,Year==Yr[j-1]&Dead_cum==1))>0){
    Yr1<-subset(Sub_sp,Year==Yr[j-1]&Dead_cum==1)
    Yr2<-subset(Sub_sp,Year==Yr[j])
    Yr2$Dead_dist<-NA
    a <- SpatialPointsDataFrame(coords = data.frame(x = Yr2$Easting, y =Yr2$Northing ),data=data.frame(Yr2$BA))
    b <- SpatialPointsDataFrame(coords = data.frame(x = Yr1$Easting, y =Yr1$Northing ),data=data.frame(Yr1$BA2))
    buffer<- gBuffer( a, width=10, byid=TRUE )
    Yr2$Dead_No<-as.numeric(sapply(over(buffer, geometry(b), returnList = TRUE), length))
    Yr2$Dead_BA<-as.numeric(over(buffer,b,fn=sum)[,1])
    results <- spDists(a, b, longlat=F)  
    results<-ifelse(results==0,NA,results)
    for (k in 1:nrow(results)){    
      Yr2$Dead_dist[k]<-min(results[k,1:ncol(results)],na.rm = T)
    }
    }else{
      Yr2<-subset(Sub_sp,Year==Yr[j])
      Yr2$Dead_No<-0
      Yr2$Dead_BA<-0
      Yr2$Dead_dist<-NA
    }
    Years<-rbind(Years,Yr2)
    
  }
  Years2<-rbind(Years,Years2)
}

Years2$Dead_BA<-ifelse(is.na(Years2$Dead_BA),0,Years2$Dead_BA)
summary(Years2)

write.csv(Years2,"Data/Dead_size.csv",row.names=F)
