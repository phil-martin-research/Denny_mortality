rm(list=ls(all=TRUE))

#script to produce clean data for analysis of mortality

library(plyr)
library(ggplot2)
library(MuMIn)
library(sp)
library(spatstat)
library(dplyr)

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

#now create a grid that gives details of each tree in each year
Tree_grid<-data.frame(expand.grid(ID2=unique(DBH_ID$ID2),Year=unique(DBH_ID$Year),DBH=NA,BA=NA,Dead=NA))
Grid_bind<-NULL
for (i in 1:nrow(DBH_ID)){
Grid_sub<-subset(Tree_grid,ID2==DBH_ID$ID2[i])
Grid_sub<-subset(Grid_sub,Year==DBH_ID$Year[i])
Grid_sub$DBH<-DBH_ID$DBH[i]
Grid_sub$BA<-((Grid_sub$DBH)^2*(pi/4))/10000
Grid_sub$Dead<-DBH_ID$Status[i]
Grid_bind<-rbind(Grid_sub,Grid_bind)
}

head(Grid_bind)
Tree_grid1<-merge(Grid_bind,Tree_grid,by=c("ID2","Year"),all=T)
head(Tree_grid1)
head(IDs)
Tree_grid2<-merge(Tree_grid1,IDs,by=c("ID2"),all=T)
head(Tree_grid2)
Tree_grid2<-subset(Tree_grid2,!(Block>51&Year==1984)&!(Block>51&Year==1988)&!(Block>51&Year==1996))
Tree_grid2<-subset(Tree_grid2,!(Block<51&Year==1999))
keeps<-c("Block","ID2","Year","DBH.x","BA.x","Dead.x")
Tree_grid2<-Tree_grid2[keeps]
colnames(Tree_grid2)<-c("Block","ID2","Year","DBH","BA","Dead")
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
  if (nrow(Tree_sub2)>1){
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
}else{
  if (Tree_sub2$Block<51){
    if((Tree_sub2$Year)==1964){
      Y<-1984
    } else if ((Tree_sub2$Year)==1984){
      Y<-1988
    }else if ((Tree_sub2$Year)==1988){
      Y<-1996
    }else if ((Tree_sub2$Year)==1996){
      Y<-2014
    }
    Tree_sub2<-rbind(Tree_sub2,data.frame(Block=Tree_sub2$Block[1],ID2=Tree_sub2$ID2[1],Year=Y,DBH=NA,BA=NA,Dead=1,Dead2=1))
  }else{
    if((Tree_sub2$Year)==1964){
      Y<-1999
    } else if ((Tree_sub2$Year)==1999){
      Y<-2014
      }else{
      Y<-2020
      }
    Tree_sub2<-rbind(Tree_sub2,data.frame(Block=Tree_sub2$Block[1],ID2=Tree_sub2$ID2[1],Year=Y,DBH=NA,BA=NA,Dead=1,Dead2=1))  
  }
  }
Tree_dead<-rbind(Tree_sub2,Tree_dead)
}
Tree_dead<-Tree_dead[with(Tree_dead, order(ID2,Year)), ]
head(Tree_dead)
Tree_dead<-subset(Tree_dead,Year<2020)
tail(Tree_dead,n = 50)

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
head(Tree_dead,n = 20)

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

Tree_dead3$GR<-ifelse(Tree_dead3$GR<0,0,Tree_dead3$GR)
Tree_dead3$BAGR<-ifelse(Tree_dead3$BAGR<0,0,Tree_dead3$BAGR)
Tree_dead3$relGR<-ifelse(Tree_dead3$GR<0,0,Tree_dead3$elGR)
Tree_dead3$relBAGR<-ifelse(Tree_dead3$relBAGR<0,0,Tree_dead3$relBAGR)


Tree_dead3$SL<-NA
Years_1<-data.frame(Year=c(1964,1984,1988,1996,2014),SL=c(NA,20,4,12,18))
Tree_dead4_1<-NULL
Tree_dead3_1<-subset(Tree_dead3,Block<51)
for (i in 1:nrow(Years_1)){
  Dead_sub<-subset(Tree_dead3_1,Year==Years_1$Year[i])
  Dead_sub$SL<-Years_1$SL[i]
  for (j in 1:nrow(Dead_sub)){
    BA_sub<-subset(Dead_sub,BA2<Dead_sub$BA2[j])
    BA_sum<-sum(BA_sub$BA2,na.rm = T)
    BA_sum2<-sum(Dead_sub$BA2,na.rm = T)
    Dead_sub$relSize[j]<-BA_sum/BA_sum2
  }
  Tree_dead4_1<-rbind(Dead_sub,Tree_dead4_1)
}

Years_2<-data.frame(Year=c(1964,1999,2014),SL=c(NA,35,15))
Tree_dead3_2<-subset(Tree_dead3,Block>51)
Tree_dead4_2<-NULL
for (i in 1:nrow(Years_2)){
  Dead_sub<-subset(Tree_dead3_2,Year==Years_2$Year[i])
  Dead_sub$SL<-Years_2$SL[i]
  for (j in 1:nrow(Dead_sub)){
    BA_sub<-subset(Dead_sub,BA2<Dead_sub$BA2[j])
    BA_sum<-sum(BA_sub$BA2,na.rm = T)
    BA_sum2<-sum(Dead_sub$BA2,na.rm = T)
    Dead_sub$relSize[j]<-BA_sum/BA_sum2
  }
  Tree_dead4_2<-rbind(Dead_sub,Tree_dead4_2)
}

Tree_dead4<-rbind(Tree_dead4_1,Tree_dead4_2)
Tree_dead4<-Tree_dead4[with(Tree_dead4, order(ID2,Year)), ]
head(Tree_dead4,n = 20)
tail(Tree_dead4,n = 20)

write.csv(Tree_dead4,"Data/Dead.csv",row.names=F)

#add location to mortality data
keeps<-c("ID2","Easting","Northing","Species")
DBH_Loc<-DBH_ID[keeps]
DBH_Loc<-unique(DBH_Loc)
DBH_Loc<-DBH_Loc[with(DBH_Loc, order(ID2)), ]
head(Tree_dead4)
Dead_loc<-merge(Tree_dead4,DBH_Loc,by="ID2",all.x=T)
Dead_loc2<-subset(Dead_loc,Species=="Q"|Species=="F"|Species=="I")
Dead_loc2$Dead2<-ifelse(is.na(Dead_loc2$Dead),0,Dead_loc2$Dead2)
Dead_loc2$Dead_cum2<-Dead_loc2$Dead_cum-Dead_loc2$Dead2
head(Dead_loc2)
summary(Dead_loc2)

ggplot(Dead_loc2,aes(x=Easting,y=Northing,colour=as.factor(Dead_cum2)))+geom_point(shape=1)+facet_grid(Species~Year)


write.csv(Dead_loc2,"Data/Dead_spat.csv",row.names=F)

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
Years_1<-data.frame(Year=c(1964,1984,1988,1996,2014),SL=c(NA,20,4,12,18))
Dead_loc2_1<-subset(Dead_loc2,Block<51)
Years<-NULL
Years2_1<-NULL
for (i in 1:length(SU)){
  Sub_sp<-subset(Dead_loc2_1,Species==SU[i])
  for (j in 2:nrow(Years_1)){
    if (nrow(subset(Sub_sp,Year==Years_1$Year[j-1]&Dead_cum2==1))>0){
    Yr1<-subset(Sub_sp,Year==Years_1$Year[j-1]&Dead_cum2==1&DBH2>10)
    Yr2<-subset(Sub_sp,Year==Years_1$Year[j])
    Yr2$Dead_dist<-NA
    a <- SpatialPointsDataFrame(coords = data.frame(x = Yr2$Easting, y =Yr2$Northing ),data=data.frame(Yr2$BA))
    b <- SpatialPointsDataFrame(coords = data.frame(x = Yr1$Easting, y =Yr1$Northing ),data=data.frame(Yr1$BA))
    plot(a)
    points(b,col="red")
    buffer<- gBuffer( a, width=10, byid=TRUE )
    plot(buffer)
    points(a,col="red",)
    
    Yr2$Dead_No<-as.numeric(sapply(over(buffer, geometry(b), returnList = TRUE), length))
    #Yr2$Dead_BA<-as.numeric(over(buffer,b,fn=sum)[,1])
    results <- spDists(a, b, longlat=F)  
    results<-ifelse(results==0,NA,results)
    for (k in 1:nrow(results)){    
      Yr2$Dead_dist[k]<-min(results[k,1:ncol(results)],na.rm = T)
    }
    }else{
      Yr2<-subset(Sub_sp,Year==Years_1$Year[j])
      Yr2$Dead_No<-0
      #Yr2$Dead_BA<-0
      Yr2$Dead_dist<-NA
    }
    Years<-rbind(Years,Yr2)
    
  }
  Years2_1<-rbind(Years,Years2_1)
}

SU<-c("I","F")
Years_2<-data.frame(Year=c(1964,1999,2014),SL=c(NA,35,15))
Dead_loc2_2<-subset(Dead_loc2,Block>51)
Years<-NULL
Years2_2<-NULL
for (i in 1:length(SU)){
  Sub_sp<-subset(Dead_loc2_2,Species==SU[i])
  for (j in 2:nrow(Years_2)){
    if (nrow(subset(Sub_sp,Year==Years_2$Year[j-1]&Dead_cum2==1))>0){
      Yr1<-subset(Sub_sp,Year==Years_2$Year[j-1]&Dead_cum2==1&DBH2>10)
      Yr2<-subset(Sub_sp,Year==Years_2$Year[j])
      Yr2$Dead_dist<-NA
      a <- SpatialPointsDataFrame(coords = data.frame(x = Yr2$Easting, y =Yr2$Northing ),data=data.frame(Yr2$BA))
      b <- SpatialPointsDataFrame(coords = data.frame(x = Yr1$Easting, y =Yr1$Northing ),data=data.frame(Yr1$BA))
      plot(a)
      points(b,col="red")
      buffer<- gBuffer( a, width=10, byid=TRUE )
      plot(buffer)
      points(a,col="red",)
      
      Yr2$Dead_No<-as.numeric(sapply(over(buffer, geometry(b), returnList = TRUE), length))
      #Yr2$Dead_BA<-as.numeric(over(buffer,b,fn=sum)[,1])
      results <- spDists(a, b, longlat=F)  
      results<-ifelse(results==0,NA,results)
      for (k in 1:nrow(results)){    
        Yr2$Dead_dist[k]<-min(results[k,1:ncol(results)],na.rm = T)
      }
    }else{
      Yr2<-subset(Sub_sp,Year==Years_2$Year[j])
      Yr2$Dead_No<-0
      #Yr2$Dead_BA<-0
      Yr2$Dead_dist<-NA
    }
    Years<-rbind(Years,Yr2)
    
  }
  Years2_2<-rbind(Years,Years2_2)
}

Years2<-rbind(Years2_1,Years2_2)
Years3<-unique(Years2)
head(Years3)


write.csv(Years3,"Data/Dead_size.csv",row.names=F)
