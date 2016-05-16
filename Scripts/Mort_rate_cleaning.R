rm(list=ls(all=TRUE))

#script to produce clean data for analysis of mortality rates

library(plyr)
library(ggplot2)
library(MuMIn)
library(sp)
library(spatstat)
library(dplyr)


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

head(DBH_ID)

#need to create a loop that compares teh number of stems at time 1 to the number of stems (that were present in time 1)
#at time 2




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
