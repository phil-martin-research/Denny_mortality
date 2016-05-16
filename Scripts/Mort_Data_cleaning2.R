rm(list=ls(all=TRUE))

#script to produce clean data for analysis of mortality

library(plyr)
library(ggplot2)
library(MuMIn)
library(sp)
library(spatstat)
library(dplyr)


#import data
DBH<-read.csv("Data/Denny_trees_cleaned.csv",colClasses=c("Tree_ID"="character"))
#subset trees to give only those inside plots and for years after 1960
#also remove data from the short transect which was surveyed in 1999
#and for which plots have a higher number than 51
DBH<-subset(DBH,In_out=="In")
DBH<-subset(DBH,Year>1960)
DBH<-subset(DBH,!Block>51&Year!=1999)

#produce a list of distinct trees and give them new
#easier to work with ID numbers
IDs<-distinct(DBH[c("Tree_ID","Block")])
colnames(IDs)<-c("ID1","Block")
IDs$ID2<-as.numeric(row.names(IDs))

#now give the easch tree in the DBH dataframe an ID
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

#merge together data from dataframe of trees and tree grid
#to give a record of each tree for each year, even when the
#tree was not yet present (i.e. prior to recruitment)
#or after the tree was dead
Tree_grid1<-merge(Grid_bind,Tree_grid,by=c("ID2","Year"),all=T)
head(Tree_grid1)
Tree_grid2<-merge(Tree_grid1,IDs,by=c("ID2"),all=T)


#drop coulmns that are no longer needed and rename the columns to make more sense
keeps<-c("Block","ID2","Year","DBH.x","BA.x","Dead.x")
Tree_grid2<-Tree_grid2[keeps]
colnames(Tree_grid2)<-c("Block","ID2","Year","DBH","BA","Dead")
Tree_grid2$Dead<-ifelse(Tree_grid2$Dead==1,0,1)

#work out whether trees are dead
#run a loop for each tree to look at rows above
#if any of the rows indicate that the tree is alive at
#a previous time, but it is not present or dead now 
#classify it as dead, otherwise classify it as alive

Tree_IDs<-unique(Tree_grid2$ID2)
Tree_dead<-NULL
for (i in 1:length(Tree_IDs)){
  Tree_sub2<-subset(Tree_grid2,ID2==Tree_IDs[i])
  Tree_sub2$Dead2<-NA
  Tree_sub2$Dead2[1]<-Tree_sub2$Dead[1]
  if (nrow(Tree_sub2)>1){
  for (y in 2:nrow(Tree_sub2)){
    if (is.na(Tree_sub2$Dead[y])&&is.na(Tree_sub2$Dead[y-1])==T){
      #if tree is not present in dataset yet (i.e. not dead but not alive) then code it 
      #as NA.
      Tree_sub2$Dead2[y]<-NA
    }
    #if tree died at previous time point, code as NA
    else if (((sum(Tree_sub2$Dead[1:(y-1)],na.rm = T))>0)&&(is.na(Tree_sub2$Dead[y]))){
    Tree_sub2$Dead2[y]<-NA
    }
    #if tree was not dead at previous time point but is no longer present
    #code it as dead (i.e.=1)
    else if ((sum(Tree_sub2$Dead[1:(y-1)],na.rm = T)==0)&&(is.na(Tree_sub2$Dead[y]))){
      Tree_sub2$Dead2[y]<-1
    } 
    #if tree was not present before and is now present and alive
    #code this as alive
    else if (is.na(Tree_sub2$Dead[y-1])&&(Tree_sub2$Dead[y]==0)==T){
      Tree_sub2$Dead2[y]<-0
    }
    #if tree is coded as dead keep this code
    else if (Tree_sub2$Dead[y]==1){
      Tree_sub2$Dead2[y]<-1
    }else {
    Tree_sub2$Dead2[y]<-0
    }
  }
}else{#if there are no rows for the tree data create a dataframe and mark presence as NA
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
    Tree_sub2<-data.frame(Block=Tree_sub2$Block[1],ID2=Tree_sub2$ID2[1],Year=Y,DBH=NA,BA=NA,Dead=1,Dead2=1)
  }
}
  #bind all the data together
Tree_dead<-rbind(Tree_sub2,Tree_dead)
}
#order the data by their ID number and the year of survey
Tree_dead<-Tree_dead[with(Tree_dead, order(ID2,Year)), ]
tail(Tree_dead,n = 50)

#now add a column to identify dead trees to work out
#cumulative number of dead trees over time and space

#first produce an index with unique tree IDs
Tree_IDs<-unique(Tree_grid2$ID2)
#create a null dataframe to be used later for the data from the loop
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

write.csv(Tree_dead2,"Data/For_mort_rates.csv",row.names=F)

