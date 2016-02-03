#script to look at forest structure to allow comparisons with individual based model

library(ggplot2)
library(plyr)

#read in data on trees in Denny
Trees<-read.csv("data/Denny_trees_cleaned.csv")
Trees_10<-subset(Trees,DBH>=10&Status!=0&Block<=50)
Trees_10$Year<-ifelse(Trees_10$Year==1999,1996,Trees_10$Year)
Trees_10$BA<-((Trees_10$DBH/200)^2)*pi

#histogram of the size structure of the forest
ggplot(data=Trees,aes(x=DBH))+geom_histogram(binwidth=10)+facet_wrap(~Year)

ddply(Trees_10,.(Year),summarise,Block_count=length(unique(Block)),tree_size=mean(DBH),BA=(sum(((DBH/200)^2)*pi)/2.54))


ddply(Trees,.(Year),summarise,Tree_count=sum(Status,na.rm = T)/1.84,tree_size=mean(DBH),BA=(sum(((DBH/200)^2)*pi)/1.84))

BA_plots<-ddply(Trees,.(Block,Year),summarise,Tree_count=sum(Status,na.rm = T)/2.54,tree_size=mean(DBH),BA=sum(((((DBH/200)^2)*pi))/0.04))
ddply(BA_plots,.(Year),summarise,max_BA=max(BA))

