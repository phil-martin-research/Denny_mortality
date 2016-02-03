#this script calculates changes in the basal area, stem density, mean tree size etc. of the Denny wood plots 

library(plyr)
library(reshape)
library(ggplot2)


#load in data on BA and SD
BA<-read.csv("Data/Denny_plots.csv")
BA_clean<-BA[-c(6:8,12:ncol(BA))]
BA_clean<-subset(BA_clean,Block<=50)
BA_melt<-melt(BA_clean,id.vars =c("Block","Year"))
BA_melt$Year<-ifelse(BA_melt$Year==1996,1999,BA_melt$Year)

BA_summary<-ddply(BA_melt,.(Year,variable),summarise,sum_value=(sum(value)),Plot_no=length(Block))
BA_summary<-ddply(BA_summary,.(Year,variable),transform,plot_mean=sum_value/Plot_no)


ggplot(data=BA_summary,aes(x=Year,y=plot_mean,colour=variable))+geom_line()+facet_wrap(~variable,scales = "free_y")

#load in tree data


Trees<-read.csv("Data/Denny_trees_cleaned.csv")
Trees<-subset(Trees,Block<51&Year>1960)
Trees<-subset(Trees,Species=="Q"|Species=="F")

head(Trees)

Trees_summary<-ddply(Trees,.(Year),summarise,m_DBH=median(DBH),BA=(sum((DBH/200)^2)*3.142)/1.84,SD=length(DBH)/1.84)
Trees_melt<-melt(Trees_summary,id.vars = c("Year"))


2*(sqrt(((3000/10000)/3.142)))


(2*(sqrt(3000/3.142)))/100


ggplot(Trees_melt,aes(x=Year,y=value))+geom_line()+facet_wrap(~variable,scales = "free")


ggplot(Trees,aes(x=DBH))+geom_histogram()+facet_grid(~Year)
