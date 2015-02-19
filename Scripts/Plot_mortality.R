#script to calculate death and recruitment rate

#import data
Dead<-read.csv("Data/Dead.csv")


#calculate plot level mortality

#n1=N0*(1-m)^t
#t is measurement interval
#m=1-(n1/n0)^1/t
#1-((1-((N0-N1)/N0))^1/t)



YB<-unique(Dead$Block)
DR<-data.frame()
for (i in 1:length(YB)){
  Block_sub<-subset(Dead,Block==YB[i])
  Years<-unique(Block_sub$Year)
  for (j in 2:length(Years)){
  T1<-subset(Block_sub,Year==Years[2-1]&Dead!=1)
  T2<-subset(Block_sub,Year==Years[2]&Dead==0)
  N1<-nrow(T1)
  N2<-nrow(T2)
  T<-Years[2]-Years[1]
  Rate<-1-((1-((N1-N2)/N1))^(1/T))
  
  (T2$Year[]-Block_sub$Year[2-1])
  
    _sub<-data.frame(Block=YB$Block[i],Year=Years[i],Rate=Rate)
  DR<-rbind(DR_sub,DR)
  }
}
row.names(DR)<-NULL
DR2<-as.data.frame(DR)

ggplot(DR,aes(x=Year,y=Rate))+geom_point()


Death_rate<-ddply(Tree_dead3,.(Year,DBH2),summarize,Rate=sum(Dead2)/length(Dead2))

Death_rate$Period<-NA
Death_rate$Period[1]<-20
for (i in 2:nrow(Death_rate)){
  Death_rate$Period[i]<-Death_rate$Year[i]-Death_rate$Year[i-1]
}

Death_rate$Cor_rate<-Death_rate$Rate/Death_rate$Period

ggplot(Death_rate,aes(x=DBH2,y=Rate))+geom_point()+facet_wrap(~Year)

summary(Tree_dead3$Dead2)