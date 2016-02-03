#script to calculate growth rates of beech trees

library(ggplot2)

GR<-read.csv("Data/Beech_growth_rates.csv")

GR<-(GR)[-c(1:3)]
colnames(GR)<-c("DBH","Age")
GR_clean<-GR[complete.cases(GR),]


plot(GR$Age,GR$DBH)

M0<-lm(DBH~1,data=GR_clean)
M1<-lm(DBH~Age,data=GR_clean)
summary(M1)
par(mfrow=c(2,2))
plot(M1)


new.data<-data.frame(Age=seq(from=min(GR_clean$Age),to=max(GR_clean$Age),by = 1),DBH=NA)
new.data$DBH<-predict(M1,newdata=new.data)


Plot_1<-ggplot(data=GR_clean,aes(x=Age,y=DBH))+geom_point()
Plot_2<-Plot_1+geom_line(data=new.data,aes(x=Age,y=DBH))
