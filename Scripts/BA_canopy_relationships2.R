rm(list=ls(all=TRUE))

library(ggplot2)
library(plyr)
library(lme4)
library(MuMIn)

Can_BA<-read.csv("Data/Phil_Data_Seedlings_Saplings.csv")
head(Can_BA)
ggplot(Can_BA,aes(x=SBA,y=Can_trans))+geom_point()

Can_BA$Can_trans<-qlogis(Can_BA$Canopy.Openness.Total/100)
Can_BA$SBA_std<-(Can_BA$SBA-mean(Can_BA$SBA))/sd(Can_BA$SBA)

#now produce models of relationship between stand basal area and canopy openness
M0<-lmer(Can_trans~1+(1|Site),data=Can_BA)
M1<-lmer(Can_trans~SBA_std+(1|Site),data=Can_BA)
summary(M1)

df<-data.frame(SBA=seq(0,100,0.1))
df$SBA_std<-(df$SBA-mean(Can_BA$SBA))/sd(Can_BA$SBA)
df$Can_open<-plogis(predict(M1,newdata=df,re.form=NA))*100

theme_set(theme_bw(base_size=12))
P1<-ggplot(data=Can_BA,aes(x=SBA,y=Canopy.Openness.Total))+geom_point(shape=1,size=2)
P2<-P1+geom_line(data=df,aes(x=SBA,y=Can_open))+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1,colour="black",fill=NA))
P3<-P2+xlab(expression(paste("Stand basal area ( ",m^2," ",ha^-1,")")))+ylab("Stand canopy openness (%)")
P3
ggsave("Figures/SBA_Canopy.png",height=4,width=6,dpi=800,units="in")
