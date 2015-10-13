#script to look at seedling density related to canopy in Paul's plots

library(ggplot2)
library(lme4)
library(MuMIn)
library(gridExtra)

#load in data
Seedlings<-read.csv("Data/Phil_Data_Seedlings_Saplings.csv")

#models of seedling abundance over gradient of canopy openness
Seed0<-glmer(FagusSeedlings~1+(1|Site),family="poisson",data=Seedlings)
Seed1<-glmer(FagusSeedlings~Canopy.Openness.Total+(1|Site),family="poisson",data=Seedlings)

plot(Seedlings$Canopy.Openness.Total,exp(predict(Seed1,re.form=NA)))

Seedlings$pred<-exp(predict(Seed1,re.form=NA))


#models of sapling abundance over gradient of canopy openness
Sap0<-glmer(FagusSaplings~1+(1|Site),family="poisson",data=Seedlings)
Sap1<-glmer(FagusSaplings~Canopy.Openness.Total+(1|Site),family="poisson",data=Seedlings)

Seedlings$pred2<-exp(predict(Sap1,re.form=NA))

plot(Seedlings$Canopy.Openness.Total,exp(predict(Sap1,re.form=NA)))
plot(Seedlings$SBA,Seedlings$Canopy.Openness.Total)


theme_set(theme_bw(base_size=12))
Seedlings_p1<-ggplot(Seedlings, aes(x=Canopy.Openness.Total,y=FagusSeedlings))+geom_point(shape=1)+geom_line(aes(group=NULL,y=pred),colour="black")
Seedlings_p2<-Seedlings_p1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Seedlings_p3<-Seedlings_p2+ylab(expression(paste("Beech seedling density", subplot^-1,sep=" ")))+xlab("Canopy openness")+ annotate("text", x = 0, y = 70, label = "(a)")


theme_set(theme_bw(base_size=12))
Saplings_p1<-ggplot(Seedlings, aes(x=Canopy.Openness.Total,y=FagusSaplings))+geom_point(shape=1)+geom_line(aes(group=NULL,y=pred2),colour="black")
Saplings_p2<-Saplings_p1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Saplings_p3<-Saplings_p2+ylab(expression(paste("Beech sapling density", subplot^-1,sep=" ")))+xlab("Canopy openness")+ annotate("text", x = 0, y = 6, label = "(b)")

png("Figures/Seedling_sapling.png",height=4,width=8,res = 800,units = "in")
grid.arrange(Seedlings_p3,Saplings_p3,ncol=2)
dev.off()
