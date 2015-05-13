#script to look at evidence for self thinning in Denny

#import data
rm(list=ls(all=TRUE))
Trees<-read.csv("Data/Denny_trees_cleaned.csv")

#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)
library(scales)
library(reshape)
library(grid)

std <- function(x) sd(x)/sqrt(length(x))

#set year as 1996 where it is 1999
Trees$Year<-ifelse(Trees$Year==1999,1996,Trees$Year)
Trees<-subset(Trees,Year==1964|Year==1996|Year==2014)
Trees<-subset(Trees,Block!=25|Block!=26)

#write a loop to show stem dynamics for each time period
Trees_ddply<-ddply(Trees,.(Year,Block),summarise,BA=sum(((DBH)^2*(pi/4))/10000),SDM=length(Tree_ID))
Trees_ddply<-subset(Trees_ddply,Block!=26)
Trees_ddply<-subset(Trees_ddply,Block!=25)



#model the relationship between changes in SDM and those in BA
#first put everything on a log scale
Trees_ddply$log_BA<-log(Trees_ddply$BA)
Trees_ddply$log_SDM<-log(Trees_ddply$SDM)
Increase_BA$Species<-"All species"

M1<-lmer(log_BA~log_SDM+(log_SDM|Block),data=Trees_ddply)
M3_2<-lmer(log_BA~offset((-3/2)*log_SDM)+(log_SDM|Block),data=Trees_ddply)

M_list<-list(M1,M3_2)
M_sel<-model.sel(M_list)
M_sel$r_sq<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M3_2)[1])


summary(Trees_ddply)

new.data<-data.frame(log_SDM=seq(0,4.357,length.out = 500))
new.data$log_BA<-0

mm <- model.matrix(terms(M1),new.data)
new.data$log_BA <- predict(M1,new.data,re.form=NA)
pvar1 <- diag(mm %*% tcrossprod(vcov(M1),mm))
tvar1 <- pvar1+VarCorr(M1)$Block[1]  ## must be adapted for more complex models
new.data <- data.frame(
  new.data
  , plo = new.data$log_BA-2*sqrt(pvar1)
  , phi = new.data$log_BA+2*sqrt(pvar1)
  , tlo = new.data$log_BA-2*sqrt(tvar1)
  , thi = new.data$log_BA+2*sqrt(tvar1)
)

new.data$Species<-"All species"

ddply(Trees_ddply,.(Block),summarise,Counts=length(SDM))

#plot this predicted relationship
theme_set(theme_bw(base_size=12))
Thin_plot1<-ggplot(Trees_ddply,aes(x=SDM,y=BA,shape=as.factor(Year),colour=as.factor(Year),group=Block))+geom_point(alpha=0.5)+scale_x_log10()+scale_y_log10()
Thin_plot2<-Thin_plot1+geom_line(data=new.data,aes(x=exp(log_SDM),y=exp(log_BA),shape=NULL,group=NULL),colour="black")+geom_ribbon(data=new.data,aes(x=exp(log_SDM),ymin=exp(plo),ymax=exp(phi),y=NULL,shape=NULL,group=NULL),colour=NA,fill="grey",alpha=0.2)
Thin_plot3<-Thin_plot2+scale_color_discrete("Year")+scale_shape_discrete("Year")+ylab(expression(paste("Total subplot basal area (",m^2,")",sep="")))+xlab("Subplot stem density")
Thin_plot4<-Thin_plot3+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Thin_plot4
ggsave("Figures/Self_thinning_all.png",height=5,width=7,dpi=1200)

#now do the same again for trees <10cm
Trees<-subset(Trees,DBH<=10)
Trees_ddply<-ddply(Trees,.(Year,Block),summarise,BA=sum(((DBH)^2*(pi/4))/10000),SDM=length(Tree_ID))
Trees_ddply<-subset(Trees_ddply,Block!=26)
Trees_ddply<-subset(Trees_ddply,Block!=25)

#model the relationship between changes in SDM and those in BA
#first put everything on a log scale
Trees_ddply$log_BA<-log(Trees_ddply$BA)
Trees_ddply$log_SDM<-log(Trees_ddply$SDM)

M1<-lmer(log_BA~log_SDM+(log_SDM|Block),data=Trees_ddply)
M3_2<-lmer(log_BA~offset((-3/2)*log_SDM)+(log_SDM|Block),data=Trees_ddply)

r.squaredGLMM(M1)
r.squaredGLMM(M3_2)

summary(Trees_ddply)
new.data<-data.frame(log_SDM=seq(0,4.708,length.out = 500))
new.data$log_BA<-0

mm <- model.matrix(terms(M1),new.data)
new.data$log_BA <- predict(M1,new.data,re.form=NA)
pvar1 <- diag(mm %*% tcrossprod(vcov(M1),mm))
tvar1 <- pvar1+VarCorr(M1)$Block[1]  ## must be adapted for more complex models
new.data <- data.frame(
  new.data
  , plo = new.data$log_BA-2*sqrt(pvar1)
  , phi = new.data$log_BA+2*sqrt(pvar1)
  , tlo = new.data$log_BA-2*sqrt(tvar1)
  , thi = new.data$log_BA+2*sqrt(tvar1)
)



#plot this predicted relationship
theme_set(theme_bw(base_size=12))
Thin_plot1<-ggplot(Trees_ddply,aes(x=SDM,y=BA,shape=as.factor(Year),colour=as.factor(Year),group=Block))+geom_point(alpha=0.5)+scale_x_log10()+scale_y_log10()
Thin_plot2<-Thin_plot1+geom_line(data=new.data,aes(x=exp(log_SDM),y=exp(log_BA),shape=NULL,group=NULL),colour="black")+geom_ribbon(data=new.data,aes(x=exp(log_SDM),ymin=exp(plo),ymax=exp(phi),y=NULL,shape=NULL,group=NULL),colour=NA,fill="grey",alpha=0.2)
Thin_plot3<-Thin_plot2+scale_color_discrete("Year")+scale_shape_discrete("Year")+ylab(expression(paste("Total subplot basal area (",m^2,")",sep="")))+xlab("Subplot stem density")
Thin_plot4<-Thin_plot3+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Thin_plot4
