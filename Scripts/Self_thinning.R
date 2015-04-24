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


Blocks<-unique(Trees_ddply$Block)
Increase_BA<-NULL
for (i in 1:length(Blocks)){
  Trees_sub<-subset(Trees_ddply,Block==Blocks[i])
  if (((Trees_sub$BA[2]-Trees_sub$BA[1])>0)&((Trees_sub$BA[3]-Trees_sub$BA[1])>0)){
    Increase_BA<-rbind(Trees_sub,Increase_BA)
  }else{
  }
}



#model the relationship between changes in SDM and those in BA
#first put everything on a log scale
Increase_BA$log_BA<-log(Increase_BA$BA)
Increase_BA$log_SDM<-log(Increase_BA$SDM)
Increase_BA$Species<-"All species"

M1<-lmer(log_BA~log_SDM+(log_SDM|Block),data=Increase_BA)
summary(M1)
r.squaredGLMM(M1)


new.data<-data.frame(log_SDM=seq(2.079,4.111,length.out = 500))
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

#plot this predicted relationship
theme_set(theme_bw(base_size=12))
Thin_plot1<-ggplot(Increase_BA,aes(x=SDM,y=BA,shape=as.factor(Year),colour=as.factor(Year),group=Block))+geom_point()+geom_path(colour="black",alpha=0.3,lty=2)+scale_x_log10(limits = c(5,100),breaks=c(1,10,50,100))+scale_y_log10(limits = c(0.5,5),breaks=c(0.1,1,5))
Thin_plot2<-Thin_plot1+geom_line(data=new.data,aes(x=exp(log_SDM),y=exp(log_BA),shape=NULL,group=NULL),colour="black")+geom_ribbon(data=new.data,aes(x=exp(log_SDM),ymin=exp(plo),ymax=exp(phi),y=NULL,shape=NULL,group=NULL),colour=NA,fill="grey",alpha=0.2)
Thin_plot3<-Thin_plot2+scale_color_discrete("Year")+scale_shape_discrete("Year")+ylab(expression(paste("Total subplot basal area (",m^2,")",sep="")))+xlab("Subplot stem density")
Thin_plot4<-Thin_plot3+ annotate("text", x=30, y=4, label="'R'^2*'=0.048'", parse=TRUE)+annotate("text", x=30, y=5, label="log(Basal area)=1.05-0.16 x log(Stem density)",)
Thin_plot4+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
ggsave("Figures/Self_thinning_all.png",height=5,width=7,dpi=1200)

#import data for the major species
Denny_plots<-read.csv("Data/Denny_plots.csv")
head(Denny_plots)

#plot the relationship between stem density and ba for the threee major species
ggplot(Denny_plots,aes(x=FM,y=F_BA))+geom_point()+scale_x_log10()+scale_y_log10()
ggplot(Denny_plots,aes(x=QM,y=Q_BA))+geom_point()+scale_x_log10()+scale_y_log10()
ggplot(Denny_plots,aes(x=IM,y=I_BA))+geom_point()+scale_x_log10()+scale_y_log10()

#on the face of it there seems to be either no relationship or a positive relationship between
#stem density and BA for each species. However we need to look at the dynamics to assess this properly
Denny_plots$Year<-ifelse(Denny_plots$Year==1999,1996,Denny_plots$Year)
Denny_plots<-subset(Denny_plots,Year==1964|Year==1996|Year==2014)
Denny_plots<-subset(Denny_plots,Block!=7)
Blocks<-unique(Denny_plots$Block)

#first beech
Increase_F<-NULL
for (i in 1:length(Blocks)){
  Trees_sub<-subset(Denny_plots,Block==Blocks[i])
  if (((Trees_sub$F_BA[2]-Trees_sub$F_BA[1])>0)&((Trees_sub$F_BA[3]-Trees_sub$F_BA[1])>0)&
        ((Trees_sub$Q_BA[3]-Trees_sub$Q_BA[2])>0)){
    Increase_F<-rbind(Trees_sub,Increase_F)
  }else{
  }
}
Increase_F2<-NULL
Blocks_F<-unique(Increase_F$Block)
for (i in 1:length(Blocks_F)){
  Sub_F<-subset(Increase_F,Block==Blocks_F[i])
  if (((Sub_F$FM[2]-Sub_F$FM[1])<0)&((Sub_F$FM[3]-Sub_F$FM[1])<0)){
    Increase_F2<-rbind(Sub_F,Increase_F2)
  }else{
  }
}

Increase_F2$Species<-"Beech"
length(unique(Increase_F2$Block))
ggplot(Increase_F2,aes(x=FM,y=F_BA,colour=as.factor(Year),group=Block))+geom_point()+geom_path(colour="black",lty=2,alpha=0.2)+scale_x_log10()+scale_y_log10()


#for beech 15 plots have increased in BA and lost stem density
#now use mixed model to work out the slope of the line
Increase_F2$log_BA<-log(Increase_F2$F_BA/25)
Increase_F2$log_SDM<-log(Increase_F2$FM)
M1<-lmer(log_BA~log_SDM+(log_SDM|Block),data=Increase_F2)
summary(M1)
r.squaredGLMM(M1)

summary(Increase_F2)
new.data_F<-data.frame(log_SDM=seq(1.946,3.258,length.out = 500))
new.data_F$log_BA<-0

mm <- model.matrix(terms(M1),new.data_F)
new.data_F$log_BA <- predict(M1,new.data_F,re.form=NA)
pvar1 <- diag(mm %*% tcrossprod(vcov(M1),mm))
tvar1 <- pvar1+VarCorr(M1)$Block[1]  ## must be adapted for more complex models
new.data_F <- data.frame(
  new.data_F
  , plo = new.data_F$log_BA-2*sqrt(pvar1)
  , phi = new.data_F$log_BA+2*sqrt(pvar1)
  , tlo = new.data_F$log_BA-2*sqrt(tvar1)
  , thi = new.data_F$log_BA+2*sqrt(tvar1)
)

new.data_F$Species<-"Beech"

#now for oak
Increase_Q<-NULL
for (i in 1:length(Blocks)){
  Trees_sub<-subset(Denny_plots,Block==Blocks[i])
  if (((Trees_sub$Q_BA[2]-Trees_sub$Q_BA[1])>0)&((Trees_sub$Q_BA[3]-Trees_sub$Q_BA[1])>0)&
        ((Trees_sub$Q_BA[3]-Trees_sub$Q_BA[2])>0)){
    Increase_Q<-rbind(Trees_sub,Increase_Q)
  }else{
  }
}
Increase_Q2<-NULL
Blocks_Q<-unique(Increase_Q$Block)
Increase_I2<-NULL
for (i in 1:length(Blocks_Q)){
  Sub_Q<-subset(Increase_Q,Block==Blocks_Q[i])
  if (((Sub_Q$QM[2]-Sub_Q$QM[1])<0)&((Sub_Q$QM[3]-Sub_Q$QM[1])<0)){
    Increase_Q2<-rbind(Sub_Q,Increase_Q2)
  }else{
  }
}

length(unique(Increase_Q2$Block))
Increase_Q2$Species<-"Oak"
ggplot(Increase_Q2,aes(x=QM,y=Q_BA,colour=as.factor(Year),group=Block))+geom_point()+geom_path(colour="black",lty=2,alpha=0.2)+scale_x_log10()+scale_y_log10()
#and for oak 4 have increased in BA and lost stem density

#now use mixed model to work out the slope of the line
Increase_Q2$log_BA<-log(Increase_Q2$Q_BA/25)
Increase_Q2$log_SDM<-log(Increase_Q2$QM)
M1<-lmer(log_BA~log_SDM+(log_SDM|Block),data=Increase_Q2)
summary(M1)
r.squaredGLMM(M1)

summary(Increase_Q2)
new.data_Q<-data.frame(log_SDM=seq(0.69,1.79,length.out = 500))
new.data_Q$log_BA<-0

mm <- model.matrix(terms(M1),new.data_Q)
new.data_Q$log_BA <- predict(M1,new.data_Q,re.form=NA)
pvar1 <- diag(mm %*% tcrossprod(vcov(M1),mm))
tvar1 <- pvar1+VarCorr(M1)$Block[1]  ## must be adapted for more complex models
new.data_Q <- data.frame(
  new.data_Q
  , plo = new.data_Q$log_BA-2*sqrt(pvar1)
  , phi = new.data_Q$log_BA+2*sqrt(pvar1)
  , tlo = new.data_Q$log_BA-2*sqrt(tvar1)
  , thi = new.data_Q$log_BA+2*sqrt(tvar1)
)
new.data_Q$Species<-"Oak"

#now for holly
Increase_I<-NULL
for (i in 1:length(Blocks)){
  Trees_sub<-subset(Denny_plots,Block==Blocks[i])
  if (((Trees_sub$I_BA[2]-Trees_sub$I_BA[1])>0)&((Trees_sub$I_BA[3]-Trees_sub$I_BA[1])&((Trees_sub$I_BA[3]-Trees_sub$I_BA[2])>0)&
        (sum(Trees_sub$IM>0)))){
    Increase_I<-rbind(Trees_sub,Increase_I)
  }else{
  }
}
Blocks_I<-unique(Increase_I$Block)
Increase_I2<-NULL
for (i in 1:length(Blocks_I)){
     Sub_I<-subset(Increase_I,Block==Blocks_I[i])
    if (((Sub_I$IM[2]-Sub_I$IM[1])<0)&((Sub_I$IM[3]-Sub_I$IM[1])<0)){
      Increase_I2<-rbind(Sub_I,Increase_I2)
    }else{
    }
}
Increase_I2$Species<-"Holly"
length(unique(Increase_I2$Block))
ggplot(Increase_I2,aes(x=IM,y=I_BA,colour=as.factor(Year),group=Block))+geom_point()+geom_path(colour="black",lty=2,alpha=0.2)+scale_x_log10()+scale_y_log10()
#and for Holly 5 plots have increased in BA and lost stem density at the same time
#now model these thinning slopes
Increase_I2$log_BA<-log(Increase_I2$I_BA/25)
Increase_I2$log_SDM<-log(Increase_I2$IM)
Increase_I2<-subset(Increase_I2,Block!=15)
M1<-lmer(log_BA~log_SDM+(log_SDM|Block),data=Increase_I2)
summary(M1)
r.squaredGLMM(M1)

summary(Increase_I2)
new.data_I<-data.frame(log_SDM=seq(min(Increase_I2$log_SDM),max(Increase_I2$log_SDM),length.out = 500))
new.data_I$log_BA<-0

mm <- model.matrix(terms(M1),new.data_I)
new.data_I$log_BA <- predict(M1,new.data_I,re.form=NA)
pvar1 <- diag(mm %*% tcrossprod(vcov(M1),mm))
tvar1 <- pvar1+VarCorr(M1)$Block[1]  ## must be adapted for more complex models
new.data_I <- data.frame(
  new.data_I
  , plo = new.data_I$log_BA-2*sqrt(pvar1)
  , phi = new.data_I$log_BA+2*sqrt(pvar1)
  , tlo = new.data_I$log_BA-2*sqrt(tvar1)
  , thi = new.data_I$log_BA+2*sqrt(tvar1)
)
new.data_I$Species<-"Holly"


#create a plot of all this data
All_preds<-rbind(new.data,new.data_F,new.data_Q,new.data_I)
keeps<-c("Block","Year","log_BA","log_SDM","Species")
Increase_BA<-Increase_BA[keeps]
Increase_F2<-Increase_F2[keeps]
Increase_Q2<-Increase_Q2[keeps]
Increase_I2<-Increase_I2[keeps]
All_increases<-rbind(Increase_BA,Increase_F2,Increase_Q2,Increase_I2)
head(All_preds)

#now plot this
Thinning_plot1<-ggplot(All_increases,aes(x=exp(log_SDM),y=exp(log_BA),group=Block,colour=as.factor(Year)))+geom_point()+geom_path(colour="black",lty=2,alpha=0.2)+facet_wrap(~Species)
Thinning_plot2<-Thinning_plot1+scale_x_log10(limits = c(1,100),breaks=c(1,10,100))+scale_y_log10(limits = c(0.001,10),breaks=c(0.01,0.1,1,10))
Thinning_plot3<-Thinning_plot2+geom_line(data=All_preds,aes(x=exp(log_SDM),y=exp(log_BA),group=Species),colour="black")
Thinning_plot4<-Thinning_plot3+geom_ribbon(data=All_preds,aes(x=exp(log_SDM),y=exp(log_BA),ymin=exp(plo),ymax=exp(phi),group=Species),alpha=0.2,colour=NA)
Thinning_plot5<-Thinning_plot4+scale_color_discrete("Year")+scale_shape_discrete("Year")+ylab(expression(paste("Total subplot basal area (",m^2,")",sep="")))+xlab("Subplot stem density")
Thinning_plot5+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
ggsave("Figures/Self_thinning_all_species.png",height=6,width=8,dpi=1200)
