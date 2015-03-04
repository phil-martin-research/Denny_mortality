#script to work on Zuur et al example of
#logistic GLMM

rm(list=ls(all=TRUE))

#load packages
library(rms)

#load data
Koalas<-read.table("Data/Zuur_data/Koalas.txt",header = T)

#look at collinearity
cor(Koalas[,6:22],method="spearman")
pairs(Koalas[,8:10])

#reduce collinearity

#highly suitable plus suitable habitat
Koalas$phss_2.5km_new<-Koalas[,"phss_2.5km"]-Koalas[,"phss_5km"]
Koalas$phss_1km_new<-Koalas[,"phss_1km"]-Koalas[,"phss_2.5km"]

#marginal habitat
Koalas$pm_2.5km_new <- Koalas[,"pm_2.5km"] -Koalas[,"pm_5km"]
Koalas$pm_1km_new <- Koalas[,"pm_1km"]-Koalas[,"pm_2.5km"]


#patch density
Koalas$pdens_2.5km_new <- Koalas[,"pdens_2.5km"]-Koalas[,"pdens_5km"]
Koalas$pdens_1km_new <- Koalas[,"pdens_1km"]-Koalas[,"pdens_2.5km"]

#edge density
Koalas$edens_2.5km_new <- Koalas[,"edens_2.5km"]-Koalas[,"edens_5km"]
Koalas$edens_1km_new <- Koalas[,"edens_1km"]-Koalas[,"edens_2.5km"]


#road density
Koalas$rdens_2.5km_new <- Koalas[,"rdens_2.5km"]-Koalas[,"rdens_5km"]
Koalas$rdens_1km_new <- Koalas[,"rdens_1km"]-Koalas[,"rdens_2.5km"]


#correlation has been reduced
cor(Koalas[,c("phss_5km","phss_2.5km_new","phss_1km_new")],method="spearman")
str(Koalas)
pairs(Koalas[,c(8,23:24)])

#######################################################
#create glm model for koala presence or absence########
#######################################################

#at the 5km scale
Glm_5km<-glm(presence~pprim_ssite+psec_ssite+phss_5km+phss_2.5km_new+phss_1km_new+
               pm_5km+pm_2.5km_new+pm_1km_new+pdens_5km+pdens_2.5km_new+pdens_1km_new+
               rdens_5km+rdens_2.5km+rdens_1km_new,data=Koalas,family=binomial)
vif(Glm_5km)

#at the 2.5km scale
Glm2.5km<-glm(presence~pprim_ssite+psec_ssite+phss_2.5km+phss_1km_new+
                pm_2.5km+pm_1km_new+pdens_2.5km+pdens_1km_new+
                rdens_2.5km+rdens_1km_new,data=Koalas,family=binomial)
vif(Glm2.5km)
#at the 1km scale
Glm1km<-glm(presence~pprim_ssite+psec_ssite+phss_1km+
                pm_1km+pdens_1km+rdens_2.5km+rdens_1km_new,
                data=Koalas,family=binomial)
vif(Glm1km)

#look at spatial autocorrelation in the data
library(ncf)

Correlog<-spline.correlog(x=Koalas[,"easting"],y=Koalas[,"northing"],
                          z=Koalas[,"presence"],xmax=10000)
plot.spline.correlog(Correlog)

#look at spatial autocorrelation in the residuals
Correlog_Glm_5km<-spline.correlog(x=Koalas[,"easting"],y=Koalas[,"northing"],
                                  z=residuals(Glm_5km,type="pearson"),xmax=10000)
plot.spline.correlog(Correlog_Glm_5km)

###################################
#produce glmm at the 5km scale#####
###################################

library(glmmML)
Glmm_5km<-glmmML(presence~pprim_ssite+psec_ssite+phss_5km+phss_2.5km_new+phss_1km_new+
               pm_5km+pm_2.5km_new+pm_1km_new+pdens_5km+pdens_2.5km_new+pdens_1km_new+
               rdens_5km+rdens_2.5km+rdens_1km_new,cluster=site,data=Koalas,family=binomial)
Correlog.Glmm_5km<-spline.correlog(x=Koalas[,"easting"],y=Koalas[,"northing"],
                          z=pres.glmmML(model=Glmm_5km, data=Koalas),xmax=10000)

plot.spline.correlog(Correlog.Glmm_5km)

#standardise explanatory variables by centreing and dividing by the sd
Koalas_St<-cbind(Koalas[,1:5],
            apply(X=Koalas[,6:ncol(Koalas)],
                  MARGIN=2, FUN=function(x) {(x-mean(x)/sd(x))}))

#fit one of the GLMM models

M1<-glmmML(presence~pprim_ssite+psec_ssite+phss_1km+pm_1km,cluster=site,
       data=Koalas_St,family=binomial)
summary(M1)

#visualise partial residuals

