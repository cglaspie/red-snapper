# library(parallel)
# detectCores(all.tests = FALSE, logical = TRUE)
# library(stringr)
# library(data.table)
# library(dplyr)
# library(raster)
library(mgcv) #Need
# library(colorRamps)
# library(fields)
# library(mapdata)
# library(akima)
# library(raster)
# library(tidyverse)
# library(sp)
# library(shape)
# library(rgdal)
# library(rgeos)
# library(rmapshaper)
# library(gstat)
# library(RColorBrewer)
# library(viridis)
# library(combinat)
# library(MASS)
# library(mvtnorm)
library(ecp) ## Need


########################################################################

#GLM/GAM code for joined fishy-enviro data

#SEAMAP DATA UNITS:
#TEMPMID: degrees C [1 decimal point]
#SALMID: psu [1 decimal point]
#SURF.CHLOR: mg/m^3
#BOT.OXY: ppm
#LAT/LON: decimal degrees

# dat<-read.csv("Red_SnapperCPUE_pipeline_artreef_platform_seabed.csv")
# 
# dat[dat==-9999]<-NA
# 
# fishdata=data.frame("YEAR"=dat$year,"month"=dat$month,"LATITUDE"=dat$DECSLAT,
#                     "LONGITUDE"=dat$DECSLON,"SURF.TEMP"=dat$TEMPSURF,"BOT TEMP"=dat$TEMPMAX,
#                     "BOT.SAL"=dat$SALMAX,"SURF.CHLOR"=dat$CHLORSURF,"BOT.OXY"=dat$OXYMAX,
#                     "effort"=dat$MIN_FISH,"count"=dat$CNTEXP,
#                     "rscpue"=dat$CPUE,"BATHYMETRY"=dat$bathy,"RUGOSITY"=dat$rugosity,
#                     "PLATFORM"=dat$n.platform,"PIPELINE"=dat$pipeline,
#                     "GRAVEL"=dat$gravel,"MUD"=dat$mud,"ROCK"=dat$rock,
#                     "SAND"=dat$sand,"CARBON"=dat$carbon,"REEF"=dat$n.art.reef)
# fishdata$count[is.na(fishdata$count)]=0
# fishdata$GRAVEL[is.na(fishdata$GRAVEL)]=-9999
# fishdata$MUD[is.na(fishdata$MUD)]=-9999
# fishdata$ROCK[is.na(fishdata$ROCK)]=-9999
# fishdata$SAND[is.na(fishdata$SAND)]=-9999
# fishdata$SURF.CHLOR[is.na(fishdata$SURF.CHLOR)]=-999
# 
# 
# 
# nrow(fishdata) #22638
# summary(fishdata)
# 
# fishenvdata=na.omit(fishdata)
# nrow(fishenvdata) #19052
# 
# fishenvdata[fishenvdata==-999]<-NA
# fishenvdata[fishenvdata==-9999]<-0
# 
# summary(fishenvdata)
# 
# fishenvdata=na.omit(fishenvdata)
# nrow(fishenvdata) #17472
# 
# fishenvdata$BATHYMETRY[fishenvdata$BATHYMETRY<0]=-1*fishenvdata$BATHYMETRY[fishenvdata$BATHYMETRY<0]
# 
# fishenvdata$SURF.CHLOR[fishenvdata$SURF.CHLOR<=0]=0.01
# fishenvdata$SURF.CHLOR=log(fishenvdata$SURF.CHLOR)
# fishenvdata$RUGOSITY=log(fishenvdata$RUGOSITY+0.1)
# fishenvdata$GRAVEL[fishenvdata$GRAVEL<0]=0
# fishenvdata$ROCK[fishenvdata$ROCK<0]=0
# 
# fishenvdata$PA=0
# fishenvdata$PA[fishenvdata$rscpue>0]=1
# 
# summerdata=fishenvdata[fishenvdata$month==6|fishenvdata$month==7,]
# falldata=fishenvdata[fishenvdata$month==10|fishenvdata$month==11,]
# 
# summary(summerdata)
# summary(falldata)
# 
# summerdata=summerdata[summerdata$BOT.TEMP>=15,]
# summerdata=summerdata[summerdata$BOT.SAL>=25,]
# falldata=falldata[falldata$BOT.TEMP>=15,]
# falldata=falldata[falldata$BOT.SAL>=25,]
# 
# falldata$logrscpue=log((falldata$rscpue*60)+1)
# 
# falldata$rand=sample(1:10000, nrow(falldata))
# falldata=falldata[order(falldata$rand),]
# falldata$Validate=rep("TRAIN",nrow(falldata))
# falldata$Validate[1:(nrow(falldata)/10)]="TEST"
# falldatatrain=falldata[falldata$Validate=="TRAIN",]
# 
# falldataonlypos=falldatatrain[falldatatrain$rscpue>0,]
# 
# summerdata$logrscpue=log((summerdata$rscpue*60)+1)
# 
# summerdata$rand=sample(1:10000, nrow(summerdata))
# summerdata=summerdata[order(summerdata$rand),]
# summerdata$Validate=rep("TRAIN",nrow(summerdata))
# summerdata$Validate[1:(nrow(summerdata)/10)]="TEST"
# 
# summerdatatrain=summerdata[summerdata$Validate=="TRAIN",]
# 
# summerdataonlypos=summerdatatrain[summerdatatrain$rscpue>0,]
# 
# fallcorrfishvars<-cor(falldatatrain[5:22],y=NULL,use="pairwise.complete.obs",method=c("pearson","kendall","spearman"))
# # write.csv(fallcorrfishvars,"corr_fall.csv")
# 
# summercorrfishvars<-cor(summerdatatrain[5:22],y=NULL,use="pairwise.complete.obs",method=c("pearson","kendall","spearman"))
# # write.csv(summercorrfishvars,"corr_summer.csv")
# 
# # write.csv(falldata,"falldata.csv")
# # write.csv(summerdata,"summerdata.csv")

falldata=read.csv("falldata.csv")
summerdata=read.csv("summerdata.csv")

summerdatatrain=summerdata[summerdata$Validate=="TRAIN",]
falldatatrain=falldata[falldata$Validate=="TRAIN",]

falldataonlypos=falldatatrain[falldatatrain$rscpue>0,]
summerdataonlypos=summerdatatrain[summerdatatrain$rscpue>0,]

###########################################################################################################
#                                                CHANGEPOINT
###########################################################################################################

# stppData1=data.frame("YR"=falldata$YEAR,"rscpue"=falldata$rscpue,"lat"=falldata$LATITUDE,"lon"=falldata$LONGITUDE)
# stppData1=stppData1[order(stppData1$YR,stppData1$lat,stppData1$lon),]
# stppData1=as.matrix(stppData1)
# member <- as.numeric(cut(stppData1[, 1], breaks = seq(1986, 2021, by = 1)))
# pen <- function(cp) -length(cp)
# output1 <- e.agglo(X = stppData1[,c(1,4)], member = member, alpha = 1,penalty=pen)
# fall.clusters1=cbind(output1$cluster,stppData1[,1])
# 
# cluster=fall.clusters1[,1]
# cl.years=fall.clusters1[,2]
# 
# jpeg("ClusterFall.jpg",width = 3, height = 3,units="in",res=300)
# 
# plot(cluster,cl.years,xlab="Cluster",ylab="Year")
# 
# dev.off()
# 
# unique(cl.years[cluster==1])
# unique(cl.years[cluster==2])
# unique(cl.years[cluster==3])
# 
# stppData2=data.frame("YR"=summerdata$YEAR,"rscpue"=summerdata$rscpue,"lat"=summerdata$LATITUDE,"lon"=summerdata$LONGITUDE)
# stppData2=stppData2[order(stppData2$YR,stppData2$lat,stppData2$lon),]
# stppData2=as.matrix(stppData2)
# member <- as.numeric(cut(stppData2[, 1], breaks = seq(1986, 2021, by = 1)))
# pen <- function(cp) -length(cp)
# output2 <- e.agglo(X = stppData2[,c(1,4)], member = member, alpha = 1,penalty=pen)
# summer.clusters2=cbind(output2$cluster,stppData2[,1])
# 
# cluster2=summer.clusters2[,1]
# cl.years2=summer.clusters2[,2]
# 
# jpeg("ClusterSummer.jpg",width = 3, height = 3,units="in",res=300)
# 
# plot(cluster2,cl.years2,xlab="Cluster",ylab="Year")
# 
# dev.off()
# 
# unique(cl.years2[cluster2==1])
# unique(cl.years2[cluster==2])
# unique(cl.years[cluster==3])

###########################################################################################################
#                                         GAM Model
###########################################################################################################

# fall.full.interactive.modelgaus<-bam(logrscpue~s(YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(SURF.TEMP,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(BOT.TEMP,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(SURF.CHLOR,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(BOT.OXY,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(falldataonlypos$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(GRAVEL,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(ROCK,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(SAND,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(CARBON,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(REEF,YEAR,k=round(0.8*length(unique(falldataonlypos$REEF)),0))+te(LATITUDE,LONGITUDE,k=50),data=falldataonlypos,family=gaussian(link = "identity"),select=TRUE,discrete=TRUE,nthreads=8)
# fall.full.interactive.modellog<-bam(rscpue~s(YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(SURF.TEMP,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(BOT.TEMP,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(SURF.CHLOR,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(BOT.OXY,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(falldataonlypos$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(GRAVEL,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(ROCK,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(SAND,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(CARBON,YEAR,k=round(0.8*length(unique(falldataonlypos$YEAR)),0))+te(REEF,YEAR,k=round(0.8*length(unique(falldataonlypos$REEF)),0))+te(LATITUDE,LONGITUDE,k=50),data=falldataonlypos,family=gaussian(link = "log"),select=TRUE,discrete=TRUE,nthreads=8)
fall.PA.interactive.modellogit<-bam(PA~s(YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(SURF.TEMP,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(BOT.TEMP,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(SURF.CHLOR,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(BOT.OXY,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(falldatatrain$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(GRAVEL,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(ROCK,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(SAND,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(CARBON,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(REEF,YEAR,k=round(0.8*length(unique(falldatatrain$REEF)),0))+te(LATITUDE,LONGITUDE,k=50),data=falldatatrain,family = binomial(link=logit),select=TRUE,discrete=TRUE,nthreads=8)

# saveRDS(fall.full.interactive.modelgaus,"fall.full.interactive.modelgaus.rds")
# saveRDS(fall.full.interactive.modellog,"fall.full.interactive.modellog.rds")
saveRDS(fall.PA.interactive.modellogit,"fall.PA.interactive.modellogit.rds")
# fall.full.interactive.modelgaus<-readRDS("fall.full.interactive.modelgaus.rds")
# fall.full.interactive.modellog<-readRDS("fall.full.interactive.modellog.rds")
# fall.PA.interactive.modellogit<-readRDS("fall.PA.interactive.modellogit.rds")
# 
# jpeg("Figure 8.jpg",width = 3.75, height = 9.5, units="in",res=300)
# 
# par(mfrow=c(5,2),mar=c(4.1,4.1,2.1,2.1))
# plot(fall.full.interactive.modelgaus,select=2,scheme=2,xlab="Surf. Temp.",main="",ylab="Year")
# plot(fall.full.interactive.modelgaus,select=3,scheme=2,xlab="Bot. Temp.",main="",ylab="Year")
# plot(fall.full.interactive.modelgaus,select=4,scheme=2,xlab="Bot.Sal.",main="",ylab="Year")
# plot(fall.full.interactive.modelgaus,select=5,scheme=2,xlab="Surf. Chlor",main="",ylab="Year")
# plot(fall.full.interactive.modelgaus,select=7,scheme=1,xlab="Bathymetry",ylab="Additive Effect")
# plot(fall.full.interactive.modelgaus,select=8,ylim=c(-0.4,0.4),scheme=1,xlab="Rugosity",ylab="Additive Effect")
# plot(fall.full.interactive.modelgaus,select=9,scheme=2,xlab="Platform",main="",ylab="Year")
# plot(fall.full.interactive.modelgaus,select=10,scheme=2,xlab="Pipeline",main="",ylab="Year")
# plot(fall.full.interactive.modelgaus,select=14,scheme=1,xlab="Oyster",ylab="Additive Effect")
# plot(fall.full.interactive.modelgaus,select=17,scheme=2,xlab="Reef",main="",ylab="Year")
# 
# dev.off()
# 
# jpeg("Supp 7.jpg",width = 3.75, height = 7.5, units="in",res=300)
# 
# par(mfrow=c(4,2),mar=c(4.1,4.1,2.1,2.1))
# plot(fall.full.interactive.modelgaus,select=1,scheme=1,ylim=c(-.2,.2),xlab="Year",ylab="Additive Effect")
# plot(fall.full.interactive.modelgaus,select=6,scheme=2,xlab="Bot. Oxy.",main="",ylab="Year")
# plot(fall.full.interactive.modelgaus,select=11,scheme=1,ylim=c(-.2,.2),xlab="Carbonate",ylab="Additive Effect")
# plot(fall.full.interactive.modelgaus,select=12,scheme=1,ylim=c(-.2,.2),xlab="Gravel",ylab="Additive Effect")
# plot(fall.full.interactive.modelgaus,select=13,scheme=1,ylim=c(-.2,.2),xlab="Mud",ylab="Additive Effect")
# plot(fall.full.interactive.modelgaus,select=15,scheme=1,ylim=c(-.2,.2),xlab="Rock",ylab="Additive Effect")
# plot(fall.full.interactive.modelgaus,select=16,scheme=1,ylim=c(-.2,.2),xlab="Sand",ylab="Additive Effect")
# plot(fall.full.interactive.modelgaus,select=18,scheme=2,xlab="Longitude",main="",ylab="Latitude")
# 
# 
# dev.off()
# 
# jpeg("Supp 6.jpg",width = 9, height = 18,units="in",res=300)
# 
# par(mfrow=c(6,3))
# plot(fall.PA.interactive.modellogit,select=1,scheme=1,ylim=c(-.2,.2),xlab="Year",ylab="Additive Effect")
# plot(fall.PA.interactive.modellogit,select=2,scheme=2,xlab="Surf. Temp.",main="",ylab="Year")
# plot(fall.PA.interactive.modellogit,select=3,scheme=2,xlab="Bot. Temp.",main="",ylab="Year")
# plot(fall.PA.interactive.modellogit,select=4,scheme=2,xlab="Bot.Sal.",main="",ylab="Year")
# plot(fall.PA.interactive.modellogit,select=5,scheme=2,xlab="Surf. Chlor",main="",ylab="Year")
# plot(fall.PA.interactive.modellogit,select=6,scheme=2,xlab="Bot. Oxy.",main="",ylab="Year")
# plot(fall.PA.interactive.modellogit,select=7,scheme=1,xlab="Bathymetry",ylab="Additive Effect")
# plot(fall.PA.interactive.modellogit,select=8,ylim=c(-1,1),scheme=1,xlab="Rugosity",ylab="Additive Effect")
# plot(fall.PA.interactive.modellogit,select=9,scheme=2,xlab="Platform",main="",ylab="Year")
# plot(fall.PA.interactive.modellogit,select=10,scheme=2,xlab="Pipeline",main="",ylab="Year")
# plot(fall.PA.interactive.modellogit,select=11,scheme=1,ylim=c(-.4,.4),xlab="Carbonate",ylab="Additive Effect")
# plot(fall.PA.interactive.modellogit,select=12,scheme=1,ylim=c(-.4,.4),xlab="Gravel",ylab="Additive Effect")
# plot(fall.PA.interactive.modellogit,select=13,scheme=1,ylim=c(-.8,.8),xlab="Mud",ylab="Additive Effect")
# plot(fall.PA.interactive.modellogit,select=14,scheme=1,ylim=c(-.4,.4),xlab="Oyster",ylab="Additive Effect")
# plot(fall.PA.interactive.modellogit,select=15,scheme=1,ylim=c(-.2,.2),xlab="Rock",ylab="Additive Effect")
# plot(fall.PA.interactive.modellogit,select=16,scheme=1,ylim=c(-.2,.2),xlab="Sand",ylab="Additive Effect")
# plot(fall.PA.interactive.modellogit,select=17,scheme=2,xlab="Reef",main="",ylab="Year")
# plot(fall.PA.interactive.modellogit,select=18,scheme=2,xlab="Longitude",main="",ylab="Latitude")
# 
# dev.off()
# 
# PA.cc4<-predict.gam(fall.PA.interactive.modellogit,newdata=falldata,type="response",se=TRUE)
# pos.cc4<-predict.gam(fall.full.interactive.modelgaus,newdata=falldata,type="response",se=TRUE)
# falldata$fit=pos.cc4$fit*PA.cc4$fit
# falldata$pos.fit=pos.cc4$fit
# falldata$PA.fit=PA.cc4$fit
# falldata$se.pos=pos.cc4$se.fit
# falldata$se.PA=PA.cc4$se.fit
# 
# plot(falldata$logrscpue[falldata$Validate=="TEST"],falldata$fit[falldata$Validate=="TEST"])
# cor(falldata$logrscpue[falldata$Validate=="TEST"],falldata$fit[falldata$Validate=="TEST"]) # 0.6842458
# 
# lm1=lm(falldata$logrscpue[falldata$Validate=="TEST"]~falldata$fit[falldata$Validate=="TEST"])
# summary(lm1)$r.squared # 0.4681924
# 
# raster.data=data.frame("lon"=falldata$LONGITUDE,"lat"=falldata$LATITUDE,"fit"=pos.cc4$fit*PA.cc4$fit,"pos.fit"=pos.cc4$fit,"PA.fit"=PA.cc4$fit,"se.pos"=pos.cc4$se.fit,"se.PA"=PA.cc4$se.fit)
# 
# write.csv(falldata,"falldata.csv",row.names=FALSE)
# 
# write.csv(raster.data,"falldata.preds.csv",row.names=FALSE)
# 
# falldata.t1=falldata[falldata$YEAR<=1994,]
# nrow(falldata.t1)
# falldata.t2=falldata[falldata$YEAR>1994&falldata$YEAR<=2004,]
# nrow(falldata.t2)
# falldata.t3=falldata[falldata$YEAR>2004,]
# nrow(falldata.t3)
# write.csv(falldata.t1,"falldata.t1.csv")
# write.csv(falldata.t2,"falldata.t2.csv")
# write.csv(falldata.t3,"falldata.t3.csv")

######

# summer.full.interactive.modelgaus<-bam(logrscpue~s(YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(SURF.TEMP,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(SURF.CHLOR,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(BOT.OXY,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(summerdataonlypos$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(GRAVEL,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(ROCK,YEAR,k=round(0.8*length(unique(summerdataonlypos$ROCK)),0))+te(SAND,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(CARBON,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(REEF,YEAR,k=round(0.8*length(unique(summerdataonlypos$REEF)),0))+te(LATITUDE,LONGITUDE,k=50),data=summerdataonlypos,family=gaussian(link = "identity"),select=TRUE,discrete=TRUE,nthreads=8)
summer.full.interactive.modellog<-bam(logrscpue~s(YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(SURF.TEMP,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(SURF.CHLOR,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(BOT.OXY,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(summerdataonlypos$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(GRAVEL,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(ROCK,YEAR,k=round(0.8*length(unique(summerdataonlypos$ROCK)),0))+te(SAND,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(CARBON,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(REEF,YEAR,k=round(0.8*length(unique(summerdataonlypos$REEF)),0))+te(LATITUDE,LONGITUDE,k=50),data=summerdataonlypos,family=gaussian(link = "log"),select=TRUE,discrete=TRUE,nthreads=8)
summer.PA.interactive.modellogit<-bam(PA~s(YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(SURF.TEMP,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(SURF.CHLOR,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(BOT.OXY,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(summerdatatrain$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(GRAVEL,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(ROCK,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(SAND,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(CARBON,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(REEF,YEAR,k=round(0.8*length(unique(summerdatatrain$REEF)),0))+te(LATITUDE,LONGITUDE,k=50),data=summerdatatrain,family = binomial(link=logit),select=TRUE,discrete=TRUE,nthreads=8)

# saveRDS(summer.full.interactive.modelgaus,"summer.full.interactive.modelgaus.rds")
saveRDS(summer.full.interactive.modellog,"summer.full.interactive.modellog.rds")
saveRDS(summer.PA.interactive.modellogit,"summer.PA.interactive.modellogit.rds")

# summer.full.interactive.modelgaus<-readRDS("summer.full.interactive.modelgaus.rds")
# summer.full.interactive.modellog<-readRDS("summer.full.interactive.modellog.rds")
# summer.PA.interactive.modellogit<-readRDS("summer.PA.interactive.modellogit.rds")
# 
# jpeg("Figure 7.jpg",width = 3.75, height = 9.5, units="in",res=300)
# 
# par(mfrow=c(5,2),mar=c(4.1,4.1,2.1,2.1))
# plot(summer.PA.interactive.modellogit,select=2,scheme=2,xlab="Surf. Temp.",main="",ylab="Year")
# plot(summer.PA.interactive.modellogit,select=3,scheme=2,xlab="Bot.Sal.",main="",ylab="Year")
# plot(summer.PA.interactive.modellogit,select=4,scheme=2,xlab="Surf. Chlor",main="",ylab="Year")
# plot(summer.PA.interactive.modellogit,select=5,scheme=2,xlab="Bot. Oxy.",main="",ylab="Year")
# plot(summer.PA.interactive.modellogit,select=6,scheme=1,xlab="Bathymetry",ylab="Additive Effect")
# plot(summer.PA.interactive.modellogit,select=7,ylim=c(-1,1),scheme=1,xlab="Rugosity",ylab="Additive Effect")
# plot(summer.PA.interactive.modellogit,select=8,scheme=2,xlab="Platform",main="",ylab="Year")
# plot(summer.PA.interactive.modellogit,select=9,scheme=2,xlab="Pipeline",main="",ylab="Year")
# plot(summer.PA.interactive.modellogit,select=12,scheme=1,ylim=c(-0.4,0.4),xlab="Mud",ylab="Additive Effect")
# plot(summer.PA.interactive.modellogit,select=16,scheme=2,xlab="Reef",main="",ylab="Year")
# 
# dev.off()
# 
# 
# jpeg("Supp 4.jpg",width = 3.75, height = 7.5, units="in",res=300)
# 
# par(mfrow=c(4,2),mar=c(4.1,4.1,2.1,2.1))
# plot(summer.PA.interactive.modellogit,select=1,scheme=1,ylim=c(-.2,.2),xlab="Year",ylab="Additive Effect")
# plot(summer.PA.interactive.modellogit,select=10,scheme=1,ylim=c(-.2,.2),xlab="Carbonate",ylab="Additive Effect")
# plot(summer.PA.interactive.modellogit,select=11,scheme=1,ylim=c(-.2,.2),xlab="Gravel",ylab="Additive Effect")
# plot(summer.PA.interactive.modellogit,select=13,scheme=1,ylim=c(-.2,.2),xlab="Oyster",ylab="Additive Effect")
# plot(summer.PA.interactive.modellogit,select=14,scheme=1,ylim=c(-.2,.2),xlab="Rock",ylab="Additive Effect")
# plot(summer.PA.interactive.modellogit,select=15,scheme=1,ylim=c(-.2,.2),xlab="Sand",ylab="Additive Effect")
# plot(summer.PA.interactive.modellogit,select=17,scheme=2,xlab="Longitude",main="",ylab="Latitude")
# 
# 
# dev.off()
# 
# jpeg("Supp 5.jpg",width = 9, height = 18,units="in",res=300)
# 
# par(mfrow=c(6,3))
# plot(summer.full.interactive.modelgaus,select=1,scheme=1,ylim=c(-.2,.2),xlab="Year",ylab="Additive Effect")
# plot(summer.full.interactive.modelgaus,select=2,scheme=2,xlab="Surf. Temp.",main="",ylab="Year")
# plot(summer.full.interactive.modelgaus,select=3,scheme=2,xlab="Bot.Sal.",main="",ylab="Year")
# plot(summer.full.interactive.modelgaus,select=4,scheme=2,xlab="Surf. Chlor",main="",ylab="Year")
# plot(summer.full.interactive.modelgaus,select=5,scheme=2,xlab="Bot. Oxy.",main="",ylab="Year")
# plot(summer.full.interactive.modelgaus,select=6,scheme=1,xlab="Bathymetry",ylab="Additive Effect")
# plot(summer.full.interactive.modelgaus,select=7,ylim=c(-1,1),scheme=1,xlab="Rugosity",ylab="Additive Effect")
# plot(summer.full.interactive.modelgaus,select=8,scheme=2,xlab="Platform",main="",ylab="Year")
# plot(summer.full.interactive.modelgaus,select=9,scheme=2,xlab="Pipeline",main="",ylab="Year")
# plot(summer.full.interactive.modelgaus,select=10,scheme=1,ylim=c(-.2,.2),xlab="Carbonate",ylab="Additive Effect")
# plot(summer.full.interactive.modelgaus,select=11,scheme=1,ylim=c(-.2,.2),xlab="Gravel",ylab="Additive Effect")
# plot(summer.full.interactive.modelgaus,select=12,scheme=1,ylim=c(-0.2,0.2),xlab="Mud",ylab="Additive Effect")
# plot(summer.full.interactive.modelgaus,select=13,scheme=1,ylim=c(-.4,.4),xlab="Oyster",ylab="Additive Effect")
# plot(summer.full.interactive.modelgaus,select=14,scheme=1,ylim=c(-.4,.4),xlab="Rock",ylab="Additive Effect")
# plot(summer.full.interactive.modelgaus,select=15,scheme=1,ylim=c(-.2,.2),xlab="Sand",ylab="Additive Effect")
# plot(summer.full.interactive.modelgaus,select=16,scheme=2,xlab="Reef",main="",ylab="Year")
# plot(summer.full.interactive.modelgaus,select=17,scheme=2,xlab="Longitude",main="",ylab="Latitude")
# 
# dev.off()
# 
# 
# PA.cc4<-predict.gam(summer.PA.interactive.modellogit,newdata=summerdata,type="response",se=TRUE)
# pos.cc4<-predict.gam(summer.full.interactive.modelgaus,newdata=summerdata,type="response",se=TRUE)
# summerdata$fit=pos.cc4$fit*PA.cc4$fit
# summerdata$pos.fit=pos.cc4$fit
# summerdata$PA.fit=PA.cc4$fit
# summerdata$se.pos=pos.cc4$se.fit
# summerdata$se.PA=PA.cc4$se.fit
# 
# plot(summerdata$logrscpue[summerdata$Validate=="TEST"],summerdata$fit[summerdata$Validate=="TEST"])
# cor(summerdata$logrscpue[summerdata$Validate=="TEST"],summerdata$fit[summerdata$Validate=="TEST"]) # 0.4852331
# 
# lm1=lm(summerdata$logrscpue[summerdata$Validate=="TEST"]~summerdata$fit[summerdata$Validate=="TEST"])
# summary(lm1)$r.squared # 0.2354511
# 
# raster.data=data.frame("lon"=summerdata$LONGITUDE,"lat"=summerdata$LATITUDE,"fit"=pos.cc4$fit*PA.cc4$fit,"pos.fit"=pos.cc4$fit,"PA.fit"=PA.cc4$fit,"se.pos"=pos.cc4$se.fit,"se.PA"=PA.cc4$se.fit)
# 
# write.csv(summerdata,"summerdata.csv",row.names=FALSE)
# 
# write.csv(raster.data,"summerdata.preds.csv",row.names=FALSE)
# 
# 
# summerdata.t1=summerdata[summerdata$YEAR<=2003,]
# nrow(summerdata.t1)
# summerdata.t2=summerdata[summerdata$YEAR>2003,]
# nrow(summerdata.t2)
# write.csv(summerdata.t1,"summerdata.t1.csv")
# write.csv(summerdata.t2,"summerdata.t2.csv")



###########################################################################################################
#                                               FIGURE 1
###########################################################################################################

# dat1<-read.csv("falldata.csv")
# dat2<-read.csv("summerdata.csv")
# 
# dat3=rbind(dat1,dat2)
# 
# coordinates(dat3)=~LONGITUDE+LATITUDE
# crs(dat3)=crs(shape)
# 
# x=dat3$LONGITUDE
# y=dat3$LATITUDE
# 
# xr <- range(x)
# yr <- range(y)
# b <- round((xr[2]-xr[1])/100,2)
# 
# xmin=min(dat3$LONGITUDE)
# xmax=max(dat3$LONGITUDE)
# ymin=min(dat3$LATITUDE)
# ymax=max(dat3$LATITUDE)
# #
# grd <- expand.grid(x=seq(from=xr[1],to=xr[2],by=b),
#                    y=seq(from=yr[1],to=yr[2],by=b))
# #
# coordinates(grd) <- ~ x+y
# gridded(grd) <- TRUE
# 
# dfgrd <- as.data.frame(grd)
# 
# raster_grd <- rasterFromXYZ(dfgrd)
# 
# ######
# 
# # get the convex hull
# coords <- cbind(dat$LONGITUDE,dat$LATITUDE)
# hull_points <- coords[chull(coords),]
# # convert to polygon
# hull_polygon <- hull_points %>%
#         Polygon() %>%
#         list() %>%
#         Polygons(1) %>%
#         list() %>%
#         SpatialPolygons()
# 
# shape=readOGR(dsn=".",layer="Gulf_of_Mexico_Shoreline")
# 
# crs(hull_polygon)=crs(shape)
# 
# x0=c(-90.59182, -91.43793, -91.68679, -93.67765, -94.35786, -95.28693, -95.96714, -96.36531, -96.18281, -96.13304)
# y0=c(27.99438, 27.92224, 27.83808, 27.82606, 27.77796, 27.69380, 27.52547, 26.94834, 26.40728, 25.95039)
# 
# coords0 <- cbind(x0,y0)
# 
# # convert to polygon
# cut_polygon <- coords0 %>%
#         Polygon() %>%
#         list() %>%
#         Polygons(1) %>%
#         list() %>%
#         SpatialPolygons()
# 
# crs(cut_polygon)=crs(shape)
# 
# hull_polygon=gDifference(hull_polygon,cut_polygon)
# 
# #####
# shape=readOGR(dsn=".",layer="Gulf_of_Mexico_Shoreline")
# 
# pipe=readOGR(dsn=".",layer="ppl_arcs")
# 
# crs(pipe)=crs(shape)
# 
# pipe_subset <- gIntersection(pipe, hull_polygon, byid=TRUE)
# 
# rug.rast=rasterize(dat3,raster_grd,dat3$RUGOSITY,fun=mean)
# 
# platform=readOGR(dsn=".",layer="platform")
# 
# crs(platform)=crs(shape)
# 
# platform_subset <- gIntersection(platform, hull_polygon, byid=TRUE)
# 
# jpeg("FIG 1.jpg",width = 6, height = 4,units="in",res=300)
# 
# par(mar=c(5.1, 4.1, 2.1, 2.1))
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(rug.rast, col=rev(topo.colors(24))[6:24],xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# plot(pipe_subset,col="gray45",lwd=0.5,add=TRUE)
# 
# plot(platform_subset,col="black",pch='.',add=TRUE)
# 
# image.plot(zlim=range(c(0,1.9)),
#            lab.breaks=c("0.0","0.1","0.2","0.3","0.4",
#                         "0.5","0.6","0.7","0.8",
#                         "0.9","1.0","1.1","1.2",
#                         "1.3","1.4","1.5","1.6",
#                         "1.7","1.8","1.9"),
#            col=rev(topo.colors(24))[6:24],legend.only=TRUE,horizontal=TRUE)
# dev.off()
# 
# ###########################################################################################################
# #                                             FISHING MAP
# ###########################################################################################################
# 
# 
# dat1<-read.csv("falldata.csv")
# dat2<-read.csv("summerdata.csv")
# 
# dat3=rbind(dat1,dat2)
# 
# coordinates(dat3)=~LONGITUDE+LATITUDE
# crs(dat3)=crs(shape)
# 
# x=dat3$LONGITUDE
# y=dat3$LATITUDE
# 
# xr <- range(x)
# yr <- range(y)
# b <- round((xr[2]-xr[1])/100,2)
# 
# xmin=min(dat3$LONGITUDE)
# xmax=max(dat3$LONGITUDE)
# ymin=min(dat3$LATITUDE)
# ymax=max(dat3$LATITUDE)
# #
# grd <- expand.grid(x=seq(from=xr[1],to=xr[2],by=b),
#                    y=seq(from=yr[1],to=yr[2],by=b))
# #
# coordinates(grd) <- ~ x+y
# gridded(grd) <- TRUE
# 
# dfgrd <- as.data.frame(grd)
# 
# raster_grd <- rasterFromXYZ(dfgrd)
# 
# ######
# 
# dat3.t1=dat3[summerdata$YEAR<=1995,]
# nrow(dat3.t1)
# 
# dat3.t2=dat3[summerdata$YEAR>1995&summerdata$YEAR<=2005,]
# nrow(dat3.t2)
# 
# dat3.t3=dat3[summerdata$YEAR>2005&summerdata$YEAR<=2010,]
# nrow(dat3.t3)
# 
# dat3.t4=dat3[summerdata$YEAR>2010,]
# nrow(dat3.t4)
# 
# effort.t1 <- rasterize(dat3.t1,raster_grd,dat3.t1$effort,fun='count')
# effort.t2 <- rasterize(dat3.t2,raster_grd,dat3.t2$effort,fun='count')
# effort.t3 <- rasterize(dat3.t3,raster_grd,dat3.t3$effort,fun='count')
# effort.t4 <- rasterize(dat3.t4,raster_grd,dat3.t4$effort,fun='count')
# 
# colfunc <- colorRampPalette(c("white", "darkgreen"))
# 
# shape=readOGR(dsn=".",layer="Gulf_of_Mexico_Shoreline")
# 
# jpeg("Effort.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(effort.t1, col=colfunc(5),breaks=c(0,1,5,10,15,20),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0,25)),
#            lab.breaks=c("0","1","5","10","15",
#                         "20"),
#            col=colfunc(5),legend.only=TRUE,horizontal=TRUE)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(effort.t2, col=colfunc(5),breaks=c(0,1,5,10,15,20),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(effort.t3, col=colfunc(5),breaks=c(0,1,5,10,15,20),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(effort.t4, col=colfunc(5),breaks=c(0,1,5,10,15,20),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# dev.off()
# 
# ###########################################################################################################
# #                                          REL ABUND FIGURE
# ###########################################################################################################
# 
# summerdata=read.csv("summerdata.csv")
# falldata=read.csv("falldata.csv")
# 
# 
# ######
# 
# summerdata.t1=read.csv("summerdata.t1.csv")
# summerdata.t2=read.csv("summerdata.t2.csv")
# summerdata.t3=read.csv("summerdata.t3.csv")
# summerdata.t4=read.csv("summerdata.t4.csv")
# falldata.t1=read.csv("falldata.t1.csv")
# falldata.t2=read.csv("falldata.t2.csv")
# falldata.t3=read.csv("falldata.t3.csv")
# falldata.t4=read.csv("falldata.t4.csv")
# 
# 
# coordinates(falldata.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata.t3) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata.t4) <- ~ LONGITUDE+LATITUDE
# 
# coordinates(summerdata.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata.t3) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata.t4) <- ~ LONGITUDE+LATITUDE
# 
# 
# falldata2<-read.csv("gwr_fall_result.csv")
# summerdata2<-read.csv("gwr_summer_result.csv")
# falldata22<-read.csv("gwr_fall_result.se.csv")
# summerdata22<-read.csv("gwr_summer_result.se.csv")
# 
# 
# summerdata2.t1=summerdata2[summerdata2$YEAR2<=1995,]
# nrow(summerdata2.t1)
# 
# falldata2.t1=falldata2[falldata2$YEAR2<=1995,]
# nrow(falldata2.t1)
# 
# summerdata2.t2=summerdata2[summerdata2$YEAR2>1995&summerdata2$YEAR2<=2005,]
# nrow(summerdata2.t2)
# falldata2.t2=falldata2[falldata2$YEAR2>1995&falldata2$YEAR2<=2005,]
# falldata2.t2=na.omit(falldata2.t2)
# nrow(falldata2.t2)
# 
# summerdata2.t3=summerdata2[summerdata2$YEAR2>2005&summerdata2$YEAR2<=2010,]
# nrow(summerdata2.t3)
# falldata2.t3=falldata2[falldata2$YEAR2>2005&falldata2$YEAR2<=2010,]
# nrow(falldata2.t3)
# 
# summerdata2.t4=summerdata2[summerdata2$YEAR2>2010,]
# nrow(summerdata2.t4)
# falldata2.t4=falldata2[falldata2$YEAR2>2010,]
# nrow(falldata2.t4)
# 
# coordinates(falldata2.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata2.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata2.t3) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata2.t4) <- ~ LONGITUDE+LATITUDE
# 
# coordinates(summerdata2.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata2.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata2.t3) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata2.t4) <- ~ LONGITUDE+LATITUDE
# 
# summerdata22.t1=summerdata22[summerdata22$YEAR2<=1995,]
# nrow(summerdata22.t1)
# 
# falldata22.t1=falldata22[falldata22$YEAR2<=1995,]
# nrow(falldata22.t1)
# 
# summerdata22.t2=summerdata22[summerdata22$YEAR2>1995&summerdata22$YEAR2<=2005,]
# nrow(summerdata22.t2)
# falldata22.t2=falldata22[falldata22$YEAR2>1995&falldata22$YEAR2<=2005,]
# falldata22.t2=na.omit(falldata22.t2)
# nrow(falldata22.t2)
# 
# summerdata22.t3=summerdata22[summerdata22$YEAR2>2005&summerdata22$YEAR2<=2010,]
# nrow(summerdata22.t3)
# falldata22.t3=falldata22[falldata22$YEAR2>2005&falldata22$YEAR2<=2010,]
# nrow(falldata22.t3)
# 
# summerdata22.t4=summerdata22[summerdata22$YEAR2>2010,]
# nrow(summerdata22.t4)
# falldata22.t4=falldata22[falldata22$YEAR2>2010,]
# nrow(falldata22.t4)
# 
# coordinates(falldata22.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata22.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata22.t3) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata22.t4) <- ~ LONGITUDE+LATITUDE
# 
# coordinates(summerdata22.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata22.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata22.t3) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata22.t4) <- ~ LONGITUDE+LATITUDE
# 
# ######
# 
# 
# jpeg("new.gam.Fall.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# f1_raster <- rasterize(falldata.t1,raster_grd,falldata.t1$fit,fun=mean)
# #
# crs(f1_raster)=crs(shape)
# 
# values(f1_raster)[values(f1_raster)<0]=0
# values(f1_raster)[values(f1_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(f1_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #########################################################################
# 
# 
# 
# #
# 
# f2_raster <- rasterize(falldata.t2,raster_grd,falldata.t2$fit,fun=mean)
# #
# crs(f2_raster)=crs(shape)
# 
# values(f2_raster)[values(f2_raster)<0]=0
# values(f2_raster)[values(f2_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(f2_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# f3_raster <- rasterize(falldata.t3,raster_grd,falldata.t3$fit,fun=mean)
# #
# crs(f3_raster)=crs(shape)
# 
# values(f3_raster)[values(f3_raster)<0]=0
# values(f3_raster)[values(f3_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(f3_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0,1.5)),
#            lab.breaks=c("0.00","0.10","0.20","0.30","0.40",
#                         "0.50","0.60","0.70","0.80",
#                         "0.90","1.00","1.10","1.20",
#                         "1.30","1.40","1.50"),
#            col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),legend.only=TRUE,horizontal=TRUE)
# 
# 
# #
# f4_raster <- rasterize(falldata.t4,raster_grd,falldata.t4$fit,fun=mean)
# #
# crs(f4_raster)=crs(shape)
# 
# values(f4_raster)[values(f4_raster)<0]=0
# values(f4_raster)[values(f4_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(f4_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# dev.off()
# 
# 
# jpeg("new.gam.Summer.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# s1_raster <- rasterize(summerdata.t1,raster_grd,summerdata.t1$fit,fun=mean)
# #
# crs(s1_raster)=crs(shape)
# 
# values(s1_raster)[values(s1_raster)<0]=0
# values(s1_raster)[values(s1_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(s1_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# s2_raster <- rasterize(summerdata.t2,raster_grd,summerdata.t2$fit,fun=mean)
# #
# crs(s2_raster)=crs(shape)
# 
# values(s2_raster)[values(s2_raster)<0]=0
# values(s2_raster)[values(s2_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(s2_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# s3_raster <- rasterize(summerdata.t3,raster_grd,summerdata.t3$fit,fun=mean)
# #
# crs(s3_raster)=crs(shape)
# 
# values(s3_raster)[values(s3_raster)<0]=0
# values(s3_raster)[values(s3_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(s3_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0,1.5)),
#            lab.breaks=c("0.00","0.10","0.20","0.30","0.40",
#                         "0.50","0.60","0.70","0.80",
#                         "0.90","1.00","1.10","1.20",
#                         "1.30","1.40","1.50"),
#            col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),legend.only=TRUE,horizontal=TRUE)
# 
# #
# s4_raster <- rasterize(summerdata.t4,raster_grd,summerdata.t4$fit,fun=mean)
# #
# crs(s4_raster)=crs(shape)
# 
# values(s4_raster)[values(s4_raster)<0]=0
# values(s4_raster)[values(s4_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(s4_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# dev.off()
# 
# ######
# 
# 
# jpeg("new.gwr.Fall.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# f12_raster <- rasterize(falldata2.t1,raster_grd,falldata2.t1$pred,fun=mean)
# #
# crs(f12_raster)=crs(shape)
# 
# values(f12_raster)[values(f12_raster)<0]=0
# values(f12_raster)[values(f12_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# box()
# 
# image(f12_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# f22_raster <- rasterize(falldata2.t2,raster_grd,falldata2.t2$pred,fun=mean)
# #
# crs(f22_raster)=crs(shape)
# 
# values(f22_raster)[values(f22_raster)<0]=0
# values(f22_raster)[values(f22_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f22_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# f32_raster <- rasterize(falldata2.t3,raster_grd,falldata2.t3$pred,fun=mean)
# #
# crs(f32_raster)=crs(shape)
# 
# values(f32_raster)[values(f32_raster)<0]=0
# values(f32_raster)[values(f32_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f32_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# f42_raster <- rasterize(falldata2.t4,raster_grd,falldata2.t4$pred,fun=mean)
# #
# crs(f42_raster)=crs(shape)
# 
# values(f42_raster)[values(f42_raster)<0]=0
# values(f42_raster)[values(f42_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f42_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# dev.off()
# 
# 
# jpeg("new.gwr.Summer.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# s12_raster <- rasterize(summerdata2.t1,raster_grd,summerdata2.t1$pred,fun=mean)
# #
# crs(s12_raster)=crs(shape)
# 
# values(s12_raster)[values(s12_raster)<0]=0
# values(s12_raster)[values(s12_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# box()
# 
# image(s12_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# s22_raster <- rasterize(summerdata2.t2,raster_grd,summerdata2.t2$pred,fun=mean)
# #
# crs(s22_raster)=crs(shape)
# 
# values(s22_raster)[values(s22_raster)<0]=0
# values(s22_raster)[values(s22_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s22_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# s32_raster <- rasterize(summerdata2.t3,raster_grd,summerdata2.t3$pred,fun=mean)
# #
# crs(s32_raster)=crs(shape)
# 
# values(s32_raster)[values(s32_raster)<0]=0
# values(s32_raster)[values(s32_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s32_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# s42_raster <- rasterize(summerdata2.t4,raster_grd,summerdata2.t4$pred,fun=mean)
# #
# crs(s42_raster)=crs(shape)
# 
# values(s42_raster)[values(s42_raster)<0]=0
# values(s42_raster)[values(s42_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s42_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# dev.off()
# 
# ######
# 
# jpeg("new.obs.Fall.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# f1_raster <- rasterize(falldata.t1,raster_grd,falldata.t1$logrscpue,fun=mean)
# #
# crs(f1_raster)=crs(shape)
# 
# values(f1_raster)[values(f1_raster)<0]=0
# values(f1_raster)[values(f1_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# box()
# 
# image(f1_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# f2_raster <- rasterize(falldata.t2,raster_grd,falldata.t2$logrscpue,fun=mean)
# #
# crs(f2_raster)=crs(shape)
# 
# values(f2_raster)[values(f2_raster)<0]=0
# values(f2_raster)[values(f2_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f2_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# f3_raster <- rasterize(falldata.t3,raster_grd,falldata.t3$logrscpue,fun=mean)
# #
# crs(f3_raster)=crs(shape)
# 
# values(f3_raster)[values(f3_raster)<0]=0
# values(f3_raster)[values(f3_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f3_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# f4_raster <- rasterize(falldata.t4,raster_grd,falldata.t4$logrscpue,fun=mean)
# #
# crs(f4_raster)=crs(shape)
# 
# values(f4_raster)[values(f4_raster)<0]=0
# values(f4_raster)[values(f4_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f4_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# dev.off()
# 
# 
# jpeg("new.obs.Summer.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# s1_raster <- rasterize(summerdata.t1,raster_grd,summerdata.t1$logrscpue,fun=mean)
# #
# crs(s1_raster)=crs(shape)
# 
# values(s1_raster)[values(s1_raster)<0]=0
# values(s1_raster)[values(s1_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# box()
# 
# image(s1_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# s2_raster <- rasterize(summerdata.t2,raster_grd,summerdata.t2$logrscpue,fun=mean)
# #
# crs(s2_raster)=crs(shape)
# 
# values(s2_raster)[values(s2_raster)<0]=0
# values(s2_raster)[values(s2_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s2_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# s3_raster <- rasterize(summerdata.t3,raster_grd,summerdata.t3$logrscpue,fun=mean)
# #
# crs(s3_raster)=crs(shape)
# 
# values(s3_raster)[values(s3_raster)<0]=0
# values(s3_raster)[values(s3_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s3_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# s4_raster <- rasterize(summerdata.t4,raster_grd,summerdata.t4$logrscpue,fun=mean)
# #
# crs(s4_raster)=crs(shape)
# 
# values(s4_raster)[values(s4_raster)<0]=0
# values(s4_raster)[values(s4_raster)>1.5]=1.5
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s4_raster, col=c(rev(brewer.pal(11, "RdYlBu"))[1:7],rep(rev(brewer.pal(11, "RdYlBu"))[8:11],each=2)),
#       breaks=c(seq(0,1.5,by=0.1)),xlab="",ylab="",add=TRUE,zlim=range(c(0,1.5)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# dev.off()
# 
# 
# ###########################################################################################################
# #                                           ERROR FIGURE
# ###########################################################################################################
# 
# 
# jpeg("new.R2.Fall.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# f122_raster <- rasterize(falldata22.t1,raster_grd,falldata22.t1$localR2,fun=mean)
# #
# crs(f122_raster)=crs(shape)
# 
# values(f122_raster)[values(f122_raster)<0.15]=0.15
# values(f122_raster)[values(f122_raster)>0.6]=0.6
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# box()
# 
# image(f122_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:10]),
#       breaks=c(seq(0.15,0.60,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# f222_raster <- rasterize(falldata22.t2,raster_grd,falldata22.t2$localR2,fun=mean)
# #
# crs(f222_raster)=crs(shape)
# 
# values(f222_raster)[values(f222_raster)<0.15]=0.15
# values(f222_raster)[values(f222_raster)>0.6]=0.6
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f222_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:10]),
#       breaks=c(seq(0.15,0.60,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# f322_raster <- rasterize(falldata22.t3,raster_grd,falldata22.t3$localR2,fun=mean)
# #
# crs(f322_raster)=crs(shape)
# 
# values(f322_raster)[values(f322_raster)<0.15]=0.15
# values(f322_raster)[values(f322_raster)>0.6]=0.6
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f322_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:10]),
#       breaks=c(seq(0.15,0.60,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.15,0.60)),
#            lab.breaks=c("0.15","0.20","0.25","0.30","0.35","0.40","0.45","0.50","0.55","0.60"),
#            col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:10]),legend.only=TRUE,horizontal=TRUE)
# 
# #
# 
# f422_raster <- rasterize(falldata22.t4,raster_grd,falldata22.t4$localR2,fun=mean)
# #
# crs(f422_raster)=crs(shape)
# 
# values(f422_raster)[values(f422_raster)<0.15]=0.15
# values(f422_raster)[values(f422_raster)>0.6]=0.6
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f422_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:10]),
#       breaks=c(seq(0.15,0.60,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# dev.off()
# 
# 
# 
# jpeg("new.R2.Summer.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# s122_raster <- rasterize(summerdata22.t1,raster_grd,summerdata22.t1$localR2,fun=mean)
# #
# crs(s122_raster)=crs(shape)
# 
# values(s122_raster)[values(s122_raster)<0.15]=0.15
# values(s122_raster)[values(s122_raster)>0.6]=0.6
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# box()
# 
# image(s122_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:10]),
#       breaks=c(seq(0.15,0.60,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# s222_raster <- rasterize(summerdata22.t2,raster_grd,summerdata22.t2$localR2,fun=mean)
# #
# crs(s222_raster)=crs(shape)
# 
# values(s222_raster)[values(s222_raster)<0.15]=0.15
# values(s222_raster)[values(s222_raster)>0.6]=0.6
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s222_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:10]),
#       breaks=c(seq(0.15,0.60,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# s322_raster <- rasterize(summerdata22.t3,raster_grd,summerdata22.t3$localR2,fun=mean)
# #
# crs(s322_raster)=crs(shape)
# 
# values(s322_raster)[values(s322_raster)<0.15]=0.15
# values(s322_raster)[values(s322_raster)>0.6]=0.6
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s322_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:10]),
#       breaks=c(seq(0.15,0.60,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.15,0.60)),
#            lab.breaks=c("0.15","0.20","0.25","0.30","0.35","0.40","0.45","0.50","0.55","0.60"),
#            col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:10]),legend.only=TRUE,horizontal=TRUE)
# 
# #
# 
# s422_raster <- rasterize(summerdata22.t4,raster_grd,summerdata22.t4$localR2,fun=mean)
# #
# crs(s422_raster)=crs(shape)
# 
# values(s422_raster)[values(s422_raster)<0.15]=0.15
# values(s422_raster)[values(s422_raster)>0.6]=0.6
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s422_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:10]),
#       breaks=c(seq(0.15,0.60,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# dev.off()
# 
# 
# ######
# 
# 
# jpeg("new.posSE.Fall.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# f1posse_raster <- rasterize(falldata.t1,raster_grd,falldata.t1$se.pos,fun=mean)
# #
# crs(f1posse_raster)=crs(shape)
# 
# values(f1posse_raster)[values(f1posse_raster)<0.05]=0.05
# values(f1posse_raster)[values(f1posse_raster)>0.65]=0.65
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(f1posse_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:8],rep(brewer.pal(11, "PRGn")[9:10],each=2),brewer.pal(11, "PRGn")[11]),
#       breaks=c(seq(0.05,0.65,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.05,0.65)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# f2posse_raster <- rasterize(falldata.t2,raster_grd,falldata.t2$se.pos,fun=mean)
# #
# crs(f2posse_raster)=crs(shape)
# 
# values(f2posse_raster)[values(f2posse_raster)<0.05]=0.05
# values(f2posse_raster)[values(f2posse_raster)>0.65]=0.65
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(f2posse_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:8],rep(brewer.pal(11, "PRGn")[9:10],each=2),brewer.pal(11, "PRGn")[11]),
#       breaks=c(seq(0.05,0.65,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.05,0.65)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# f3posse_raster <- rasterize(falldata.t3,raster_grd,falldata.t3$se.pos,fun=mean)
# #
# crs(f3posse_raster)=crs(shape)
# 
# values(f3posse_raster)[values(f3posse_raster)<0.05]=0.05
# values(f3posse_raster)[values(f3posse_raster)>0.65]=0.65
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(f3posse_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:8],rep(brewer.pal(11, "PRGn")[9:10],each=2),brewer.pal(11, "PRGn")[11]),
#       breaks=c(seq(0.05,0.65,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.05,0.65)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.05,0.65)),
#            lab.breaks=c("0.05","0.10","0.15","0.20","0.25","0.30","0.35","0.40","0.45","0.50","0.55","0.60","0.65"),
#            col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:8],rep(brewer.pal(11, "PRGn")[9:10],each=2),brewer.pal(11, "PRGn")[11]),
#            legend.only=TRUE,horizontal=TRUE)
# 
# 
# #
# f4posse_raster <- rasterize(falldata.t4,raster_grd,falldata.t4$se.pos,fun=mean)
# #
# crs(f4posse_raster)=crs(shape)
# 
# values(f4posse_raster)[values(f4posse_raster)<0.05]=0.05
# values(f4posse_raster)[values(f4posse_raster)>0.65]=0.65
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(f4posse_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:8],rep(brewer.pal(11, "PRGn")[9:10],each=2),brewer.pal(11, "PRGn")[11]),
#       breaks=c(seq(0.05,0.65,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.05,0.65)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# dev.off()
# 
# 
# jpeg("new.posSE.Summer.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# s1posse_raster <- rasterize(summerdata.t1,raster_grd,summerdata.t1$se.pos,fun=mean)
# #
# crs(s1posse_raster)=crs(shape)
# 
# values(s1posse_raster)[values(s1posse_raster)<0.05]=0.05
# values(s1posse_raster)[values(s1posse_raster)>0.65]=0.65
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(s1posse_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:8],rep(brewer.pal(11, "PRGn")[9:10],each=2),brewer.pal(11, "PRGn")[11]),
#       breaks=c(seq(0.05,0.65,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.05,0.65)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# s2posse_raster <- rasterize(summerdata.t2,raster_grd,summerdata.t2$se.pos,fun=mean)
# #
# crs(s2posse_raster)=crs(shape)
# 
# values(s2posse_raster)[values(s2posse_raster)<0.05]=0.05
# values(s2posse_raster)[values(s2posse_raster)>0.65]=0.65
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(s2posse_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:8],rep(brewer.pal(11, "PRGn")[9:10],each=2),brewer.pal(11, "PRGn")[11]),
#       breaks=c(seq(0.05,0.65,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.05,0.65)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# s3posse_raster <- rasterize(summerdata.t3,raster_grd,summerdata.t3$se.pos,fun=mean)
# #
# crs(s3posse_raster)=crs(shape)
# 
# values(s3posse_raster)[values(s3posse_raster)<0.05]=0.05
# values(s3posse_raster)[values(s3posse_raster)>0.65]=0.65
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(s3posse_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:8],rep(brewer.pal(11, "PRGn")[9:10],each=2),brewer.pal(11, "PRGn")[11]),
#       breaks=c(seq(0.05,0.65,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.05,0.65)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.05,0.65)),
#            lab.breaks=c("0.05","0.10","0.15","0.20","0.25","0.30","0.35","0.40","0.45","0.50","0.55","0.60","0.65"),
#            col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:8],rep(brewer.pal(11, "PRGn")[9:10],each=2),brewer.pal(11, "PRGn")[11]),
#            legend.only=TRUE,horizontal=TRUE)
# #
# s4posse_raster <- rasterize(summerdata.t4,raster_grd,summerdata.t4$se.pos,fun=mean)
# #
# crs(s4posse_raster)=crs(shape)
# 
# 
# values(s4posse_raster)[values(s4posse_raster)>0.65]=0.65
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(2)
# box()
# 
# image(s4posse_raster, col=c(brewer.pal(11, "PRGn")[1:5],brewer.pal(11, "PRGn")[7:8],rep(brewer.pal(11, "PRGn")[9:10],each=2),brewer.pal(11, "PRGn")[11]),
#       breaks=c(seq(0.05,0.65,by=0.05)),xlab="",ylab="",add=TRUE,zlim=range(c(0.05,0.65)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# dev.off()
# 
# ######
# 
# 
# jpeg("new.PASE.Fall.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# f1PAse_raster <- rasterize(falldata.t1,raster_grd,falldata.t1$se.PA,fun=mean)
# #
# crs(f1PAse_raster)=crs(shape)
# 
# values(f1PAse_raster)[values(f1PAse_raster)>0.16]=0.16
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# box()
# 
# image(f1PAse_raster, col=c(brewer.pal(11, "PRGn")[1:4],brewer.pal(11, "PRGn")[8:11]),
#       breaks=c(seq(0.0,0.16,by=0.02)),xlab="",ylab="",add=TRUE,zlim=range(c(0.00,0.16)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# f2PAse_raster <- rasterize(falldata.t2,raster_grd,falldata.t2$se.PA,fun=mean)
# #
# crs(f2PAse_raster)=crs(shape)
# 
# values(f2PAse_raster)[values(f2PAse_raster)>0.16]=0.16
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f2PAse_raster, col=c(brewer.pal(11, "PRGn")[1:4],brewer.pal(11, "PRGn")[8:11]),
#       breaks=c(seq(0.0,0.16,by=0.02)),xlab="",ylab="",add=TRUE,zlim=range(c(0.00,0.16)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# f3PAse_raster <- rasterize(falldata.t3,raster_grd,falldata.t3$se.PA,fun=mean)
# #
# crs(f3PAse_raster)=crs(shape)
# 
# values(f3PAse_raster)[values(f3PAse_raster)>0.16]=0.16
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f3PAse_raster, col=c(brewer.pal(11, "PRGn")[1:4],brewer.pal(11, "PRGn")[8:11]),
#       breaks=c(seq(0.0,0.16,by=0.02)),xlab="",ylab="",add=TRUE,zlim=range(c(0.00,0.16)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.00,0.16)),
#            lab.breaks=c("0.00","0.02","0.04","0.06","0.08","0.10","0.12","0.14","0.16"),
#            col=c(brewer.pal(11, "PRGn")[1:4],brewer.pal(11, "PRGn")[8:11]),
#            legend.only=TRUE,horizontal=TRUE)
# 
# #
# f4PAse_raster <- rasterize(falldata.t4,raster_grd,falldata.t4$se.PA,fun=mean)
# #
# crs(f4PAse_raster)=crs(shape)
# 
# values(f4PAse_raster)[values(f4PAse_raster)>0.16]=0.16
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(f4PAse_raster, col=c(brewer.pal(11, "PRGn")[1:4],brewer.pal(11, "PRGn")[8:11]),
#       breaks=c(seq(0.0,0.16,by=0.02)),xlab="",ylab="",add=TRUE,zlim=range(c(0.00,0.16)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# dev.off()
# 
# 
# jpeg("new.PASE.Summer.jpg",width = 2.5, height = 7.5,units="in",res=300)
# 
# par(mfrow=c(4,1),mar=c(2.1, 2.1, 2.1, 2.1))
# 
# s1PAse_raster <- rasterize(summerdata.t1,raster_grd,summerdata.t1$se.PA,fun=mean)
# #
# crs(s1PAse_raster)=crs(shape)
# 
# values(s1PAse_raster)[values(s1PAse_raster)>0.16]=0.16
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# box()
# 
# image(s1PAse_raster, col=c(brewer.pal(11, "PRGn")[1:4],brewer.pal(11, "PRGn")[8:11]),
#       breaks=c(seq(0.0,0.16,by=0.02)),xlab="",ylab="",add=TRUE,zlim=range(c(0.00,0.16)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# s2PAse_raster <- rasterize(summerdata.t2,raster_grd,summerdata.t2$se.PA,fun=mean)
# #
# crs(s2PAse_raster)=crs(shape)
# 
# values(s2PAse_raster)[values(s2PAse_raster)>0.16]=0.16
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s2PAse_raster, col=c(brewer.pal(11, "PRGn")[1:4],brewer.pal(11, "PRGn")[8:11]),
#       breaks=c(seq(0.0,0.16,by=0.02)),xlab="",ylab="",add=TRUE,zlim=range(c(0.00,0.16)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# 
# #
# 
# s3PAse_raster <- rasterize(summerdata.t3,raster_grd,summerdata.t3$se.PA,fun=mean)
# #
# crs(s3PAse_raster)=crs(shape)
# 
# values(s3PAse_raster)[values(s3PAse_raster)>0.16]=0.16
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s3PAse_raster, col=c(brewer.pal(11, "PRGn")[1:4],brewer.pal(11, "PRGn")[8:11]),
#       breaks=c(seq(0.0,0.16,by=0.02)),xlab="",ylab="",add=TRUE,zlim=range(c(0.00,0.16)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.00,0.16)),
#            lab.breaks=c("0.00","0.02","0.04","0.06","0.08","0.10","0.12","0.14","0.16"),
#            col=c(brewer.pal(11, "PRGn")[1:4],brewer.pal(11, "PRGn")[8:11]),
#            legend.only=TRUE,horizontal=TRUE)
# #
# s4PAse_raster <- rasterize(summerdata.t4,raster_grd,summerdata.t4$se.PA,fun=mean)
# #
# crs(s4PAse_raster)=crs(shape)
# 
# values(s4PAse_raster)[values(s4PAse_raster)>0.16]=0.16
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# box()
# 
# image(s4PAse_raster, col=c(brewer.pal(11, "PRGn")[1:4],brewer.pal(11, "PRGn")[8:11]),
#       breaks=c(seq(0.0,0.16,by=0.02)),xlab="",ylab="",add=TRUE,zlim=range(c(0.00,0.16)))
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# #
# 
# dev.off()
# 
# 
# 
# 
# ##########################
# ##########################
# ##########################
# 
# 
# summerdata=read.csv("summerdata.csv")
# falldata=read.csv("falldata.csv")
# 
# xr <- range(falldata$LONGITUDE)
# yr <- range(falldata$LATITUDE)
# b <- round((xr[2]-xr[1])/100,2)
# 
# xmin=min(falldata$LONGITUDE)
# xmax=max(falldata$LONGITUDE)
# ymin=min(summerdata$LATITUDE)
# ymax=max(summerdata$LATITUDE)
# #
# grd <- expand.grid(x=seq(from=xr[1],to=xr[2],by=b),
#                    y=seq(from=yr[1],to=yr[2],by=b))
# #
# coordinates(grd) <- ~ x+y
# gridded(grd) <- TRUE
# 
# xr <- range(summerdata$LONGITUDE)
# yr <- range(summerdata$LATITUDE)
# b <- round((xr[2]-xr[1])/100,2)
# #
# grd2 <- expand.grid(x=seq(from=xr[1],to=xr[2],by=b),
#                     y=seq(from=yr[1],to=yr[2],by=b))
# #
# coordinates(grd2) <- ~ x+y
# gridded(grd2) <- TRUE
# 
# shape=readOGR(dsn=".",layer="Gulf_of_Mexico_Shoreline")
# 
# ######
# 
# summerdata.t1=read.csv("summerdata.t1.csv")
# summerdata.t2=read.csv("summerdata.t2.csv")
# summerdata.t3=read.csv("summerdata.t3.csv")
# falldata.t1=read.csv("falldata.t1.csv")
# falldata.t2=read.csv("falldata.t2.csv")
# falldata.t3=read.csv("falldata.t3.csv")
# 
# coordinates(falldata.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata.t3) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata.t3) <- ~ LONGITUDE+LATITUDE
# 
# 
# falldata2<-gwr_fall_result.se
# summerdata2<-gwr_summer_result.se
# falldata22<-read.csv("falldata.csv")
# summerdata22<-read.csv("summerdata.csv")
# 
# 
# summerdata2.t1=summerdata2[summerdata$YEAR<=1994,]
# nrow(summerdata2.t1)
# 
# falldata2.t1=falldata2[falldata$YEAR<=1994,]
# nrow(falldata2.t1)
# 
# summerdata2.t2=summerdata2[summerdata$YEAR>1994&summerdata$YEAR<=2009,]
# nrow(summerdata2.t2)
# falldata2.t2=falldata2[falldata$YEAR>1994&falldata$YEAR<=2009,]
# nrow(falldata2.t2)
# 
# summerdata2.t3=summerdata2[summerdata$YEAR>2009,]
# nrow(summerdata2.t3)
# falldata2.t3=falldata2[falldata$YEAR>2009,]
# nrow(falldata2.t3)
# 
# falldata2.t1=falldata2.t1[!is.na(falldata2.t1$LONGITUDE)&!is.na(falldata2.t1$LONGITUDE),]
# falldata2.t2=falldata2.t2[!is.na(falldata2.t2$LONGITUDE)&!is.na(falldata2.t2$LONGITUDE),]
# falldata2.t3=falldata2.t3[!is.na(falldata2.t3$LONGITUDE)&!is.na(falldata2.t3$LONGITUDE),]
# summerdata2.t1=summerdata2.t1[!is.na(summerdata2.t1$LONGITUDE)&!is.na(summerdata2.t1$LONGITUDE),]
# summerdata2.t2=summerdata2.t2[!is.na(summerdata2.t2$LONGITUDE)&!is.na(summerdata2.t2$LONGITUDE),]
# summerdata2.t3=summerdata2.t3[!is.na(summerdata2.t3$LONGITUDE)&!is.na(summerdata2.t3$LONGITUDE),]
# 
# coordinates(falldata2.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata2.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata2.t3) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata2.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata2.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata2.t3) <- ~ LONGITUDE+LATITUDE
# 
# ######
# 
# par(mar=c(6.1, 7.1, 4.1, 6.1))
# 
# jpeg("ErrorFall.jpg",width = 11, height = 8,units="in",res=300)
# 
# par(mfrow=c(3,3))
# 
# idwf1<-idw(formula=se.pos~1, locations=falldata.t1, newdata=grd)
# #
# 
# idwf1_output <- as.data.frame(idwf1)[, 1:3]
# 
# idwf1_raster <- rasterFromXYZ(idwf1_output)
# 
# idwf1_raster_crop <- mask(idwf1_raster, hull_polygon)
# 
# crs(idwf1_raster_crop)=crs(shape)
# 
# values(idwf1_raster_crop)[values(idwf1_raster_crop)<0.10]=0.10
# values(idwf1_raster_crop)[values(idwf1_raster_crop)>0.25]=0.25
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idwf1_raster_crop, col=c(rev(brewer.pal(10, "PRGn"))),xlab="",ylab="",add=TRUE,zlim=range(c(0.10,0.25)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.10,0.25)),
#            lab.breaks=c("0.10","","0.13","","0.16","","0.19","","0.22","","0.25"),
#            col=c(rev(brewer.pal(10, "PRGn"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw<-idw(formula=se.PA~1, locations=falldata.t1, newdata=grd)
# #
# 
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# values(idw_raster_crop)[values(idw_raster_crop)<0]=0
# values(idw_raster_crop)[values(idw_raster_crop)>0.16]=0.16
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw_raster_crop, col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),xlab="",ylab="",add=TRUE,zlim=range(c(0,0.16)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0,0.16)),
#            lab.breaks=c("0.00","","0.02","","0.04","","0.06","","0.08","",
#                         "0.10","","0.12","","0.14","","0.16"),
#            col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),legend.only=TRUE,horizontal=TRUE)
# 
# idw2f1<-idw(formula=localR2~1, locations=falldata2.t1, newdata=grd)
# #
# 
# idw2f1_output <- as.data.frame(idw2f1)[, 1:3]
# 
# idw2f1_raster <- rasterFromXYZ(idw2f1_output)
# 
# idw2f1_raster_crop <- mask(idw2f1_raster, hull_polygon)
# 
# crs(idw2f1_raster_crop)=crs(shape)
# 
# values(idw2f1_raster_crop)[values(idw2f1_raster_crop)<0.15]=0.15
# values(idw2f1_raster_crop)[values(idw2f1_raster_crop)>0.6]=0.6
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw2f1_raster_crop, col=c(brewer.pal(9, "PRGn")),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.15,0.60)),
#            lab.breaks=c("0.15","0.20","0.25","0.30","0.35","0.40","0.45","0.50","0.55","0.60"),
#            col=c(brewer.pal(9, "PRGn")),legend.only=TRUE,horizontal=TRUE)
# 
# idwf2<-idw(formula=se.pos~1, locations=falldata.t2, newdata=grd)
# #
# idwf2_output <- as.data.frame(idwf2)[, 1:3]
# 
# idwf2_raster <- rasterFromXYZ(idwf2_output)
# 
# idwf2_raster_crop <- mask(idwf2_raster, hull_polygon)
# 
# crs(idwf2_raster_crop)=crs(shape)
# 
# values(idwf2_raster_crop)[values(idwf2_raster_crop)<0.10]=0.10
# values(idwf2_raster_crop)[values(idwf2_raster_crop)>0.25]=0.25
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idwf2_raster_crop, col=c(rev(brewer.pal(10, "PRGn"))),xlab="",ylab="",add=TRUE,zlim=range(c(0.10,0.25)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.10,0.25)),
#            lab.breaks=c("0.10","","0.13","","0.16","","0.19","","0.22","","0.25"),
#            col=c(rev(brewer.pal(10, "PRGn"))),legend.only=TRUE,horizontal=TRUE)
# #
# 
# #
# idw<-idw(formula=se.PA~1, locations=falldata.t2, newdata=grd)
# #
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# values(idw_raster_crop)[values(idw_raster_crop)<0]=0
# values(idw_raster_crop)[values(idw_raster_crop)>0.16]=0.16
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw_raster_crop, col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),xlab="",ylab="",add=TRUE,zlim=range(c(0,0.16)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0,0.16)),
#            lab.breaks=c("0.00","","0.02","","0.04","","0.06","","0.08","",
#                         "0.10","","0.12","","0.14","","0.16"),
#            col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),legend.only=TRUE,horizontal=TRUE)
# 
# idw2f2<-idw(formula=localR2~1, locations=falldata2.t2, newdata=grd)
# #
# 
# idw2f2_output <- as.data.frame(idw2f2)[, 1:3]
# 
# idw2f2_raster <- rasterFromXYZ(idw2f2_output)
# 
# idw2f2_raster_crop <- mask(idw2f2_raster, hull_polygon)
# 
# crs(idw2f2_raster_crop)=crs(shape)
# 
# values(idw2f2_raster_crop)[values(idw2f2_raster_crop)<0.15]=0.15
# values(idw2f2_raster_crop)[values(idw2f2_raster_crop)>0.6]=0.6
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw2f2_raster_crop, col=c(brewer.pal(9, "PRGn")),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.15,0.60)),
#            lab.breaks=c("0.15","0.20","0.25","0.30","0.35","0.40","0.45","0.50","0.55","0.60"),
#            col=c(brewer.pal(9, "PRGn")),legend.only=TRUE,horizontal=TRUE)
# 
# idwf3<-idw(formula=se.pos~1, locations=falldata.t3, newdata=grd)
# #
# 
# idwf3_output <- as.data.frame(idwf3)[, 1:3]
# 
# idwf3_raster <- rasterFromXYZ(idwf3_output)
# 
# idwf3_raster_crop <- mask(idwf3_raster, hull_polygon)
# 
# crs(idwf3_raster_crop)=crs(shape)
# 
# 
# values(idwf3_raster_crop)[values(idwf3_raster_crop)<0.10]=0.10
# values(idwf3_raster_crop)[values(idwf3_raster_crop)>0.25]=0.25
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idwf3_raster_crop, col=c(rev(brewer.pal(10, "PRGn"))),xlab="",ylab="",add=TRUE,zlim=range(c(0.10,0.25)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.10,0.25)),
#            lab.breaks=c("0.10","","0.13","","0.16","","0.19","","0.22","","0.25"),
#            col=c(rev(brewer.pal(10, "PRGn"))),legend.only=TRUE,horizontal=TRUE)
# #
# 
# idw<-idw(formula=se.PA~1, locations=falldata.t3, newdata=grd)
# #
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# values(idw_raster_crop)[values(idw_raster_crop)<0]=0
# values(idw_raster_crop)[values(idw_raster_crop)>0.16]=0.16
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw_raster_crop, col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),xlab="",ylab="",add=TRUE,zlim=range(c(0,0.16)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0,0.16)),
#            lab.breaks=c("0.00","","0.02","","0.04","","0.06","","0.08","",
#                         "0.10","","0.12","","0.14","","0.16"),
#            col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),legend.only=TRUE,horizontal=TRUE)
# 
# idw2f3<-idw(formula=localR2~1, locations=falldata2.t3, newdata=grd)
# #
# 
# idw2f3_output <- as.data.frame(idw2f3)[, 1:3]
# 
# idw2f3_raster <- rasterFromXYZ(idw2f3_output)
# 
# idw2f3_raster_crop <- mask(idw2f3_raster, hull_polygon)
# 
# crs(idw2f3_raster_crop)=crs(shape)
# 
# values(idw2f3_raster_crop)[values(idw2f3_raster_crop)<0.15]=0.15
# values(idw2f3_raster_crop)[values(idw2f3_raster_crop)>0.6]=0.6
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw2f3_raster_crop, col=c(brewer.pal(9, "PRGn")),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.15,0.60)),
#            lab.breaks=c("0.15","0.20","0.25","0.30","0.35","0.40","0.45","0.50","0.55","0.60"),
#            col=c(brewer.pal(9, "PRGn")),legend.only=TRUE,horizontal=TRUE)
# 
# dev.off()
# 
# ######
# 
# 
# jpeg("ErrorSummer.jpg",width = 11, height = 8,units="in",res=300)
# 
# par(mfrow=c(3,3))
# 
# idws1<-idw(formula=se.pos~1, locations=summerdata.t1, newdata=grd2)
# #
# idws1_output <- as.data.frame(idws1)[, 1:3]
# 
# idws1_raster <- rasterFromXYZ(idws1_output)
# 
# idws1_raster_crop <- mask(idws1_raster, hull_polygon)
# 
# crs(idws1_raster_crop)=crs(shape)
# 
# values(idws1_raster_crop)[values(idws1_raster_crop)<0.10]=0.10
# values(idws1_raster_crop)[values(idws1_raster_crop)>0.25]=0.25
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idws1_raster_crop, col=c(rev(brewer.pal(10, "PRGn"))),xlab="",ylab="",add=TRUE,zlim=range(c(0.10,0.25)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.10,0.25)),
#            lab.breaks=c("0.10","","0.13","","0.16","","0.19","","0.22","","0.25"),
#            col=c(rev(brewer.pal(10, "PRGn"))),legend.only=TRUE,horizontal=TRUE)
# 
# 
# idw<-idw(formula=se.PA~1, locations=summerdata.t1, newdata=grd2)
# #
# 
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# values(idw_raster_crop)[values(idw_raster_crop)<0]=0
# values(idw_raster_crop)[values(idw_raster_crop)>0.16]=0.16
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw_raster_crop, col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),xlab="",ylab="",add=TRUE,zlim=range(c(0,0.16)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0,0.16)),
#            lab.breaks=c("0.00","","0.02","","0.04","","0.06","","0.08","",
#                         "0.10","","0.12","","0.14","","0.16"),
#            col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),legend.only=TRUE,horizontal=TRUE)
# #
# idw2s1<-idw(formula=localR2~1, locations=summerdata2.t1, newdata=grd2)
# #
# 
# idw2s1_output <- as.data.frame(idw2s1)[, 1:3]
# 
# idw2s1_raster <- rasterFromXYZ(idw2s1_output)
# 
# idw2s1_raster_crop <- mask(idw2s1_raster, hull_polygon)
# 
# crs(idw2s1_raster_crop)=crs(shape)
# 
# 
# values(idw2s1_raster_crop)[values(idw2s1_raster_crop)<0.15]=0.15
# values(idw2s1_raster_crop)[values(idw2s1_raster_crop)>0.6]=0.6
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw2s1_raster_crop, col=c(brewer.pal(9, "PRGn")),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.15,0.60)),
#            lab.breaks=c("0.15","0.20","0.25","0.30","0.35","0.40","0.45","0.50","0.55","0.60"),
#            col=c(brewer.pal(9, "PRGn")),legend.only=TRUE,horizontal=TRUE)
# 
# idws2<-idw(formula=se.pos~1, locations=summerdata.t2, newdata=grd2)
# #
# 
# idws2_output <- as.data.frame(idws2)[, 1:3]
# 
# idws2_raster <- rasterFromXYZ(idws2_output)
# 
# idws2_raster_crop <- mask(idws2_raster, hull_polygon)
# 
# crs(idws2_raster_crop)=crs(shape)
# 
# values(idws2_raster_crop)[values(idws2_raster_crop)<0.10]=0.10
# values(idws2_raster_crop)[values(idws2_raster_crop)>0.25]=0.25
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idws2_raster_crop, col=c(rev(brewer.pal(10, "PRGn"))),xlab="",ylab="",add=TRUE,zlim=range(c(0.10,0.25)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.10,0.25)),
#            lab.breaks=c("0.10","","0.13","","0.16","","0.19","","0.22","","0.25"),
#            col=c(rev(brewer.pal(10, "PRGn"))),legend.only=TRUE,horizontal=TRUE)
# 
# 
# idw<-idw(formula=se.PA~1, locations=summerdata.t2, newdata=grd2)
# #
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# values(idw_raster_crop)[values(idw_raster_crop)<0]=0
# values(idw_raster_crop)[values(idw_raster_crop)>0.16]=0.16
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw_raster_crop, col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),xlab="",ylab="",add=TRUE,zlim=range(c(0,0.16)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0,0.16)),
#            lab.breaks=c("0.00","","0.02","","0.04","","0.06","","0.08","",
#                         "0.10","","0.12","","0.14","","0.16"),
#            col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),legend.only=TRUE,horizontal=TRUE)
# 
# idw2s2<-idw(formula=localR2~1, locations=summerdata2.t2, newdata=grd2)
# #
# 
# idw2s2_output <- as.data.frame(idw2s2)[, 1:3]
# 
# idw2s2_raster <- rasterFromXYZ(idw2s2_output)
# 
# idw2s2_raster_crop <- mask(idw2s2_raster, hull_polygon)
# 
# crs(idw2s2_raster_crop)=crs(shape)
# 
# values(idw2s2_raster_crop)[values(idw2s2_raster_crop)<0.15]=0.15
# values(idw2s2_raster_crop)[values(idw2s2_raster_crop)>0.6]=0.6
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw2s2_raster_crop, col=c(brewer.pal(9, "PRGn")),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.15,0.60)),
#            lab.breaks=c("0.15","0.20","0.25","0.30","0.35","0.40","0.45","0.50","0.55","0.60"),
#            col=c(brewer.pal(9, "PRGn")),legend.only=TRUE,horizontal=TRUE)
# 
# idws3<-idw(formula=se.pos~1, locations=summerdata.t3, newdata=grd2)
# #
# 
# idws3_output <- as.data.frame(idws3)[, 1:3]
# 
# idws3_raster <- rasterFromXYZ(idws3_output)
# 
# idws3_raster_crop <- mask(idws3_raster, hull_polygon)
# 
# crs(idws3_raster_crop)=crs(shape)
# 
# 
# values(idws3_raster_crop)[values(idws3_raster_crop)<0.10]=0.10
# values(idws3_raster_crop)[values(idws3_raster_crop)>0.25]=0.25
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idws3_raster_crop, col=c(rev(brewer.pal(10, "PRGn"))),xlab="",ylab="",add=TRUE,zlim=range(c(0.10,0.25)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.10,0.25)),
#            lab.breaks=c("0.10","","0.13","","0.16","","0.19","","0.22","","0.25"),
#            col=c(rev(brewer.pal(10, "PRGn"))),legend.only=TRUE,horizontal=TRUE)
# 
# 
# 
# idw<-idw(formula=se.PA~1, locations=summerdata.t3, newdata=grd2)
# #
# 
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# values(idw_raster_crop)[values(idw_raster_crop)<0]=0
# values(idw_raster_crop)[values(idw_raster_crop)>0.16]=0.16
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw_raster_crop, col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),xlab="",ylab="",add=TRUE,zlim=range(c(0,0.16)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0,0.16)),
#            lab.breaks=c("0.00","","0.02","","0.04","","0.06","","0.08","",
#                         "0.10","","0.12","","0.14","","0.16"),
#            col=c(rev(brewer.pal(11, "PRGn"))[1:6],rep(rev(brewer.pal(11, "PRGn"))[7:11],each=2)),legend.only=TRUE,horizontal=TRUE)
# #
# idw2s3<-idw(formula=localR2~1, locations=summerdata2.t3, newdata=grd2)
# #
# 
# idw2s3_output <- as.data.frame(idw2s3)[, 1:3]
# 
# idw2s3_raster <- rasterFromXYZ(idw2s3_output)
# 
# idw2s3_raster_crop <- mask(idw2s3_raster, hull_polygon)
# 
# crs(idw2s3_raster_crop)=crs(shape)
# 
# values(idw2s3_raster_crop)[values(idw2s3_raster_crop)<0.15]=0.15
# values(idw2s3_raster_crop)[values(idw2s3_raster_crop)>0.6]=0.6
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idw2s3_raster_crop, col=c(brewer.pal(9, "PRGn")),xlab="",ylab="",add=TRUE,zlim=range(c(0.15,0.60)))
# 
# 
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(0.15,0.60)),
#            lab.breaks=c("0.15","0.20","0.25","0.30","0.35","0.40","0.45","0.50","0.55","0.60"),
#            col=c(brewer.pal(9, "PRGn")),legend.only=TRUE,horizontal=TRUE)
# 
# dev.off()
# 
# ###########################################################################################################
# #                                           LOCAL COEFF
# ###########################################################################################################
# 
# 
# jpeg("local_coeff_t3.jpg",width = 6, height = 23,units="in",res=300)
# 
# par(mfrow=c(9,2),mar=c(2.1,4.1,5.1,2.1))
# 
# 
# idw2f1_raster_crop <- rasterize(falldata2.t3,raster_grd,falldata2.t3$SURF.TEMP,fun=mean)
# #
# crs(idw2f1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Surface Temp fall")
# axis(3)
# axis(2)
# box()
# 
# image(idw2f1_raster_crop, breaks=seq(min(na.omit(values(idw2f1_raster_crop))),
#                                      max(na.omit(values(idw2f1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2f1_raster_crop))),
#                   max(na.omit(values(idw2f1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw2s1_raster_crop <- rasterize(summerdata2.t3,raster_grd,summerdata2.t3$SURF.TEMP,fun=mean)
# #
# crs(idw2s1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Surface Temp summer")
# axis(3)
# axis(2)
# box()
# 
# image(idw2s1_raster_crop, breaks=seq(min(na.omit(values(idw2s1_raster_crop))),
#                                      max(na.omit(values(idw2s1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2s1_raster_crop))),
#                   max(na.omit(values(idw2s1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw2f1_raster_crop <- rasterize(falldata2.t3,raster_grd,falldata2.t3$BOT.TEMP,fun=mean)
# #
# crs(idw2f1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Bottom Temp fall")
# axis(3)
# axis(2)
# box()
# 
# image(idw2f1_raster_crop, breaks=seq(min(na.omit(values(idw2f1_raster_crop))),
#                                      max(na.omit(values(idw2f1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2f1_raster_crop))),
#                   max(na.omit(values(idw2f1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="")
# 
# idw2f1_raster_crop <- rasterize(falldata2.t3,raster_grd,falldata2.t3$BOT.SAL,fun=mean)
# #
# crs(idw2f1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Bottom Salinity fall")
# axis(3)
# axis(2)
# box()
# 
# image(idw2f1_raster_crop, breaks=seq(min(na.omit(values(idw2f1_raster_crop))),
#                                      max(na.omit(values(idw2f1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2f1_raster_crop))),
#                   max(na.omit(values(idw2f1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw2s1_raster_crop <- rasterize(summerdata2.t3,raster_grd,summerdata2.t3$BOT.SAL,fun=mean)
# #
# crs(idw2s1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Bottom Salinity summer")
# axis(3)
# axis(2)
# box()
# 
# image(idw2s1_raster_crop, breaks=seq(min(na.omit(values(idw2s1_raster_crop))),
#                                      max(na.omit(values(idw2s1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2s1_raster_crop))),
#                   max(na.omit(values(idw2s1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw2f1_raster_crop <- rasterize(falldata2.t3,raster_grd,falldata2.t3$BOT.OXY,fun=mean)
# #
# crs(idw2f1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Bottom DO fall")
# axis(3)
# axis(2)
# box()
# 
# image(idw2f1_raster_crop, breaks=seq(min(na.omit(values(idw2f1_raster_crop))),
#                                      max(na.omit(values(idw2f1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2f1_raster_crop))),
#                   max(na.omit(values(idw2f1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw2s1_raster_crop <- rasterize(summerdata2.t3,raster_grd,summerdata2.t3$BOT.OXY,fun=mean)
# #
# crs(idw2s1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Bottom DO summer")
# axis(3)
# axis(2)
# box()
# 
# image(idw2s1_raster_crop, breaks=seq(min(na.omit(values(idw2s1_raster_crop))),
#                                      max(na.omit(values(idw2s1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2s1_raster_crop))),
#                   max(na.omit(values(idw2s1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw2f1_raster_crop <- rasterize(falldata2.t3,raster_grd,falldata2.t3$SURF.CHLOR,fun=mean)
# #
# crs(idw2f1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Surface Chlorophyll fall")
# axis(3)
# axis(2)
# box()
# 
# image(idw2f1_raster_crop, breaks=seq(min(na.omit(values(idw2f1_raster_crop))),
#                                      max(na.omit(values(idw2f1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2f1_raster_crop))),
#                   max(na.omit(values(idw2f1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw2s1_raster_crop <- rasterize(summerdata2.t3,raster_grd,summerdata2.t3$SURF.CHLOR,fun=mean)
# #
# crs(idw2s1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Surface Chlorophyll summer")
# axis(3)
# axis(2)
# box()
# 
# image(idw2s1_raster_crop, breaks=seq(min(na.omit(values(idw2s1_raster_crop))),
#                                      max(na.omit(values(idw2s1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2s1_raster_crop))),
#                   max(na.omit(values(idw2s1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# 
# idw2f1_raster_crop <- rasterize(falldata2.t3,raster_grd,falldata2.t3$BATHYMETRY,fun=mean)
# #
# crs(idw2f1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Bathymetry fall")
# axis(3)
# axis(2)
# box()
# 
# image(idw2f1_raster_crop, breaks=seq(min(na.omit(values(idw2f1_raster_crop))),
#                                      max(na.omit(values(idw2f1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2f1_raster_crop))),
#                   max(na.omit(values(idw2f1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# 
# idw2s1_raster_crop <- rasterize(summerdata2.t3,raster_grd,summerdata2.t3$BATHYMETRY,fun=mean)
# #
# crs(idw2s1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Bathymetry summer")
# axis(3)
# axis(2)
# box()
# 
# image(idw2s1_raster_crop, breaks=seq(min(na.omit(values(idw2s1_raster_crop))),
#                                      max(na.omit(values(idw2s1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2s1_raster_crop))),
#                   max(na.omit(values(idw2s1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw2f1_raster_crop <- rasterize(falldata2.t3,raster_grd,falldata2.t3$RUGOSITY,fun=mean)
# #
# crs(idw2f1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Rugosity fall")
# axis(3)
# axis(2)
# box()
# 
# image(idw2f1_raster_crop, breaks=seq(min(na.omit(values(idw2f1_raster_crop))),
#                                      max(na.omit(values(idw2f1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2f1_raster_crop))),
#                   max(na.omit(values(idw2f1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw2s1_raster_crop <- rasterize(summerdata2.t3,raster_grd,summerdata2.t3$RUGOSITY,fun=mean)
# #
# crs(idw2s1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Rugosity summer")
# axis(3)
# axis(2)
# box()
# 
# image(idw2s1_raster_crop, breaks=seq(min(na.omit(values(idw2s1_raster_crop))),
#                                      max(na.omit(values(idw2s1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2s1_raster_crop))),
#                   max(na.omit(values(idw2s1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw2f1_raster_crop <- rasterize(falldata2.t3,raster_grd,falldata2.t3$REEF,fun=mean)
# #
# crs(idw2f1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Reef fall")
# axis(3)
# axis(2)
# box()
# 
# image(idw2f1_raster_crop, breaks=seq(min(na.omit(values(idw2f1_raster_crop))),
#                                      max(na.omit(values(idw2f1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2f1_raster_crop))),
#                   max(na.omit(values(idw2f1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# idw2s1_raster_crop <- rasterize(summerdata2.t3,raster_grd,summerdata2.t3$REEF,fun=mean)
# #
# crs(idw2s1_raster_crop)=crs(shape)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="Reef summer")
# axis(3)
# axis(2)
# box()
# 
# image(idw2s1_raster_crop, breaks=seq(min(na.omit(values(idw2s1_raster_crop))),
#                                      max(na.omit(values(idw2s1_raster_crop))),
#                                      length.out=12),
#       col=c(rev(brewer.pal(11, "BrBG"))),xlab="",ylab="",add=TRUE)
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=c(min(na.omit(values(idw2s1_raster_crop))),
#                   max(na.omit(values(idw2s1_raster_crop)))),col=c(rev(brewer.pal(11, "BrBG"))),legend.only=TRUE,horizontal=TRUE)
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="")
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE,main="")
# 
# dev.off()
# ###########################################################################################################
# #                                             RESIDS
# ###########################################################################################################
# 
# dat<-read.csv("falldata.csv")
# 
# # get the convex hull
# coords <- cbind(dat$LONGITUDE,dat$LATITUDE)
# hull_points <- coords[chull(coords),]
# # convert to polygon
# hull_polygon <- hull_points %>%
#   Polygon() %>%
#   list() %>%
#   Polygons(1) %>%
#   list() %>%
#   SpatialPolygons()
# 
# shape=readOGR(dsn=".",layer="Gulf_of_Mexico_Shoreline")
# 
# crs(hull_polygon)=crs(shape)
# 
# x0=c(-90.59182, -91.43793, -91.68679, -93.67765, -94.35786, -95.28693, -95.96714, -96.36531, -96.18281, -96.13304)
# y0=c(27.99438, 27.92224, 27.83808, 27.82606, 27.77796, 27.69380, 27.52547, 26.94834, 26.40728, 25.95039)
# 
# coords0 <- cbind(x0,y0)
# 
# # convert to polygon
# cut_polygon <- coords0 %>%
#   Polygon() %>%
#   list() %>%
#   Polygons(1) %>%
#   list() %>%
#   SpatialPolygons()
# 
# crs(cut_polygon)=crs(shape)
# 
# hull_polygon=gDifference(hull_polygon,cut_polygon)
# 
# summerdata=read.csv("summerdata.csv")
# falldata=read.csv("falldata.csv")
# 
# xr <- range(falldata$LONGITUDE)
# yr <- range(falldata$LATITUDE)
# b <- round((xr[2]-xr[1])/100,2)
# 
# xmin=min(falldata$LONGITUDE)
# xmax=max(falldata$LONGITUDE)
# ymin=min(summerdata$LATITUDE)
# ymax=max(summerdata$LATITUDE)
# #
# grd <- expand.grid(x=seq(from=xr[1],to=xr[2],by=b),
#                    y=seq(from=yr[1],to=yr[2],by=b))
# #
# coordinates(grd) <- ~ x+y
# gridded(grd) <- TRUE
# 
# xr <- range(summerdata$LONGITUDE)
# yr <- range(summerdata$LATITUDE)
# b <- round((xr[2]-xr[1])/100,2)
# #
# grd2 <- expand.grid(x=seq(from=xr[1],to=xr[2],by=b),
#                     y=seq(from=yr[1],to=yr[2],by=b))
# #
# coordinates(grd2) <- ~ x+y
# gridded(grd2) <- TRUE
# 
# ######
# 
# summerdata.t1=read.csv("summerdata.t1.csv")
# summerdata.t2=read.csv("summerdata.t2.csv")
# summerdata.t3=read.csv("summerdata.t3.csv")
# falldata.t1=read.csv("falldata.t1.csv")
# falldata.t2=read.csv("falldata.t2.csv")
# falldata.t3=read.csv("falldata.t3.csv")
# 
# coordinates(falldata.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata.t3) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata.t3) <- ~ LONGITUDE+LATITUDE
# 
# 
# falldata2<-read.csv("gwr_fall_result.csv")
# summerdata2<-read.csv("gwr_summer_result.csv")
# falldata22<-read.csv("falldata.csv")
# summerdata22<-read.csv("summerdata.csv")
# 
# 
# summerdata2.t1=summerdata2[summerdata$YEAR<=1994,]
# nrow(summerdata2.t1)
# 
# falldata2.t1=falldata2[falldata$YEAR<=1994,]
# nrow(falldata2.t1)
# 
# summerdata2.t2=summerdata2[summerdata$YEAR>1994&summerdata$YEAR<=2009,]
# nrow(summerdata2.t2)
# falldata2.t2=falldata2[falldata$YEAR>1994&falldata$YEAR<=2009,]
# nrow(falldata2.t2)
# 
# summerdata2.t3=summerdata2[summerdata$YEAR>2009,]
# nrow(summerdata2.t3)
# falldata2.t3=falldata2[falldata$YEAR>2009,]
# nrow(falldata2.t3)
# 
# coordinates(falldata2.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata2.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(falldata2.t3) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata2.t1) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata2.t2) <- ~ LONGITUDE+LATITUDE
# coordinates(summerdata2.t3) <- ~ LONGITUDE+LATITUDE
# 
# ######
# 
# par(mar=c(6.1, 7.1, 4.1, 6.1))
# 
# jpeg("KrigingFall.jpg",width = 11, height = 8,units="in",res=300)
# 
# par(mfrow=c(3,3))
# 
# idwf1<-idw(formula=fit~1, locations=falldata.t1, newdata=grd)
# #
# 
# idwf1_output <- as.data.frame(idwf1)[, 1:3]
# 
# idwf1_raster <- rasterFromXYZ(idwf1_output)
# 
# idwf1_raster_crop <- mask(idwf1_raster, hull_polygon)
# 
# crs(idwf1_raster_crop)=crs(shape)
# #
# 
# 
# idw2f1<-idw(formula=pred~1, locations=falldata2.t1, newdata=grd)
# #
# 
# idw2f1_output <- as.data.frame(idw2f1)[, 1:3]
# 
# idw2f1_raster <- rasterFromXYZ(idw2f1_output)
# 
# idw2f1_raster_crop <- mask(idw2f1_raster, hull_polygon)
# 
# crs(idw2f1_raster_crop)=crs(shape)
# 
# 
# idw<-idw(formula=logrscpue~1, locations=falldata.t1, newdata=grd)
# #
# 
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# f1gam=idw_raster_crop-idwf1_raster_crop
# f1gwr=idw_raster_crop-idw2f1_raster_crop
# 
# idwf2<-idw(formula=fit~1, locations=falldata.t2, newdata=grd)
# #
# idwf2_output <- as.data.frame(idwf2)[, 1:3]
# 
# idwf2_raster <- rasterFromXYZ(idwf2_output)
# 
# idwf2_raster_crop <- mask(idwf2_raster, hull_polygon)
# 
# crs(idwf2_raster_crop)=crs(shape)
# 
# 
# #
# idw2f2<-idw(formula=pred~1, locations=falldata2.t2, newdata=grd)
# #
# 
# idw2f2_output <- as.data.frame(idw2f2)[, 1:3]
# 
# idw2f2_raster <- rasterFromXYZ(idw2f2_output)
# 
# idw2f2_raster_crop <- mask(idw2f2_raster, hull_polygon)
# 
# crs(idw2f2_raster_crop)=crs(shape)
# 
# 
# 
# idw<-idw(formula=logrscpue~1, locations=falldata.t2, newdata=grd)
# #
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# f2gam=idw_raster_crop-idwf2_raster_crop
# f2gwr=idw_raster_crop-idw2f2_raster_crop
# 
# idwf3<-idw(formula=fit~1, locations=falldata.t3, newdata=grd)
# #
# 
# idwf3_output <- as.data.frame(idwf3)[, 1:3]
# 
# idwf3_raster <- rasterFromXYZ(idwf3_output)
# 
# idwf3_raster_crop <- mask(idwf3_raster, hull_polygon)
# 
# crs(idwf3_raster_crop)=crs(shape)
# 
# idw2f3<-idw(formula=pred~1, locations=falldata2.t3, newdata=grd)
# #
# 
# idw2f3_output <- as.data.frame(idw2f3)[, 1:3]
# 
# idw2f3_raster <- rasterFromXYZ(idw2f3_output)
# 
# idw2f3_raster_crop <- mask(idw2f3_raster, hull_polygon)
# 
# crs(idw2f3_raster_crop)=crs(shape)
# 
# idw<-idw(formula=logrscpue~1, locations=falldata.t3, newdata=grd)
# #
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# f3gam=idw_raster_crop-idwf3_raster_crop
# f3gwr=idw_raster_crop-idw2f3_raster_crop
# 
# ######
# 
# idws1<-idw(formula=fit~1, locations=summerdata.t1, newdata=grd2)
# #
# idws1_output <- as.data.frame(idws1)[, 1:3]
# 
# idws1_raster <- rasterFromXYZ(idws1_output)
# 
# idws1_raster_crop <- mask(idws1_raster, hull_polygon)
# 
# crs(idws1_raster_crop)=crs(shape)
# 
# #
# idw2s1<-idw(formula=pred~1, locations=summerdata2.t1, newdata=grd2)
# #
# 
# idw2s1_output <- as.data.frame(idw2s1)[, 1:3]
# 
# idw2s1_raster <- rasterFromXYZ(idw2s1_output)
# 
# idw2s1_raster_crop <- mask(idw2s1_raster, hull_polygon)
# 
# crs(idw2s1_raster_crop)=crs(shape)
# 
# 
# 
# idw<-idw(formula=logrscpue~1, locations=summerdata.t1, newdata=grd2)
# #
# 
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# s1gam=idw_raster_crop-idws1_raster_crop
# s1gwr=idw_raster_crop-idw2s1_raster_crop
#   
# idws2<-idw(formula=fit~1, locations=summerdata.t2, newdata=grd2)
# #
# 
# idws2_output <- as.data.frame(idws2)[, 1:3]
# 
# idws2_raster <- rasterFromXYZ(idws2_output)
# 
# idws2_raster_crop <- mask(idws2_raster, hull_polygon)
# 
# crs(idws2_raster_crop)=crs(shape)
# 
# 
# idw2s2<-idw(formula=pred~1, locations=summerdata2.t2, newdata=grd2)
# #
# 
# idw2s2_output <- as.data.frame(idw2s2)[, 1:3]
# 
# idw2s2_raster <- rasterFromXYZ(idw2s2_output)
# 
# idw2s2_raster_crop <- mask(idw2s2_raster, hull_polygon)
# 
# crs(idw2s2_raster_crop)=crs(shape)
# 
# 
# idw<-idw(formula=logrscpue~1, locations=summerdata.t2, newdata=grd2)
# #
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# s2gam=idw_raster_crop-idws2_raster_crop
# s2gwr=idw_raster_crop-idw2s2_raster_crop
#   
# idws3<-idw(formula=fit~1, locations=summerdata.t3, newdata=grd2)
# #
# 
# idws3_output <- as.data.frame(idws3)[, 1:3]
# 
# idws3_raster <- rasterFromXYZ(idws3_output)
# 
# idws3_raster_crop <- mask(idws3_raster, hull_polygon)
# 
# crs(idws3_raster_crop)=crs(shape)
# 
# 
# #
# idw2s3<-idw(formula=pred~1, locations=summerdata2.t3, newdata=grd2)
# #
# 
# idw2s3_output <- as.data.frame(idw2s3)[, 1:3]
# 
# idw2s3_raster <- rasterFromXYZ(idw2s3_output)
# 
# idw2s3_raster_crop <- mask(idw2s3_raster, hull_polygon)
# 
# crs(idw2s3_raster_crop)=crs(shape)
# 
# 
# idw<-idw(formula=logrscpue~1, locations=summerdata.t3, newdata=grd2)
# #
# 
# idw_output <- as.data.frame(idw)[, 1:3]
# 
# idw_raster <- rasterFromXYZ(idw_output)
# 
# idw_raster_crop <- mask(idw_raster, hull_polygon)
# 
# crs(idw_raster_crop)=crs(shape)
# 
# s3gam=idw_raster_crop-idws3_raster_crop
# s3gwr=idw_raster_crop-idw2s3_raster_crop
# 
# ######
# 
# #plot(lat~lon,data=GRP.data) # just to look at the points
# dat1<-read.csv("falldata.t1.csv")
# dat2<-read.csv("gwr_fall1_result.csv")
# 
# ######
# 
# xmin=min(dat1$LONGITUDE)
# xmax=max(dat1$LONGITUDE)
# ymin=min(dat1$LATITUDE)
# ymax=max(dat1$LATITUDE)
# 
# #https://www.youtube.com/watch?v=b-3KLVM7ank
# colfunc <- colorRampPalette(c("#F7F7F7", "#2D004B"))
# 
# cols2=c(brewer.pal(11, "PuOr")[2:5],brewer.pal(11, "PuOr")[6],brewer.pal(11, "PuOr")[6],colfunc(7)[2:7])
# 
# shape=readOGR(dsn=".",layer="Gulf_of_Mexico_Shoreline")
# 
# ######
# 
# jpeg("FallEstOb.jpg",width = 7.25, height = 8,units="in",res=300)
# 
# par(mfrow=c(3,2))
# #GAM-GWR
# #Neg (Orange) means Obs was greater than Est
# 
# values(f1gam)[values(f1gam)>1.3]=1.3
# values(f1gam)[values(f1gam)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(f1gam, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# values(f1gwr)[values(f1gwr)>1.3]=1.3
# values(f1gwr)[values(f1gwr)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(f1gwr, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# values(f2gam)[values(f2gam)>1.3]=1.3
# values(f2gam)[values(f2gam)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(f2gam, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# values(f2gwr)[values(f2gwr)>1.3]=1.3
# values(f2gwr)[values(f2gwr)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(f2gwr, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# values(f3gam)[values(f3gam)>1.3]=1.3
# values(f3gam)[values(f3gam)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(f3gam, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# values(f3gwr)[values(f3gwr)>1.3]=1.3
# values(f3gwr)[values(f3gwr)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(f3gwr, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# dev.off()
# 
# jpeg("SummerEstOb.jpg",width = 7.25, height = 8,units="in",res=300)
# 
# par(mfrow=c(3,2))
# #GAM-GWR
# #Neg (Orange) means Obs was greater than Est
# 
# values(s1gam)[values(s1gam)>1.3]=1.3
# values(s1gam)[values(s1gam)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(s1gam, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# values(s1gwr)[values(s1gwr)>1.3]=1.3
# values(s1gwr)[values(s1gwr)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(s1gwr, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# values(s2gam)[values(s2gam)>1.3]=1.3
# values(s2gam)[values(s2gam)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(s2gam, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# values(s2gwr)[values(s2gwr)>1.3]=1.3
# values(s2gwr)[values(s2gwr)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(s2gwr, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# values(s3gam)[values(s3gam)>1.3]=1.3
# values(s3gam)[values(s3gam)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(s3gam, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# values(s3gwr)[values(s3gwr)>1.3]=1.3
# values(s3gwr)[values(s3gwr)<=-1]=-1
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(s3gwr, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-1,1.3)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1.3)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# dev.off()
# ###########################################################################################################
# #                                           DIFF FIGURE
# ###########################################################################################################
# #plot(lat~lon,data=GRP.data) # just to look at the points
# dat1<-read.csv("falldata.t1.csv")
# dat2<-read.csv("gwr_fall1_result.csv")
# 
# ######
# 
# xmin=min(dat1$LONGITUDE)
# xmax=max(dat1$LONGITUDE)
# ymin=min(dat1$LATITUDE)
# ymax=max(dat1$LATITUDE)
# 
# #https://www.youtube.com/watch?v=b-3KLVM7ank
# 
# colfunc1 <- colorRampPalette(c("#8E0152", "white"))
# colfunc2 <- colorRampPalette(c("white", "#276419"))
# 
# cols2=c(colfunc1(7)[1:6],colfunc2(7)[2:7])
# shape=readOGR(dsn=".",layer="Gulf_of_Mexico_Shoreline")
# 
# 
# ######
# 
# jpeg("Diffs.jpg",width = 7.25, height = 8,units="in",res=300)
# 
# par(mfrow=c(3,2))
# #GAM-GWR
# #Neg means GWR was greater
# idwdifff1_raster_crop=idwf1_raster_crop-idw2f1_raster_crop
# 
# values(idwdifff1_raster_crop)[values(idwdifff1_raster_crop)>0.6]=0.6
# values(idwdifff1_raster_crop)[values(idwdifff1_raster_crop)<=-0.6]=-0.6
# 
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idwdifff1_raster_crop, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-0.6,0.6)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-0.6,0.6)),
#            lab.breaks=c("","-0.5","","-0.3","","-0.1","","0.1","","0.3","","0.5",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# ######
# 
# idwdiffs1_raster_crop=idws1_raster_crop-idw2s1_raster_crop
# 
# values(idwdiffs1_raster_crop)[values(idwdiffs1_raster_crop)>0.6]=0.6
# values(idwdiffs1_raster_crop)[values(idwdiffs1_raster_crop)<=-0.6]=-0.6
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idwdiffs1_raster_crop, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-0.6,0.6)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-0.6,0.6)),
#            lab.breaks=c("","-0.5","","-0.3","","-0.1","","0.1","","0.3","","0.5",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# ######
# 
# idwdifff2_raster_crop=idwf2_raster_crop-idw2f2_raster_crop
# 
# values(idwdifff2_raster_crop)[values(idwdifff2_raster_crop)>0.6]=0.6
# values(idwdifff2_raster_crop)[values(idwdifff2_raster_crop)<=-0.6]=-0.6
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idwdifff2_raster_crop, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-0.6,0.6)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-0.6,0.6)),
#            lab.breaks=c("","-0.5","","-0.3","","-0.1","","0.1","","0.3","","0.5",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# ######
# 
# idwdiffs2_raster_crop=idws2_raster_crop-idw2s2_raster_crop
# 
# values(idwdiffs2_raster_crop)[values(idwdiffs2_raster_crop)>0.6]=0.6
# values(idwdiffs2_raster_crop)[values(idwdiffs2_raster_crop)<=-0.6]=-0.6
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idwdiffs2_raster_crop, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-0.6,0.6)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-0.6,0.6)),
#            lab.breaks=c("","-0.5","","-0.3","","-0.1","","0.1","","0.3","","0.5",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# ######
# 
# idwdifff3_raster_crop=idwf3_raster_crop-idw2f3_raster_crop
# 
# values(idwdifff3_raster_crop)[values(idwdifff3_raster_crop)>0.6]=0.6
# values(idwdifff3_raster_crop)[values(idwdifff3_raster_crop)<=-0.6]=-0.6
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idwdifff3_raster_crop, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-0.6,0.6)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-0.6,0.6)),
#            lab.breaks=c("","-0.5","","-0.3","","-0.1","","0.1","","0.3","","0.5",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# 
# ######
# 
# 
# idwdiffs3_raster_crop=idws3_raster_crop-idw2s3_raster_crop
# values(idwdiffs3_raster_crop)[values(idwdiffs3_raster_crop)>0.5]=0.6
# values(idwdiffs3_raster_crop)[values(idwdiffs3_raster_crop)<=-0.5]=-0.6
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# image(idwdiffs3_raster_crop, col=cols2,xlab="",ylab="",add=TRUE, zlim=range(c(-0.6,0.6)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-0.6,0.6)),
#            lab.breaks=c("","-0.5","","-0.3","","-0.1","","0.1","","0.3","","0.5",""),
#            col=cols2,legend.only=TRUE,horizontal=TRUE)
# dev.off()
# 
# ###################################################
# ###################################################
# 
# diff=f4_raster-f1_raster
# 
# jpeg("T1T4Diff.jpg",width = 6, height = 4,units="in",res=300)
# 
# par(mfrow=c(1,1),mar=c(4.6,2.1,3.1,2.1))
# 
# plot(c(xmin,xmax),c(ymin,ymax),pch=3,cex=0.5,
#      xlab="",ylab="",col='white',axes=FALSE)
# axis(3)
# axis(2)
# box()
# 
# 
# image(diff, col=rev(c(brewer.pal(11, "RdYlBu")[1:5],brewer.pal(11, "RdYlBu")[7:11])),
#       breaks=c(seq(-1,1,by=0.2)),xlab="",ylab="",add=TRUE,zlim=range(c(-1,1)))
# 
# plot(shape,col="grey",border="gray52",add=TRUE)
# 
# image.plot(zlim=range(c(-1,1)),
#            lab.breaks=c("-1.0","-0.8","-0.6","-0.4","-0.2","0","0.2","0.4","0.6","0.8","1.0"),
#            col=rev(c(brewer.pal(11, "RdYlBu")[1:5],brewer.pal(11, "RdYlBu")[7:11])),legend.only=TRUE,horizontal=TRUE)
# 
# dev.off()