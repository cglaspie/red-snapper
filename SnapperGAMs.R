library(mgcv)

falldata=read.csv("falldata.csv")
summerdata=read.csv("summerdata.csv")

summerdatatrain=summerdata[summerdata$Validate=="TRAIN",]
falldatatrain=falldata[falldata$Validate=="TRAIN",]

falldataonlypos=falldatatrain[falldatatrain$rscpue>0,]
summerdataonlypos=summerdatatrain[summerdatatrain$rscpue>0,]

fall.PA.interactive.modellogit<-bam(PA~s(YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(SURF.TEMP,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(BOT.TEMP,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(SURF.CHLOR,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(BOT.OXY,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(falldatatrain$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(GRAVEL,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(ROCK,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(SAND,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(CARBON,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(REEF,YEAR,k=round(0.8*length(unique(falldatatrain$REEF)),0))+te(LATITUDE,LONGITUDE,k=50),data=falldatatrain,family = binomial(link=logit),select=TRUE,discrete=TRUE,nthreads=2)

saveRDS(fall.PA.interactive.modellogit,"fall.PA.interactive.modellogit.rds")

summer.full.interactive.modellog<-bam(logrscpue~s(YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(SURF.TEMP,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(SURF.CHLOR,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(BOT.OXY,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(summerdataonlypos$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(GRAVEL,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(ROCK,YEAR,k=round(0.8*length(unique(summerdataonlypos$ROCK)),0))+te(SAND,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(CARBON,YEAR,k=round(0.8*length(unique(summerdataonlypos$YEAR)),0))+te(REEF,YEAR,k=round(0.8*length(unique(summerdataonlypos$REEF)),0))+te(LATITUDE,LONGITUDE,k=50),data=summerdataonlypos,family=gaussian(link = "log"),select=TRUE,discrete=TRUE,nthreads=8)

saveRDS(summer.full.interactive.modellog,"summer.full.interactive.modellog.rds")

summer.PA.interactive.modellogit<-bam(PA~s(YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(SURF.TEMP,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(SURF.CHLOR,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(BOT.OXY,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(summerdatatrain$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(GRAVEL,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(ROCK,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(SAND,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(CARBON,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(REEF,YEAR,k=round(0.8*length(unique(summerdatatrain$REEF)),0))+te(LATITUDE,LONGITUDE,k=50),data=summerdatatrain,family = binomial(link=logit),select=TRUE,discrete=TRUE,nthreads=2)

saveRDS(summer.PA.interactive.modellogit,"summer.PA.interactive.modellogit.rds")
