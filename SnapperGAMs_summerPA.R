library(mgcv)

falldata=read.csv("falldata.csv")
summerdata=read.csv("summerdata.csv")

summerdatatrain=summerdata[summerdata$Validate=="TRAIN",]
falldatatrain=falldata[falldata$Validate=="TRAIN",]

falldataonlypos=falldatatrain[falldatatrain$rscpue>0,]
summerdataonlypos=summerdatatrain[summerdatatrain$rscpue>0,]

summer.PA.interactive.modellogit<-bam(PA~s(YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(SURF.TEMP,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(SURF.CHLOR,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(BOT.OXY,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(summerdatatrain$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(GRAVEL,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(ROCK,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(SAND,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(CARBON,YEAR,k=round(0.8*length(unique(summerdatatrain$YEAR)),0))+te(REEF,YEAR,k=round(0.8*length(unique(summerdatatrain$REEF)),0))+te(LATITUDE,LONGITUDE,k=50),data=summerdatatrain,family = binomial(link=logit),select=TRUE,discrete=TRUE,nthreads=2)

saveRDS(summer.PA.interactive.modellogit,"summer.PA.interactive.modellogit.rds")