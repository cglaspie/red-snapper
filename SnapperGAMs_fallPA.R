library(mgcv)

falldata=read.csv("falldata.csv")
summerdata=read.csv("summerdata.csv")

summerdatatrain=summerdata[summerdata$Validate=="TRAIN",]
falldatatrain=falldata[falldata$Validate=="TRAIN",]

falldataonlypos=falldatatrain[falldatatrain$rscpue>0,]
summerdataonlypos=summerdatatrain[summerdatatrain$rscpue>0,]

fall.PA.interactive.modellogit<-bam(PA~s(YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+s(SURF.TEMP,k=25)+te(BOT.TEMP,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+s(SURF.CHLOR,k=25)+te(BOT.OXY,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(falldatatrain$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+s(SAND,k=25)+s(REEF,k=10)+te(LATITUDE,LONGITUDE,k=50),data=falldatatrain,family = binomial(link=logit),select=TRUE,discrete=TRUE,nthreads=8)

saveRDS(fall.PA.interactive.modellogit,"fall.PA.interactive.modellogit.rds")
