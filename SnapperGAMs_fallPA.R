library(mgcv)

falldata=read.csv("falldata.csv")
summerdata=read.csv("summerdata.csv")

summerdatatrain=summerdata[summerdata$Validate=="TRAIN",]
falldatatrain=falldata[falldata$Validate=="TRAIN",]

falldataonlypos=falldatatrain[falldatatrain$rscpue>0,]
summerdataonlypos=summerdatatrain[summerdatatrain$rscpue>0,]

fall.PA.interactive.modellogit<-bam(PA~s(YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(SURF.TEMP,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(BOT.TEMP,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(BOT.SAL,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(SURF.CHLOR,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(BOT.OXY,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+s(BATHYMETRY,k=25)+s(RUGOSITY,k=25)+te(PLATFORM,YEAR,k=round(0.8*length(unique(falldatatrain$PLATFORM)),0))+te(PIPELINE,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(GRAVEL,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(ROCK,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(SAND,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(CARBON,YEAR,k=round(0.8*length(unique(falldatatrain$YEAR)),0))+te(REEF,YEAR,k=round(0.8*length(unique(falldatatrain$REEF)),0))+te(LATITUDE,LONGITUDE,k=50),data=falldatatrain,family = binomial(link=logit),select=TRUE,discrete=TRUE,nthreads=8)

saveRDS(fall.PA.interactive.modellogit,"fall.PA.interactive.modellogit.rds")
