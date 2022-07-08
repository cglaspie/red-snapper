library(plotrix,lib.loc="/tmp/Rtmpdu7ygb/downloaded_packages",warn.conflicts=FALSE)

x=c(6,4,7,8,3,6,6,2,4)

y=std.error(x,na.rm)

write.csv(y, file = "test.csv", row.names = FALSE)

