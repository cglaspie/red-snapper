install.packages('plotrix')
library(plotrix)

x=c(6,4,7,8,3,6,6,2,4)

y=std.error(x,na.rm)

write.csv(y, file = "test.csv", row.names = FALSE)

