getwd()
setwd("C:/Users/Public/Documents/ESDA/Energy Data Analytics/Project/Project/Luke-Skywalker")

install.packages(pbapply)
library(tidyverse)
library(pbapply)

pnl <- read.csv("Power-Networks-LCL-June2015(withAcornGps)v2.csv",stringsAsFactors = FALSE)

### pnl <- rbind(pnl,pnl1)
dim(pnl)
View(pnl)

pnl$stdorToU <- as.factor(pnl$stdorToU)
pnl$Acorn <- as.factor(pnl$Acorn)
pnl$Acorn_grouped <- as.factor(pnl$Acorn_grouped)

plot(pnl$stdorToU,pnl$KWH.hh..per.half.hour.)

a <- data.frame(c(3,4,5),c(7,8,9))
b <- data.frame(c(1,7,2),c(0,3,10))

names(a)[1] <- paste("One")
names(a)[2] <- paste("Two")

