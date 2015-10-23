library(randomForest)
setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
load("sipbInstallDates.rdata")
rdata.folder <- "D:/SIPro/rdata/"

load(paste(rdata.folder,"xtrain.rdata",sep=""))
load(paste(rdata.folder,"ytrain.rdata",sep=""))
rf.lst<-list()
for (j in 2:2){
    for (i in 1:12){  #load 12 random forests
        load(paste(rdata.folder,"rf",sipbInstallDates[j+i-1],".rdata",sep=""))
        rf.list[[i]]<-rf1
    }
    x.df<-xtrain[[j+12-1+13]]
    y.df<-ytrain[[j+12-1+13]][c("COMPANY_ID","INSTALLDT","Y_12M")]
    
}
