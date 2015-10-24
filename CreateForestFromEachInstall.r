yvar <- function(Y_M){
    if (Y_M==1) return("Y_1M")
    if (Y_M==3) return("Y_3M")
    if (Y_M==6) return("Y_6M")
    if (Y_M==12) return("Y_12M")
}

CreateRF<-function(i){
    load(paste(rdata.folder,"xdata",sipbInstallDates[i],".rdata",sep=""))
    load(paste(rdata.folder,"ydata",sipbInstallDates[i],".rdata",sep=""))
    x.df<-xdata
    x.df$COMPANY_ID<-NULL
    x.df$INSTALLDT<-NULL
    x.df$COMPANY<-NULL
    x.df$TICKER<-NULL
    
    y.df<-ydata[,c("COMPANY_ID","INSTALLDT",yvar(Y_M))]
    y.df$COMPANY_ID<-NULL
    y.df$INSTALLDT<-NULL
    
    xy.df<-merge(x.df,y.df,by="row.names")
    rm(x.df,y.df)
    row.names(xy.df)<-xy.df[,"Row.names"]
    xy.df$Row.names<-NULL
    
    #remove missing y values
    xy.df<-xy.df[complete.cases(xy.df),]
    if (Y_M==1) {rf1<-randomForest(Y_1M ~.,data=xy.df,ntree=150)}
    if (Y_M==3) {rf1<-randomForest(Y_3M ~.,data=xy.df,ntree=150)}
    if (Y_M==6) {rf1<-randomForest(Y_6M ~.,data=xy.df,ntree=150)}
    if (Y_M==12) {rf1<-randomForest(Y_12M ~.,data=xy.df,ntree=150)}
    return(rf1)
}

create_forest_from_each_install<-function(Y_M=1){
    library(randomForest)
    setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
    load("sipbInstallDates.rdata")
    rdata.folder <- "D:/SIPro/rdata/"
    
    for (i in 1:(length(sipbInstallDates)-Y_M)){
        print(i)
        rf1<-CreateRF(i)
        save(rf1,file=paste(rdata.folder,"rf",Y_M,"M",sipbInstallDates[i],".rdata",sep=""))
    }
}

#create_forest_from_each_install(1)
create_forest_from_each_install(3)
create_forest_from_each_install(6)
#create_forest_from_each_install(12)