predict_from_random_forest<-function(Y_M=1,top.n=50){
    library(randomForest)
    setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
    load("sipbInstallDates.rdata")
    rdata.folder <- "D:/SIPro/rdata/"

    yvar <- function(Y_M){
        if (Y_M==1) return("Y_1M")
        if (Y_M==3) return("Y_3M")
        if (Y_M==6) return("Y_6M")
        if (Y_M==12) return("Y_12M")
    }
    
    rf.lst<-list()
    installs.n<-length(sipbInstallDates)
    
    out<-matrix(NA,nrow = installs.n-Y_M-13,ncol=4,dimnames=list(sipbInstallDates[1:(installs.n-Y_M-13)],c("Port","Bench","Corr","Shorts")))
    
    for (i in 1:12){  #load 12 random forests
        load(paste(rdata.folder,"rf",Y_M,"M",sipbInstallDates[i],".rdata",sep=""))
        rf.lst[[i]]<-rf1
    }
    
    for (j in 2:(installs.n-13-Y_M)){
        print(j)
        for (i in 1:12){  #load 12 random forests
            if (i<12){
                rf.lst[[i]]<-rf.lst[[i+1]]
            } else {
                load(paste(rdata.folder,"rf",Y_M,"M",sipbInstallDates[j+i-1],".rdata",sep=""))    
                rf.lst[[12]]<-rf1
            }
        }
        load(paste(rdata.folder,"xdata",sipbInstallDates[j+12-1+Y_M+1],".rdata",sep=""))
        load(paste(rdata.folder,"ydata",sipbInstallDates[j+12-1+Y_M+1],".rdata",sep=""))
        
        x.df<-xdata
        y.df<-ydata[,c("COMPANY_ID","INSTALLDT",yvar(Y_M))]
        y.df<-y.df[complete.cases(y.df),]
        y.df$COMPANY_ID<-NULL
        y.df$INSTALLDT<-NULL
        
        x.df$COMPANY_ID<-NULL
        x.df$INSTALLDT<-NULL
        x.df$COMPANY<-NULL
        x.df$TICKER<-NULL
        x.df<-x.df[complete.cases(x.df),]
        
        xy.df<-merge(x.df,y.df,by="row.names")
        y.df<-data.frame(YRet=xy.df[,yvar(Y_M)],row.names=row.names(xy.df))
        x.df<-xy.df
        x.df[,yvar(Y_M)]<-NULL
        x.df$Row.names<-NULL
        y.pred<-rep(0,nrow(y.df))
        for (i in 1:12){
            y.pred<-y.pred+predict(rf.lst[[i]],x.df,type = "response")/12
        }
        yy<-data.frame(pred=y.pred,act=y.df)
        yy<-yy[order(yy$pred,decreasing = TRUE),]
        out[j,"Port"]<-mean(yy[1:top.n,"YRet"])
        out[j,"Bench"]<-mean(y.df$YRet)
        out[j,"Corr"]<-cor(y.pred,y.df$YRet)
        yy<-yy[order(yy$pred,decreasing = FALSE),]
        out[j,"Shorts"]<-mean(yy[1:top.n,"YRet"])
    }
    out<-data.frame(out)
    out$PminusB<-out$Port-out$Bench
    out$LminusS<-out$Port-out$Shorts
    return(out)
}
rdata.folder <- "D:/SIPro/rdata/"
Results1M<-predict_from_random_forest(1)
save(Results1M,file=paste(rdata.folder,"Results1M.rdata",sep=""))
Results3M<-predict_from_random_forest(3)
save(Results3M,file=paste(rdata.folder,"Results3M.rdata",sep=""))
Results6M<-predict_from_random_forest(6)
save(Results6M,file=paste(rdata.folder,"Results6M.rdata",sep=""))
Results12M<-predict_from_random_forest(12)
save(Results12M,file=paste(rdata.folder,"Results12M.rdata",sep=""))
