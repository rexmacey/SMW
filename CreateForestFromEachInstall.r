library(randomForest)
setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
load("sipbInstallDates.rdata")
rdata.folder <- "D:/SIPro/rdata/"

load(paste(rdata.folder,"xtrain.rdata",sep=""))
load(paste(rdata.folder,"ytrain.rdata",sep=""))

for (i in 1:length(xtrain)){
    #    print(i)
    x.df<-xtrain[[i]]
    idx.dup<-duplicated(x.df)
    x.df<-x.df[!idx.dup,]
    rm(idx.dup)
    row.names(x.df)<-paste(x.df[,"COMPANY_ID"],x.df[,"INSTALLDT"],sep="")
    x.df$COMPANY_ID<-NULL
    x.df$INSTALLDT<-NULL
    x.df$COMPANY<-NULL
    x.df$TICKER<-NULL
    
    y.df<-ytrain[[i]][c("COMPANY_ID","INSTALLDT","Y_12M")]
    idx.dup<-duplicated(y.df)
    y.df<-y.df[!idx.dup,]
    rm(idx.dup)
    row.names(y.df)<-paste(y.df[,"COMPANY_ID"],y.df[,"INSTALLDT"],sep="")
    y.df$COMPANY_ID<-NULL
    y.df$INSTALLDT<-NULL
    
    xy.df<-merge(x.df,y.df,by="row.names")
    rm(x.df,y.df)
    row.names(xy.df)<-xy.df[,"Row.names"]
    xy.df$Row.names<-NULL
    
    #fix SP variable
    idx<-xy.df$SP=="NA"
    levels(xy.df$SP) <- c(levels(xy.df$SP),"x")
    xy.df$SP[idx]<-"x"  # replace NA with x
    rm(idx)
    #remove missing y values
    xy.df<-xy.df[complete.cases(xy.df),]
    xy.df$ADR<-factor(xy.df$ADR)
    xy.df$OPTIONABLE<-factor(xy.df$OPTIONABLE)
    for (v in names(xy.df)){  # replace infinites with median of other variables
        if (typeof(xy.df[,v])=="double"){
            idx<-is.infinite(xy.df[,v])
            xy.df[idx,v]<-median(xy.df[!idx,v])
        }
    }

    rf1<-randomForest(Y_12M ~.,data=xy.df,ntree=150)
    save(rf1,file=paste(rdata.folder,"rf",sipbInstallDates[i],".rdata",sep=""))
}