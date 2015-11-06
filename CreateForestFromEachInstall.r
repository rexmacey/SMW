CreateRF <- function(i,Y_M=1,YType="RET") { 
    #YType may be RET for a return or SHRP for return/standard deviation
    library(randomForest)
    source("SMWUtilities.r")
    init_environment()
    strDate<-sipbInstallDates[i]
    load(paste(rdata.folder,"xdata",sipbInstallDates[i],".rdata",sep = "")) # load xdata file
    load(paste(rdata.folder,"ydata",sipbInstallDates[i],".rdata",sep = "")) # load ydata file
    #combine xdata and ydata files into 1 file.
    x.df <- xdata
    x.df$COMPANY_ID <- NULL  #this field is not used in a model
    x.df$INSTALLDT <- NULL
    x.df$COMPANY <- NULL
    x.df$TICKER <- NULL
    
    y.df <- ydata[,c("COMPANY_ID","INSTALLDT",yvar(Y_M))]
    y.df$COMPANY_ID <- NULL
    y.df$INSTALLDT <- NULL
    colnames(y.df)<-"YRet"

    xy.df <- merge(x.df,y.df,by = "row.names")
    rm(x.df,y.df)
    row.names(xy.df) <- xy.df[,"Row.names"]
    xy.df$Row.names <- NULL
    #remove missing y values
    xy.df <- xy.df[complete.cases(xy.df),]
    if (YType == "SHRP"){
        xy.df$YRet <- xy.df$YRet / (xy.df$PRCHG_SD3Y / sqrt(12/Y_M)) # convert annualized SD to Y_M periodicity
    }
    # get rid of Inf values - this should be in the CreateData script
    idx.inf <- apply(xy.df[,6:ncol(xy.df)],2,max)==Inf
    for (i in 1:length(idx.inf)){
        if (idx.inf[i]){
            xy.df[,i+5]<-ReplInfWithMax(xy.df[,i+5])
        }
    }
    rf1 <- randomForest(YRet ~ .,data = xy.df,ntree = 150)
    fname.prefix<-paste0(rdata.folder,"rf",Y_M,"M",YType)
    save(rf1,file = paste0(fname.prefix,strDate,".rdata"))
    return(rf1)
}

create_forest_from_each_install <- function(Y_M = 1,YType="RET") {
#    lapply(seq(1:(length(sipbInstallDates) - Y_M)),CreateRF,Y_M=Y_M,YType=YType)
#     setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
#     load("sipbInstallDates.rdata")
#     rdata.folder <- "D:/SIPro/rdata/"
#     fname.prefix<-paste(rdata.folder,"rf",Y_M,"M",YType,sep = "")
    source("SMWUtilities.r")
    init_environment()
    set.seed(101) # fore reproducibility
    for (i in 1:(length(sipbInstallDates) - Y_M)) {
         print(paste(Y_M,i))
         rf1 <- CreateRF(i,Y_M,YType)
     }
}




