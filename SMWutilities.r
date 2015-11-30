# Utility function for SMW

init_environment<-function(){
    library(yaml)
    setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
    sipbInstallDates<-yaml.load_file("sipbInstallDates.yaml")
    assign("sipbInstallDates",sipbInstallDates,envir = .GlobalEnv)
    assign("rdata.folder","D:/SIPro/rdata/",envir = .GlobalEnv)
    assign("mainfolder","D:/SIPro",envir=.GlobalEnv)
}

xdata_prep<-function(x){
    #input an xdata file. Remove character fields and Install Date
    # for testing load(paste0(rdata.folder,"xdata20030103.rdata"))
    # for testing x<-xdata
    vtypes<-sapply(x,class) #remove character columns
    x<-x[,!vtypes=="character"]
    x<-ReplInfWithNA(x)
}

combine_xy<-function(i,Y_M=1){ 
    # loads xdata, removes character variables, replaced Inf w NA
    # combines with ydata
    load(paste0(rdata.folder,"xdata",sipbInstallDates[i],".rdata"))
    x<-xdata_prep(xdata)
    load(paste0(rdata.folder,"ydata",sipbInstallDates[i],".rdata"))
    y <- ydata[,c("COMPANY_ID","INSTALLDT",yvar(Y_M))]
    y <- y[complete.cases(y),]
    y$COMPANY_ID <- NULL
    y$INSTALLDT <- NULL
    xy <- merge(x,y,by = "row.names")
    rownames(xy)<-xy$Row.names
    xy$Row.names<-NULL
    names(xy)[ncol(xy)]<-"YRet"
    return(xy)
}

ReplNAwMedian<-function(x){
    idx <- is.na(x)
    x[idx]<-median(x,na.rm=TRUE)
    return(x)
}

ReplInfWithNA<-function(x){ # replace Inf values with median
    idx <- x == Inf | x == -Inf
    x[idx] <- NA
    return(x)
}

ReplInfWithMedian<-function(x){ # replace Inf values with median
    idx <- x == Inf | x == -Inf
    x[idx] <- median(x[!idx])
    return(x)
}

yvar <- function(Y_M) {
    if (Y_M == 1)
        return("Y_1M")
    if (Y_M == 3)
        return("Y_3M")
    if (Y_M == 6)
        return("Y_6M")
    if (Y_M == 12)
        return("Y_12M")
}


generate_returns<-function(strDate){
    #calculate one month of returns, update returns.monthly and save
    #
        if (! file.exists(paste0(rdata.folder,"returnsmonthly.rdata"))){
            returns.monthly<-data.frame(matrix(ncol = 7, nrow = 0,
            dimnames=list(NULL,c("COMPANY_ID","PRICEDM001","PRICEDM002","RET","INSTALLDT","PRICE_M001","PRICE_M002"))))
        } else {
            if (!exists("returns.monthly")) load(paste(rdata.folder,"returnsmonthly.rdata",sep=""))
        }
        library(foreign)
        print(as.character(strDate))
        sipfolder<-dbflocations(strDate)
        si_psdc <- loadfile("si_psdc.dbf", sipfolder["dbfs"],c("COMPANY_ID","PRICE_M001","PRICE_M002"))
        si_psdd <- loadfile("si_psdd.dbf", sipfolder["dbfs"],c("COMPANY_ID","PRICEDM001","PRICEDM002"))
        z<-merge(si_psdd,si_psdc,by="COMPANY_ID")
        z[,"PRICE_M001"] <- as.numeric(z[,"PRICE_M001"])
        z[,"PRICE_M002"] <- as.numeric(z[,"PRICE_M002"])
        z<-z[complete.cases(z),]
        z<-z[z$PRICE_M001>0 & z$PRICE_M002>0,]
        z$RET <- 100*(z$PRICE_M001/z$PRICE_M002-1)
        z<-z[!duplicated(z$COMPANY_ID),]
        z$INSTALLDT <- strDate
        returns.monthly<-rbind(returns.monthly,z)
        returns.monthly <-
            returns.monthly[!duplicated(returns.monthly[,c("COMPANY_ID","PRICEDM001","PRICEDM002")]),]
    save(returns.monthly,file="returnsmonthly.rdata")
    return(returns.monthly)
}

dbflocations <- function(strDate){
    datepath <- paste0(mainfolder, "/SIP", strDate)
    subfolders <- c("Dbfs", "Datadict", "Static")
    out <- file.path(datepath, subfolders)
    
    ## FD: I feel you are adding unnecessary complexity by switching the names to lowercase
    ##     I would have kept things as-is, e.g. out$Dbfs instdead of out$dbfs
    names(out) <- tolower(subfolders)
    
    return(out) ## FD: This is now a named vector (more appropriate), not a list
}

stock_count_by_month<-function(){ # count of number of stocks in each si_ci file (All) and each in the xdata file
    # counts the number of stocks 
    out<-matrix(NA,nrow=length(sipbInstallDates),ncol=2)
    rownames(out)<-sipbInstallDates
    colnames(out)<-c("All","Filtered")
    for (i in seq_along(sipbInstallDates)){
        fn<-paste0("SIP",sipbInstallDates[i],"/Static/si_ci.dbf")
        data <- read.dbf(file = file.path(mainfolder, fn), as.is = TRUE)
        out[i,"All"]<-nrow(data)
        load(paste0(rdata.folder,"xdata",sipbInstallDates[i],".rdata"))
             out[i,"Filtered"]<-nrow(xdata)
    }
    summary(out)
    nmonths<-length(sipbInstallDates)
    plot(out[,1],type="h",col="gray",main="Number of Stocks in Universe",ylim=c(0,max(out[,1])),
         ylab="Count",xlab="Date",axes=FALSE)
    points(out[,2],col="blue",type="l")
    axis(1,labels=sipbInstallDates[seq(1,nmonths,12)],pos=0,at=seq(1,nmonths,12),las=0)
    axis(2)
    legend(0,max(out[,1]),legend=c("# in SIP Universe","# w Price>=$5 and Cap>=$250MM"),
           col=c("gray","blue"),pch=c(22,32),text.col=c("gray","blue"),lty=c(0,1))
}

returns_count_by_month<-function(){
    Number_of_Returns<-function(i){
        load(paste(rdata.folder,"ydata",sipbInstallDates[i],".rdata",sep = "")) # load ydata file
        out<-c(sum(!is.na(ydata$Y_1M)),sum(!is.na(ydata$Y_3M)),sum(!is.na(ydata$Y_6M)),sum(!is.na(ydata$Y_12M)))
    }
    nyret<-sapply(seq(1,155),Number_of_Returns)
    plot(nyret[1,], col="red", "Returns per month")
    lines(nyret[2,],col="orange")
    lines(nyret[3,],col="green")
    lines(nyret[4,],col="blue")
    return(nyret)
} 

rsq_by_month<-function(){
    rsq_for_month<-function(i){
        print(i)
        fn<-paste0(rdata.folder,"rf1MRET",sipbInstallDates[i],".rdata")
        if (file.exists(fn)){
            load(fn)
            rsq1m <- rf1$rsq[length(rf1$rsq)]
        } else {
            rsq1m<-NA
        }
        fn<-paste0(rdata.folder,"rf12MRET",sipbInstallDates[i],".rdata")
        if (file.exists(fn)){
            load(fn)
            rsq12m <- rf1$rsq[length(rf1$rsq)]
        } else {
            rsq12m<-NA
        }
        return(c(rsq1m,rsq12m))
    }
    nmonths<-length(sipbInstallDates)
    rsq_data<-sapply(seq(1,nmonths),rsq_for_month)
    plot(rsq_data[1,],col="blue",type="p",main="R-square of RFs",ylim=c(0,1),axes=FALSE,pch=16,ylab="R-Square")
    points(rsq_data[2,],col="green",pch=18)
    axis(1,labels=sipbInstallDates[seq(1,nmonths,12)],pos=-.05,at=seq(1,nmonths,12),las=0)
    axis(2)
    legend(0,max(rsq_data),legend=c("Response=1M","Response=12M"),
           text.col=c("blue","green"),lty=c(0,0),
           pch=c(16,18),col=c("blue","green"))
}

NumericColumns<-function(x){
    out<-sapply(xdata,class)=="numeric"
}

loadfile<-function(fn,folder,flds=NULL){
    # loads the fields (all fields if flds==NULL) from dbf fn in folder
    if (is.null(flds)){
        data<-read.dbf(file=paste0(folder,"/",fn),as.is = T)    
    } else {
        data<-read.dbf(file=paste0(folder,"/",fn),as.is = T)[,flds]
    }
    return(data)
}

na_freq_over_time<-function(fld){ # calc and plot % of na values for a field for each install 
    n.dates<-length(sipbInstallDates)
    out<-vector("numeric",n.dates)
    for (i in 1:n.dates){
        sdate<-sipbInstallDates[i]
        x<-readRDS(paste0(rdata.folder,"sip_",sdate,".rds"))[fld]
        out[i]<-100*apply(is.na(x),2,sum)/nrow(x)
    }
    s1<-paste0("Max = ",round(out[which.max(out)],2)," (",sipbInstallDates[which.max(out)],")")
    s2<-paste0("Min = ",round(out[which.min(out)],2)," (",sipbInstallDates[which.min(out)],")")
    plot(out,xlab="Installation #",ylab="% NA",main=paste("Percent NA for",fld))
    text(x=c(25,25),y=c(out[which.max(out)],out[which.min(out)]),labels=c(s1,s2))
    return(out)
}

attr(na_freq_over_time,"comment") <-
    "na_freq_over_time is useful for variable exploration. It calculates and plots the % of na values for a field for each install"