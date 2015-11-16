variable_zscore <- function(i,Y_M=1,YType="RET",std.err) { 
    #YType may be RET for a return or SHRP for return/standard deviation
    library(SDMTools)
    source("SMWUtilities.r")
    init_environment()
    print(i)
    se.bootstrap<-std.err[,i]
    top.n<-50
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
    rm(x.df,y.df,xdata,ydata)
    row.names(xy.df) <- xy.df[,"Row.names"]
    xy.df$Row.names <- NULL
    if (YType == "SHRP"){
        xy.df$YRet <- xy.df$YRet / (xy.df$PRCHG_SD3Y / sqrt(12/Y_M)) # convert annualized SD to Y_M periodicity
    }
    # get rid of Inf values - this should be in the CreateData script
    xy.df[xy.df == Inf]<-NA
    xy.df[xy.df == -Inf]<-NA
    se.bootstrap[length(se.bootstrap)+1]<-sd(xy.df$YRet,na.rm = TRUE)/sqrt(top.n)
    names(se.bootstrap)[length(se.bootstrap)]<-"YRET"
    xy.df<-xy.df[order(xy.df$YRet,decreasing=TRUE),]
    xy.top<-head(xy.df,top.n)
    xy.bot<-tail(xy.df,top.n)
    mkt.wtd.mean<-apply(xy.df[,6:ncol(xy.df)],2,wt.mean,wt=xy.df$MKTCAP)
    
    top.wtd.mean<-apply(xy.top[,6:ncol(xy.top)],2,wt.mean,wt=xy.top$MKTCAP)
    bot.wtd.mean<-apply(xy.bot[,6:ncol(xy.bot)],2,wt.mean,wt=xy.bot$MKTCAP)
    top.z<-(top.wtd.mean - mkt.wtd.mean) / se.bootstrap
    bot.z<-(bot.wtd.mean - mkt.wtd.mean) / se.bootstrap
    all.z<-((top.wtd.mean - bot.wtd.mean) / se.bootstrap)/2
    pctna<-apply(xy.df[,6:ncol(xy.df)],2,function(x) sum(is.na(x)))/nrow(xy.df) #percent of variable NA
    #fname.prefix<-paste0(rdata.folder,"roo",Y_M,"M",YType)
    #save(,file = paste0(fname.prefix,strDate,".rdata"))
    return(list(top.z<-top.z,bot.z<-bot.z,all.z<-all.z,pctna<-pctna))
}

create_zscores_from_each_install <- function() {
    setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
    source("SMWUtilities.r")
    init_environment()
    std.err<-readRDS(paste0(rdata.folder,"sebootstrap.rds"))
    Y_M<-1
    YType<-"RET"
    zscores.lst<-lapply(seq(1:(length(sipbInstallDates) - Y_M)),variable_zscore,Y_M=Y_M,YType=YType,std.err)
    names(zscores.lst)<-sipbInstallDates[1:(length(sipbInstallDates) - Y_M)]
    saveRDS(zscores.lst,file="zscores.rds")
    return(zscores.lst)
}

Lag1pvalue<-function(i,test){
    data<-as.vector(t(test[i,]))
    n<-ncol(test)
    lmfit<-lm(data=data.frame(data[1:(n-1)],data[2:n]))
    alm<-anova(lmfit)
    return(alm$`Pr(>F)`[1])
}

evalz<-function(){
    setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
    source("SMWUtilities.r")
    init_environment()
    Y_M<-1
    YType<-"RET"
    out<-list()
    zscores.lst<-readRDS("zscores.rds")
    #print(zscores.lst[[1]][[3]][1:3])
    #print(zscores.lst[[2]][[3]][1:3])
    all.z<-lapply(seq(1:length(zscores.lst)),function(x) zscores.lst[[x]][[3]])  
    test<-data.frame(matrix(unlist(all.z),nrow=length(all.z[[1]]),byrow=F))
    colnames(test)<-sipbInstallDates[1:(length(sipbInstallDates)-1)]
    rownames(test)<-names(zscores.lst[[1]][[3]])
    test<-test[-nrow(test),]
    test2<-data.matrix(test)
    fldavgz<-apply(test,1,median,na.rm=T)
    out$nmonths<-dim(test)[2]
    out$nvar<-dim(test)[1]
    out$maxavgz<-max(fldavgz,na.rm=T)
    out$minavgz<-min(fldavgz,na.rm=T)
    out$maxfld<-names(which.max(fldavgz))
    out$minfld<-names(which.min(fldavgz))
    library(RColorBrewer)
    pal<-brewer.pal(n=11,name="RdYlGn")
    hm<-heatmap(test2,Rowv=NA,Colv=NA,col=pal,main="Heat Map of z-values",ylab="Variable",xlab="Month")
    out$hm<-hm
    
    temp<-sapply(seq(1:683),Lag1pvalue,test=test)    
    names(temp)<-rownames(test2)
    temp<-temp[order(temp)]
    head(temp)    
    pval.plot<-plot(temp,col="blue",main="p-values of 1st order lags",
         sub="text indicates # significat at % level",
         xlab = "variable", ylab="p-value",type="h",
         ylim = c(0,1))
    n01<-sum(temp<=0.01)
    n05<-sum(temp<=0.05)
    n10<-sum(temp<=0.1)
    abline(v=n01, col="gray")
    abline(v=n05, col="gray")
    abline(v=n10, col="gray")
    labs<-c(paste(n01,"at 1%"),paste(n05,"at 5%"),
            paste(n10,"at 10%"))
    text(c(n01,n05,n10),c(.10,.20,.30),labels=labs)
    out$nsignvar<-c(n01,n05,n10)
    out$pval.plot<-pval.plot
    return(out)
}

bootstrapstderr<-function(i=1,sample.n=3000,top.n=50){
    setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
    library(SDMTools)
    source("SMWUtilities.r")
    init_environment()
    strDate<-sipbInstallDates[i]
    load(paste(rdata.folder,"xdata",sipbInstallDates[i],".rdata",sep = "")) # load xdata file
    xdata<-xdata[,sapply(xdata,class)=="numeric"]
    set.seed(101)
    generate_sample_means<-function(){
        sample.idx<-sample(1:nrow(xdata), top.n,  replace=FALSE)
        thissample<-xdata[sample.idx,]
        samplemean<-apply(thissample,2,wt.mean,wt=thissample$MKTCAP)    
    }
    
    #samplemeans<-matrix(NA,nrow = samples.n,ncol = ncol(xdata))
    #for (i in 1:samples.n){
    #    samplemeans[i,]<-generate_sample_means()    
    #}
    samplemeans<-t(replicate(sample.n,generate_sample_means()))
    out<-apply(samplemeans,2,sd)
}

#se.bootstrap<-sapply(1:length(sipbInstallDates),bootstrapstderr)
