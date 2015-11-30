# Create data files (X,Y)
library(reshape2)
setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
source("SMWUtilities.r")
init_environment()

load(paste(rdata.folder,"id_translate.rdata",sep=""))  #see note in create_103to104_lookuptable() 

load_fields_from_file<-function(fn,rdata.folder,strDate,fldlist=NULL){
    load(paste(rdata.folder,fn,"_",strDate,".rdata",sep=""))
    if (is.null(fldlist)){ #load all fields
        return(data)
    } else {
        fldlist<-unlist(strsplit(fldlist,", "))
        return(data[,fldlist])
    }
}

subforna<-function(x,subval){
    if (!is.na(subval)){
        #substitute for NA.    
        if (subval=="median"){
            x[is.na(x)]<-median(x[!is.na(x)])
        } else if (subval=="mean") {
            x[is.na(x)]<-mean(x[!is.na(x)])
        } else {
            x[is.na(x)]<-subval    
        }
    }
    return(x)
}
  
get_si_selected_fld_list<-function(){ 
    library(yaml)
    si_selected_flds<-yaml.load_file("si_selected_fields.yaml")
    return(si_selected_flds)
}

get_var_definitions <- function() {
    def_df <- read.csv(file = "var_definitions.csv", stringsAsFactors = FALSE)
    def_list <- split(def_df, def_df$GROUP)
    return(def_list)
}

get_group_data <- function(strDate, group) {
    fields <- get_si_selected_fld_list()[[group]]
    fn <- sprintf("si_%s", group)
    data <- load_fields_from_file(fn = fn, rdata.folder = rdata.folder, strDate = strDate, fldlist = fields)
    def_df <- get_var_definitions()[[group]]
    out <- list()
    for (i in seq_len(nrow(def_df))) {
        def <- def_df[i, ]
        out[[def$VARNAME]] <- eval(parse(text = def$FORMULA), envir = data)
        if (def$SUBFORNA) out[[def$VARNAME]] <- subforna(x = out[[def$VARNAME]], subval = def$SUBVAL)
    }
    out <- as.data.frame(out, stringsAsFactors = FALSE)
}

get_ci_data <- function(strDate, NAvalue=NA) {
    out<-get_group_data(strDate, group = "ci")
    # convert appropriate variables to factors
    for (x in c("IND_2_DIG","SP","EXCHANGE")){
        out[,x] <- factor(out[,x])
    }
    # needed to ensure consistency across all installs
    levels(out$SP) <- c("400","500","600","x") # needed to ensure consistency across all installs
    levels(out$EXCHANGE) <- c("A","M","N","O","x")
    levels(out$IND_2_DIG)<-c("01","02","03","04","05","06","07","08","09","10","11","12","x")
    out$ADR<-factor(out$ADR)
    out$OPTIONABLE<-factor(out$OPTIONABLE)
    return(out)
}
get_mlt_data <- function(strDate, NAvalue=NA) get_group_data(strDate, group = "mlt")   # NAvalue is ignored but kept so the code won't break
get_rat_data <- function(strDate, NAvalue=NA) get_group_data(strDate, group = "rat")   # NAvalue is ignored but kept so the code won't break

get_gr_data<- function(strDate, NAvalue=NA) get_group_data(strDate, group = "gr")
get_psd_data<- function(strDate, NAvalue=NA) get_group_data(strDate, group = "psd")
get_ee_data<- function(strDate, NAvalue=NA) get_group_data(strDate, group = "ee")
get_perc_data<- function(strDate, NAvalue=NA) get_group_data(strDate, group = "perc")

get_gross_profit<-function(strDate,NAvalue=NA){
    si_flds <- get_si_selected_fld_list()[["isa"]]
    isa<-load_fields_from_file("si_isa",rdata.folder,strDate,si_flds)
    si_flds <- get_si_selected_fld_list()[["bsa"]]
    bsa<-load_fields_from_file("si_psd",rdata.folder,strDate,si_flds)
    temp<-merge(isa,bsa,by="COMPANY_ID")
    
    out<-data.frame(temp$COMPANY_ID)
    colnames(out)<-"COMPANY_ID"
    out$GPMA_Y1 <- (temp$SALES_Y1 - temp$CGS_Y1)/temp$ASSETS_Y1
    out$GPMA_Y3 <- (temp$SALES_Y3 - temp$CGS_Y3)/temp$ASSETS_Y3
    out$GPMA_Y5 <- (temp$SALES_Y5 - temp$CGS_Y5)/temp$ASSETS_Y5
    out$GPMA_Y1_Y3 <- out$GPMA_Y1 - out$GPMA_Y3
    out$GPMA_Y1_Y5 <- out$GPMA_Y1 - out$GPMA_Y5 
    if (! is.na(NAvalue)){
        out$GPMA_Y1 <- subforna(out$GPMA_Y1,NAvalue)
        out$GPMA_Y3 <- subforna(out$GPMA_Y3,NAvalue)
        out$GPMA_Y5 <- subforna(out$GPMA_Y5,NAvalue)
        out$GPMA_Y1_Y3 <- subforna(out$GPMA_Y1_Y3,NAvalue)
        out$GPMA_Y1_Y5 <- subforna(out$GPMA_Y1_Y5,NAvalue)    
    }
    return(out)
}

get_ep<-function(strDate){
    isa<-load_fields_from_file("si_isa",rdata.folder,strDate)
    psd<-load_fields_from_file("si_psd",rdata.folder,strDate)
    mlt<-load_fields_from_file("si_mlt",rdata.folder,strDate)
    psd2<-load(paste(rdata.folder,"sip_prices","_",strDate,".rdata",sep=""))
    temp<-merge(isa,psd,by="COMPANY_ID")
    temp<-merge(temp,mlt,by="COMPANY_ID")
    out<-data.frame(temp$COMPANY_ID)
    colnames(out)<-"COMPANY_ID"
    out$EP <- 100*isa$EPSDC_12M/psd$PRICE # better then mlt$EYIELD_12M because far fewer NAs
    out$cPE <- 1/out$EP
    out$PE <- mlt$PE
    
    out$EP_1T <- 100*isa$EPSDC_Y1/price_data$PRICE_M012
    out$cEP_1T <- 1/out$EP_1T
    out$PE_1T <- mlt$PE_1T
}
yret<-function(InstallNum, nmonths,varname="RET"){
    # calculate the returns over the next nmonths AFTER InstallNum (not beginning with InstallNum)
    # in other words, calc the nmonth return after InstallNum
    iBefore<-seq(1,length(sipbInstallDates))[sipbInstallDates=="20110630"]
    iAfter<-iBefore+1
    if ((InstallNum+nmonths)>length(sipbInstallDates)){
        return(NA)  # this would be an error, trying to get return beyond what we have data for
    }
    if (InstallNum<=iBefore & (InstallNum+nmonths)>iBefore){
        # we need to translate company_id because they change from install iBefore to iAfter
        # let's do all returns before the company_id changes. 2 cases
        if (InstallNum<iBefore){  # the install number is less than iBefore so we can use yret_sub
            out.bef<-yret_sub(InstallNum+1,iBefore,"RET")
        } else { # InstallNum is iBefore and we need old company IDs. There is no return.
            out.bef<-subset(returns.monthly,INSTALLDT==sipbInstallDates[iBefore])[,c("COMPANY_ID","RET")]
            out.bef$RET<-0
        }
        # let's do all return after the id changes
        out.aft<-yret_sub(iAfter,InstallNum+nmonths,"RET")
        # now we merge
        temp.bef<-merge(out.bef,id_translate,by.x="COMPANY_ID",by.y = "COMPANY_ID.x")
        temp.aft<-merge(out.aft,id_translate,by.x="COMPANY_ID",by.y = "COMPANY_ID.y")
        temp<-merge(temp.bef,temp.aft,by.x<-"COMPANY_ID",by.y="COMPANY_ID.x",all.x=T)
        out<-data.frame(temp$COMPANY_ID,RET=100*(1+temp$RET.x/100)*(1+temp$RET.y/100)-100)
    } else {
        out<-yret_sub(InstallNum+1,InstallNum+nmonths,"RET")
    }
    colnames(out)<-c("COMPANY_ID",varname)
    out<-out[!duplicated(out$COMPANY_ID),]
    return(out)
}

yret_sub<-function(InstallStartNum,InstallEndNum,varname="RET"){
    startdt<-sipbInstallDates[InstallStartNum]
    enddt<-sipbInstallDates[InstallEndNum]
    out<-subset(returns.monthly,INSTALLDT>=startdt & INSTALLDT<=enddt)
    out<-dcast(out,COMPANY_ID~INSTALLDT,value.var = "RET",fun.aggregate = mean) #reshape to 1 row per comp, 1 col for each return
    out<-out[complete.cases(out),] # remove NA since can't calculate a return
    if (ncol(out)>2){
        out[,2:ncol(out)]<-out[,2:ncol(out)]/100+1  #convert returns from percent to 1+decimal (8 -> 1.08)
        out<-data.frame(out[,"COMPANY_ID"],100*(apply(out[,2:ncol(out)],1,prod)-1)) #get product of rows    
    }
    colnames(out)<-c("COMPANY_ID",varname)
    return(out)
}

create_103to104_lookuptable<-function(){
    # Company IDs change from the 20110630 [#103] install to 20110729 [#104].  This impacts
    # connecting companies with returns.  To handle this, we create a translation/lookup table
    # between those to months.  First we create a data frame for the lookup. This table should be used whenever
    # one needs a return from install #104+ for a install on or before #103.  So if #100 needs the next 12 months of 
    # return, it will get returns from #101 to #103 without the table and #104-#112 with the table
    # this produces id_translate_table so one can lookup old id and find new one
    iBefore<-seq(1,length(sipbInstallDates))[sipbInstallDates=="20110630"]
    iAfter<-iBefore+1
    fldlist<-c("COMPANY_ID","COMPANY","TICKER")
    X<-load_fields_from_file("si_ci",rdata.folder,sipbInstallDates[iBefore],fldlist)
    Y<-load_fields_from_file("si_ci",rdata.folder,sipbInstallDates[iAfter],fldlist)
    # if company name + ticker match, we are good 97.3% 
    X$NAMETICK<-paste(X$COMPANY,X$TICKER)
    Y$NAMETICK<-paste(Y$COMPANY,Y$TICKER)
    out<-merge(X,Y,by="NAMETICK")
    out$NAMETICK<-NULL #don't need this anymore
    X$NAMETICK<-NULL
    Y$NAMETICK<-NULL
    nrow(out)/nrow(X) #number that match
    # now we find those in X that aren't in out, the other 2.7% (we don't worry about Y not in X because
    # those are probably new companies in database and we won't need prices for those)
    NOMATCH<-X[!X$COMPANY_ID %in% out$COMPANY_ID.x,] 
    # can we find these tickers in B
    #let's look at where Ticker matches and Company doesnt
    TMATCH<-merge(NOMATCH,Y,by="TICKER")
    TMATCH$CLOSENAME<-substr(TMATCH$COMPANY.x,1,5)==substr(TMATCH$COMPANY.y,1,5) # name is close if first 5 chars match
    TMATCH[TMATCH$CLOSENAME,c("COMPANY.x","COMPANY.y","TICKER","CLOSENAME")] 
    #by inspection these are the same company. Combine TMATCH with C
    #Align TMATCH fields with C and then rbind
    TMATCH<-TMATCH[,c("COMPANY_ID.x","COMPANY.x","TICKER","COMPANY_ID.y","COMPANY.y","TICKER")]
    names(TMATCH)<-names(out)
    nrow(out)
    nrow(TMATCH)
    out<-rbind(out,TMATCH)
    nrow(out)
    #now let's show there are no outstanding
    NOMATCH<-X[!X$COMPANY_ID %in% out$COMPANY_ID.x,]  
    TMATCH<-merge(NOMATCH,Y,by="TICKER")
    nrow(TMATCH)
    # let's try to find matching names
    NMATCH<-merge(NOMATCH,Y,by="COMPANY")
    NMATCH<-NMATCH[,c("COMPANY_ID.x","COMPANY","TICKER.x","COMPANY_ID.y","COMPANY","TICKER.y")]
    names(NMATCH)<-names(out)
    nrow(out)
    nrow(NMATCH)
    out<-rbind(out,NMATCH)
    nrow(out)
    NOMATCH<-X[!X$COMPANY_ID %in% out$COMPANY_ID.x,] 
    nrow(NOMATCH)
    # by inspection some of 99 companies for which no match was found were acquired (California Pizza Kitchen)
    id_translate<-out
    save(id_translate,file=paste(rdata.folder,"id_translate.rdata",sep=""))
    return(id_translate)
}

create_xdata_file<-function(InstallNum=1,NAvalue=NA){
    # Create a file of X data from the .rdata files corresponding to SIP DBF files.
    # Use an NAvalue other than NA to replace NAs in data.  This is especially useful here if you want to replace
    # NAs with medians or means of the varialbe by install date.
    if (InstallNum>length(sipbInstallDates)) return(NA)
    strDate<-sipbInstallDates[InstallNum]
    
    xdata <- get_ci_data(strDate)
    xdata <- merge(xdata,get_psd_data(strDate,NAvalue),by="COMPANY_ID")
    
    xdata <- xdata[xdata$PRICE>=5 & xdata$MKTCAP>=250,]  # filter for prices at least $5 and market cap $250 million
    
    xdata <- merge(xdata,get_perc_data(strDate,NAvalue),by="COMPANY_ID")
    xdata <- merge(xdata,get_mlt_data(strDate,NAvalue), by="COMPANY_ID")
    xdata <- merge(xdata,get_gr_data(strDate,NAvalue),by="COMPANY_ID")
    xdata <- merge(xdata,get_rat_data(strDate,NAvalue),by="COMPANY_ID")
    xdata <- merge(xdata,get_ee_data(strDate,NAvalue),by="COMPANY_ID")
    xdata <- merge(xdata,get_gross_profit(strDate,NAvalue),by="COMPANY_ID")
    xdata$INSTALLDT <- strDate
    
    xdata2<-xdata[,c("COMPANY_ID","INSTALLDT")]
    idx.dup<-duplicated(xdata2)
    xdata<-xdata[!idx.dup,]
    row.names(xdata)<-paste(xdata[,"COMPANY_ID"],xdata[,"INSTALLDT"],sep="")
    save(xdata,file=paste0(rdata.folder,"xdata",strDate,".rdata"))
    return(xdata)
}

create_ydata_file<-function(InstallNum=1){
    # create a file with 1,3 6, and 12 month future returns to serve as dependent variable
    # this will be saved and later combined with x data
    if (InstallNum>length(sipbInstallDates)) return(NA)
    strDate<-sipbInstallDates[InstallNum]
    if (!exists("returns.monthly")) load(paste(rdata.folder,"returnsmonthly.rdata",sep=""))
    si_ci_flds <- "COMPANY_ID, TICKER"
    ydata <- load_fields_from_file("si_ci",rdata.folder,strDate,si_ci_flds)
    #Get the next 1 month return
    if ((InstallNum+1)>length(sipbInstallDates)){
        ydata$Y_1M<-NA
    } else {
        ret<-yret(InstallNum,1,"Y_1M")
        ydata<-merge(ydata,ret,by="COMPANY_ID",all.x=T)    
    }
    #Get the next 3 month return
    if ((InstallNum+3)>length(sipbInstallDates)){
        ydata$Y_3M<-NA
    } else {
        ret<-yret(InstallNum,3,"Y_3M")
        ydata<-merge(ydata,ret,by="COMPANY_ID",all.x = T)        
    }
    # all.x=T keep companies that pass but don't have returns (avoid suvivorship bias)
    
    if ((InstallNum+6)>length(sipbInstallDates)){
        ydata$Y_6M<-NA
    } else {
        ret<-yret(InstallNum,6,"Y_6M")
        ydata<-merge(ydata,ret,by="COMPANY_ID",all.x = T)    
    }
    if ((InstallNum+12)>length(sipbInstallDates)){
        ydata$Y_12M<-NA
    } else {
        ret<-yret(InstallNum,12,"Y_12M")
        # adjust price return by dividends
        isa<-load_fields_from_file("si_isa",rdata.folder,sipbInstallDates[InstallNum+12],c("COMPANY_ID","DPS_12M"))
        psd<-load_fields_from_file("si_psd",rdata.folder,sipbInstallDates[InstallNum+12],c("COMPANY_ID","PRICE"))
        temp<-merge(isa,psd,by="COMPANY_ID")
        iBefore<-seq(1,length(sipbInstallDates))[sipbInstallDates=="20110630"]
        iAfter<-iBefore+1
        
        
        if (InstallNum<=iBefore & (InstallNum+12)>iBefore){
            # We need to translate temp$COMPANY_ID to before
            temp.aft<-merge(temp,id_translate,by.x="COMPANY_ID",by.y = "COMPANY_ID.y")
            temp<-temp.aft[,c("COMPANY_ID.x","DPS_12M","PRICE")]
            names(temp)<-c("COMPANY_ID","DPS_12M","PRICE")
        }
        
        
        temp<-merge(temp,ret,by="COMPANY_ID",all.x=FALSE,all.y=TRUE)
        temp<-temp[!is.na(temp$Y_12M),]
        temp$DPS_12M[is.na(temp$DPS_12M)]<-0
        temp$Y_12M <- temp$Y_12M +100*(temp$DPS_12M / (temp$PRICE/(1+temp$Y_12M/100)))
        
        ret<-temp[,c("COMPANY_ID","Y_12M")]
        ydata<-merge(ydata,ret,by="COMPANY_ID",all.x = T)    
    }
    if ((InstallNum+24)>length(sipbInstallDates)){
        ydata$Y_24M<-NA
    } else {
        ret<-yret(InstallNum,24,"Y_24M")
        ydata<-merge(ydata,ret,by="COMPANY_ID",all.x = T)    
    }
    if ((InstallNum+36)>length(sipbInstallDates)){
        ydata$Y_36M<-NA
    } else {
        ret<-yret(InstallNum,36,"Y_36M")
        ydata<-merge(ydata,ret,by="COMPANY_ID",all.x = T)    
    }
    ydata$INSTALLDT <- strDate
    ydata2<-ydata[,c("COMPANY_ID","INSTALLDT")]
    idx.dup<-duplicated(ydata2)
    ydata<-ydata[!idx.dup,]
    row.names(ydata)<-paste(ydata[,"COMPANY_ID"],ydata[,"INSTALLDT"],sep="")
    save(ydata,file=paste0(rdata.folder,"ydata",strDate,".rdata"))
    return(ydata)
}

create_all_x_and_y_files<-function(NAvalue=NA,istart=NA,iend=NA){
    load(paste(rdata.folder,"returnsmonthly.rdata",sep=""))
    if (is.na(istart)) istart<-1
    if (is.na(iend)) iend<-length(sipbInstallDates)
    for (i in istart:iend){
        xdata<-create_xdata_file(i,NAvalue)
        ydata<-create_ydata_file(i)
    }
}


create_all_y_files<-function(NAvalue=NA,istart=NA,iend=NA){
    load(paste(rdata.folder,"returnsmonthly.rdata",sep=""))
    if (is.na(istart)) istart<-1
    if (is.na(iend)) iend<-length(sipbInstallDates)
    for (i in istart:iend){
        ydata<-create_ydata_file(i)
    }
}
