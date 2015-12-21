# Convert data from AAII's dbf files into r's rds files.
# When new AAII data file is loaded, it should be installed into a subfolder named SIPYYYYMMDD under the mainfolder as defined below
# Append an entry to sipbInstallDates which is in the working directory and save the .rds file.
# Run convertmostrecentdbf2rdata function

library(foreign)
setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
source("SMWutilities.r")
init_environment()

convertmostrecentdbf2rdata<-function(){
    strDate <- tail(sipbInstallDates, 1)    ## FD: use tail() last element.
    convert_1install_dbf_to_rds(strDate)
    convert_price_files_to_rdata(strDate)
}

tonumeric<-function(data,varnames){
    for (v in varnames){
        data[,v] <- as.numeric(data[,v])
    }
    return(data)
}

convert_price_files_to_rdata<-function(strDate){ #convert a single pair of price and date files to rdata
    library(reshape)
    sipfolder<-dbflocations(strDate)
    si_psdc <- loadfile("si_psdc", sipfolder["dbfs"])
    si_psdd <- loadfile("si_psdd", sipfolder["dbfs"])
    price_data<-merge(si_psdc,si_psdd,by="COMPANY_ID")
    price_vars<-sprintf("PRICE_M%03d",seq(1:120))
    price_data <- tonumeric(price_data,price_vars)
    outfile<-paste(mainfolder,"/rdata/sip_prices_",strDate,".rdata",sep="")
    save(price_data,file=outfile)
}

convert_all_price_files_to_r<-function(){  #convert all files
    lapply(sipbInstallDates,convert_price_files_to_rdata)
}

cleanfile<-function(df){
    XNullCol<-match("X_NullFlags",colnames(df))
    if (! is.na(XNullCol)){
        df<-df[,1:(XNullCol-1)]
    }
    for (i in 2:ncol(df)){
        df[,i]<-as.numeric(df[,i])
    }
    return(df)
}

convert_1install_dbf_to_rds<-function(strDate){
    blndebug<-FALSE
    print(strDate)
    sipfolder <- dbflocations(strDate)
    to_process <- read.table(text =
                                 "NAME     DIR       NEED_CLEAN
                             ci       static    FALSE
                             mlt      dbfs      TRUE
                             isq      static    TRUE
                             gr       dbfs      TRUE
                             perc     dbfs      TRUE
                             rat      dbfs      TRUE
                             psd      dbfs      TRUE
                             isa      static    TRUE
                             ee       dbfs      TRUE
                             bsa      static    TRUE
                             bsq      static    TRUE
                             val      dbfs      TRUE
                             cfa      static    TRUE
                             cfq      static    TRUE
                             psdc     dbfs      TRUE
                             psdd     dbfs      TRUE", header = TRUE, stringsAsFactors = FALSE)

    files <- sprintf("si_%s", to_process$NAME)
    dirs  <- sipfolder[to_process$DIR]
    need_clean <- to_process$NEED_CLEAN
    out <- read.dbf(file = paste0(file.path(dirs[1], files[1]),".dbf"), as.is = TRUE)
    if (blndebug) {
        print("DBF#: 1 Top and Bottom COMPANY_IDs")
        print(head(out$COMPANY_ID))
        print(tail(out$COMPANY_ID))
        print(paste0("DBF#: ",1," file: ",dirs[1],"/",files[1]," data rows: ",nrow(out)))
        message("number of duplicates: ", sum(duplicated(out$COMPANY_ID)))
    }
    if (need_clean[1]) data <- cleanfile(data)
    for (i in 2:length(files)) {
      data <- read.dbf(file = paste0(file.path(dirs[i], files[i]),".dbf"), as.is = TRUE)
      if (blndebug) {
          print(paste("DBF#:",i," Top and Bottom COMPANY_IDs"))
          print(head(data$COMPANY_ID))
          print(tail(data$COMPANY_ID))
          print(paste0("DBF#: ",i," file: ",dirs[i],"/",files[i]," data rows: ",nrow(data)))
      }
      idx<-apply(is.na(data),2,sum)==nrow(data)
      data<-data[!idx]
      if (need_clean[i]) data <- cleanfile(data)
      out<-merge(out,data,by="COMPANY_ID")
      if (blndebug) print(paste0("out rows after merge: ",nrow(out)))
      if (blndebug) message("number of duplicates: ", sum(duplicated(out$COMPANY_ID)))
    }
    
    # remove dupes
    dup.idx<-duplicated(out$COMPANY_ID)
    out<-out[!dup.idx,]
    idx<-apply(is.na(out),2,sum)==nrow(out) # get rid of NA columns
    out<-out[!idx]
    idx<-grep("UPDATE",colnames(out))
    if (length(idx)>0) out<-out[-idx]
    for (x in c("IND_2_DIG","IND_3_DIG","SP","EXCHANGE","STATE","COUNTRY","DOW")){
        out[,x] <- factor(out[,x])
    }
    # needed to ensure consistency across all installs
    levels(out$SP) <- c("400","500","600","x") # needed to ensure consistency across all installs
    levels(out$EXCHANGE) <- c("A","M","N","O","x")
    levels(out$IND_2_DIG)<-c("01","02","03","04","05","06","07","08","09","10","11","12","x")
    out$ADR<-factor(out$ADR)
    out$OPTIONABLE<-factor(out$OPTIONABLE)
    idx <- substr(names(out),1,5)=="REPNO" | substr(names(out),1,7)=="LASTMOD"
    out<-out[!idx]
    idx <- out$PRICE>=5 & out$MKTCAP >=250
    out<-out[idx,]
    saveRDS(out,file=paste0(rdata.folder,"sip_",strDate,".rds"))
    return(out)
}

convert_all_dbf_files_to_rds<-function(){  #convert all files
    for (i in sipbInstallDates){
        print(i)
        data<-convert_1install_dbf_to_rds(i)
    }
}

open_all_dbfs<-function(i=1){
    # utility function to open all dbfs for one date, so one can inspect various data
    out<-list()
    strDate<-sipbInstallDates[i]
    folders_list<-c("/Dbfs/","/Static/")
    for (f in folders_list){
        file_list<-list.files(path=paste0(mainfolder,"/sip",strDate,f),pattern = "*.dbf")
        for (fn in file_list){
            if (substr(fn,nchar(fn)-3,nchar(fn))==".dbf"){
                out[[fn]] <- read.dbf(file = paste0(mainfolder,"/sip",strDate,f,fn), as.is = TRUE)    
            }
        }
    }
    return(out)
}

sample_all_dbfs<-function(i=1){
    all_dbfs<-open_all_dbfs()
    for (i in 1:length(all_dbfs)){
        print(paste(names(all_dbfs)[i],"#fields=",ncol(all_dbfs[[i]]),"#obs=",nrow(all_dbfs[[i]])))
        print(head(all_dbfs[[i]][,1:6]))
        print(" ")
    }
}


