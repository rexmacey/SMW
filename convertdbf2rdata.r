# Convert data from AAII's dbf files into r's rdata files.
# When new AAII data file is loaded, it should be installed into a subfolder named SIPYYYYMMDD under the mainfolder as defined below
# Append an entry to sipbInstallDates which is in the working directory and save the .rdata file.
# Run convertmostrecentdbf2rdata function

## FD: Comments with a double pounds and "FD" (Florent Delmotte) are for code review only, feel free to delete after review.

library(foreign)
setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
source("SMWutilities.r")
init_environment()
mainfolder <- "D:/SIPro"

convertmostrecentdbf2rdata<-function(){
    strDate <- tail(sipbInstallDates, 1)    ## FD: use tail() last element.
    convert_1install_dbf_to_rdata(strDate)
    convert_price_files_to_rdata(strDate)
}



## FD: Recommend you never use T or F as shortcuts for TRUE and FALSE since T and F can be overwritten
##     e.g. your code could do a lot of weird and hard to debug things if your code had T <- 0 or F <- 1 somewhere
##     Instead, TRUE and FALSE are built-in constants so they can't be overwritten.

loadfile<-function(fn, folder, prnt=FALSE, flds=NULL){
    # loads the fields (all fields if flds==NULL) from dbf fn in folder
    data <- read.dbf(file = paste0(file.path(folder, fn),".dbf"), as.is = TRUE)
    if (!is.null(flds)) data <- data[, flds, drop = FALSE]
    if (prnt) print(str(data))
    return(data)
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

convert_1install_dbf_to_rdata<-function(strDate){  #all files except price files
    sipfolder <- dbflocations(strDate)

    ## FD: This will be a lot easier to maintain. It could stay here or go into
    ##     a csv config file.
    ##     I included a NEED_CLEAN column because you apparenlty did not
    ##     use cleanfile on ss_ci. I don't know if this was a mistake or intentional.
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
       cfq      static    TRUE", header = TRUE, stringsAsFactors = FALSE)

    files <- sprintf("si_%s", to_process$NAME)
    dirs  <- sipfolder[to_process$DIR]
    rdata <- sprintf("%s/rdata/si_%s_%s.rdata", mainfolder, to_process$NAME, strDate)
    need_clean <- to_process$NEED_CLEAN

    ## FD: a for loop will be more memory efficient
    for (i in seq_along(files)) {
      data <- loadfile(files[i], dirs[i])
      if (need_clean[i]) data <- cleanfile(data)
      save(data, file = rdata[i])
    }
}

convert_all_dbf_files_to_r<-function(){  #convert all files
    library(foreign)
    setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
    source("SMWutilities.r")
    init_environment()
    mainfolder <- "D:/SIPro"
    # lapply(sipbInstallDates,convert_1install_dbf_to_rdata)  #uses too much memory
    for (i in sipbInstallDates){
        convert_1install_dbf_to_rdata(i)
    }
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
    if (need_clean[1]) data <- cleanfile(data)
    for (i in 2:length(files)) {
      data <- read.dbf(file = paste0(file.path(dirs[i], files[i]),".dbf"), as.is = TRUE)
      if (need_clean[i]) data <- cleanfile(data)
      out<-merge(out,data,by="COMPANY_ID")
    }
    idx<-apply(is.na(out),2,sum)==nrow(out)
    out<-out[!idx]
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
    return(NULL)
}

open_all_dbfs<-function(i=1){
    # utility function to open all dbfs, so one can inspect various data
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


