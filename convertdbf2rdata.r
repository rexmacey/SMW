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

dbflocations <- function(strDate){
    datepath <- paste0(mainfolder, "/SIP", strDate)
    subfolders <- c("Dbfs", "Datadict", "Static")
    out <- file.path(datepath, subfolders)

    ## FD: I feel you are adding unnecessary complexity by switching the names to lowercase
    ##     I would have kept things as-is, e.g. out$Dbfs instdead of out$dbfs
    names(out) <- tolower(subfolders)

    return(out) ## FD: This is now a named vector (more appropriate), not a list
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
    lapply(sipbInstallDates,convert_1install_dbf_to_rdata)
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
