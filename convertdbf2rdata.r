# Convert data from AAII's dbf files into r's rdata files.
# When new AAII data file is loaded, it should be installed into a subfolder named SIPYYYYMMDD under the mainfolder as defined below
# Append an entry to sipbInstallDates which is in the working directory and save the .rdata file.
# Run convertmostrecentdbf2rdata function

library(foreign)
setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
load("sipbInstallDates.rdata")
rdata.folder <- "D:/SIPro/rdata/"
mainfolder <- "D:/SIPro"

convertmostrecentdbf2rdata<-function(){
    strDate<-sipbInstallDates[length(sipbInstallDates)]
    convert_1install_dbf_to_rdata(strDate)
    convert_price_files_to_rdata(strDate)
}

dbflocations <- function(strDate){
    out <- list()
    out$dbfs <- paste(mainfolder,"/SIP",strDate,"/Dbfs",sep="")
    out$datadict <- paste(mainfolder,"/SIP",strDate,"/Datadict",sep="")
    out$static <- paste(mainfolder,"/SIP",strDate,"/Static",sep="")
    return(out)
}
loadfile<-function(fn,folder,prnt=T,flds=NULL){
    # loads the fields (all fields if flds==NULL) from dbf fn in folder
    if (is.null(flds)){
        data<-read.dbf(file=paste(folder,"/",fn,sep=""),as.is = T)    
    } else {
        data<-read.dbf(file=paste(folder,"/",fn,sep=""),as.is = T)[,flds]
    }
    if(prnt){
        print(str(data))
    }
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
    si_psdc <- loadfile("si_psdc.dbf", sipfolder$dbfs,F)
    si_psdd <- loadfile("si_psdd.dbf", sipfolder$dbfs,F)
    #var<-paste("pr_",strDate,sep="")
    #assign(var,merge(si_psdc,si_psdd,by="COMPANY_ID"))
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
    sipfolder<-dbflocations(strDate)
    si_ci <- loadfile("si_ci.dbf", sipfolder$static,F)
    si_mlt <- cleanfile(loadfile("si_mlt.dbf", sipfolder$dbfs,F))
    si_isq <- cleanfile(loadfile("si_isq.dbf", sipfolder$static,F))
    si_gr <- cleanfile(loadfile("si_gr.dbf", sipfolder$dbfs,F))
    si_perc <- cleanfile(loadfile("si_perc.dbf", sipfolder$dbfs,F))
    si_rat <- cleanfile(loadfile("si_rat.dbf", sipfolder$dbfs,F))
    si_psd <- cleanfile(loadfile("si_psd.dbf", sipfolder$dbfs,F))
    si_isa <- cleanfile(loadfile("si_isa.dbf", sipfolder$static,F))
    save(si_ci,file=paste(mainfolder,"/rdata/si_ci_",strDate,".rdata",sep=""))
    save(si_mlt,file=paste(mainfolder,"/rdata/si_mlt_",strDate,".rdata",sep=""))
    save(si_isq,file=paste(mainfolder,"/rdata/si_isq_",strDate,".rdata",sep=""))
    save(si_gr,file=paste(mainfolder,"/rdata/si_gr_",strDate,".rdata",sep=""))
    save(si_perc,file=paste(mainfolder,"/rdata/si_perc_",strDate,".rdata",sep=""))
    save(si_rat,file=paste(mainfolder,"/rdata/si_rat_",strDate,".rdata",sep=""))
    save(si_psd,file=paste(mainfolder,"/rdata/si_psd_",strDate,".rdata",sep=""))
    save(si_isa,file=paste(mainfolder,"/rdata/si_isa_",strDate,".rdata",sep=""))
    si_ee <- cleanfile(loadfile("si_ee.dbf", sipfolder$dbfs,F))
    si_bsa <- cleanfile(loadfile("si_bsa.dbf", sipfolder$static,F))
    si_bsq <- cleanfile(loadfile("si_bsq.dbf", sipfolder$static,F))
    si_val <- cleanfile(loadfile("si_val.dbf", sipfolder$dbfs,F))
    si_cfa <- cleanfile(loadfile("si_cfa.dbf", sipfolder$static,F))
    si_cfq <- cleanfile(loadfile("si_cfq.dbf", sipfolder$static,F))
    save(si_ee,file=paste(mainfolder,"/rdata/si_ee_",strDate,".rdata",sep=""))
    save(si_bsa,file=paste(mainfolder,"/rdata/si_bsa_",strDate,".rdata",sep=""))
    save(si_bsq,file=paste(mainfolder,"/rdata/si_bsq_",strDate,".rdata",sep=""))
    save(si_val,file=paste(mainfolder,"/rdata/si_val_",strDate,".rdata",sep=""))
    save(si_cfa,file=paste(mainfolder,"/rdata/si_cfa_",strDate,".rdata",sep=""))
    save(si_cfq,file=paste(mainfolder,"/rdata/si_cfq_",strDate,".rdata",sep=""))
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
