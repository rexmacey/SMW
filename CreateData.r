# Create data files (in-sample, out of sample)
library(reshape2)
setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
load("sipbInstallDates.rdata")
rdata.folder <- "D:/SIPro/rdata/"
load(paste(rdata.folder,"returnsmonthly.rdata",sep=""))
load(paste(rdata.folder,"id_translate.rdata",sep=""))

load_fields_from_file<-function(fn,rdata.folder,strDate,fldlist=NULL){
    load(paste(rdata.folder,fn,"_",strDate,".rdata",sep=""))
    out<-get(ls(pattern="si_")) # returns the object 
    if (is.null(fldlist)){ #load all fields
        return(out)
    } else {
        fldlist<-unlist(strsplit(fldlist,", "))
        return(out[,fldlist])
    }
}

get_data_asof<-function(InstallNum=1){
    # create strings of field names to be extracted from various files. Then load the files. Then create new fields
    strDate<-sipbInstallDates[InstallNum]
    si_ci_flds <- "COMPANY_ID, TICKER, COMPANY, ADR, SIC, IND_2_DIG, IND_3_DIG, OPTIONABLE, SP, EXCHANGE"  # field names must be delimited by a comma AND a space
    si_mlt_flds <- NULL
    si_isq_flds <- "COMPANY_ID, EPSCON_Q1, EPSCON_Q2, EPSCON_Q5, EPSCON_Q6"
    si_gr_flds <- "COMPANY_ID, EPSC_G5F, EPSC_G1Q5, EPSC_G2Q6, EPSC_G1T, EPSC_G1F, EPSC_G2Y3, EPSC_G3Y4, DIV_Y7Y1, EPS_Y7Y1"
    si_perc_flds <- NULL # "COMPANY_ID, RRSW_4Q, RRS_04W, RRS_13W, RRS_26W, RRS_52W, RAVD_10D"
    si_rat_flds <- "COMPANY_ID, PTM_12M, CURR_Q1, LTD_EQ_Q1, PAYOUT_12M, FSCORE_Y1"
    si_psd_flds <- paste("COMPANY_ID, PRP_2YH, SHR_AQ1, MKTCAP, PRCHG_SD3Y, PRICE, PRICE_DATE, INSTPS, INSTSS, ",
                         "FLOAT, SHRINST, SHRINSTN, SHRINSD, PRCHG_04W, RS_04W, RS_13W, RS_26W, ",
                         "AVD_10D, AVM_03M, BETA, PRCHG_04W, PRCHG_13W, PRCHG_26W, PRCHG_52W, ",
                         "PR_PRH_52W", sep="")
    si_isa_flds <- paste("COMPANY_ID, EPSCON_12M, EPSCON_Y1, EPSCON_Y2, EPSCON_Y3, EPSCON_Y4, EPSCON_Y5, ",
                         "DPS_12M, DPS_Y1, DPS_Y2, DPS_Y3, DPS_Y4, DPS_Y5, DPS_Y6, DPS_Y7, ",
                         "EPS_12M, EPS_Y1, EPSDC_Y1, PTI_12M, INT_12M",sep="")
    si_ee_flds <- paste("COMPANY_ID, QS_PERC, EPSN_EQ0, EPSN_EY0, EPS_EY0, EPSPM_EY0, EPSUM_EY0, EPSDM_EY0, ",
                        "EPS_EY1, EPSPM_EY1, EPSUM_EY1, EPSDM_EY1",sep="")
    si_bsq_flds <- "COMPANY_ID, LTDEBT_Q1, PREF_Q1, STDEBT_Q1, CASH_Q1, AR_Q1, INV_Q1, AP_Q1"
    out <- load_fields_from_file("si_ci",rdata.folder,strDate,si_ci_flds)
    out <- merge(out,load_fields_from_file("si_mlt",rdata.folder,strDate,si_mlt_flds), by="COMPANY_ID")
    out <- merge(out,load_fields_from_file("si_isq",rdata.folder,strDate,si_isq_flds), by="COMPANY_ID")
    out <- merge(out,load_fields_from_file("si_gr",rdata.folder,strDate,si_gr_flds), by="COMPANY_ID")
    out <- merge(out,load_fields_from_file("si_perc",rdata.folder,strDate,si_perc_flds), by="COMPANY_ID")
    out <- merge(out,load_fields_from_file("si_rat",rdata.folder,strDate,si_rat_flds), by="COMPANY_ID")
    out <- merge(out,load_fields_from_file("si_psd",rdata.folder,strDate,si_psd_flds), by="COMPANY_ID")
    out <- merge(out,load_fields_from_file("si_isa",rdata.folder,strDate,si_isa_flds), by="COMPANY_ID")
    out <- merge(out,load_fields_from_file("si_ee",rdata.folder,strDate,si_ee_flds), by="COMPANY_ID")
    out <- merge(out,load_fields_from_file("si_bsq",rdata.folder,strDate,si_bsq_flds), by="COMPANY_ID")
    # convert appropriate variables to factors
    for (x in c("SIC","IND_2_DIG","IND_3_DIG","SP","EXCHANGE")){
        out[,x] <- factor(out[,x])
    }
    # create new variables based on existing ones. These are mainly ratios to allow for comparisons
    # EPS Est Y0 / EPS Dil Cont Y1
    out$RAT_EPSEY0_DCY1 <- out$EPS_EY0 / out$EPSDC_Y1
    # EPS Est Y0 / EPS Est Y0 last month
    out$RAT_EPSEY0_LM <- out$EPS_EY0/out$EPSPM_EY0
    # EPS Est Y0-Revisions up / EPS Est Y0-Revisions down
    out$RAT_EPSREVUPY0_DN <- out$EPSUM_EY0/out$EPSDM_EY0
    # EPS Est Y1 / EPS Est Y1 last month
    out$RAT_EPSEY1_LM <- out$EPS_EY1/out$EPSPM_EY1
    # EPS Est Y1-Revisions up / EPS Est Y1-Revisions down
    out$RAT_EPSREVUPY1_DN <- out$EPSUM_EY1/out$EPSDM_EY1
    # Rel Str 13W / Rel Str 26W
    out$RAT_RS13W_26W <- out$RS_13W / out$RS_26W
    # EPSC-Growth12M / EPSC-Growth1Y
    out$RAT_EPSCGR12M_1Y <- out$EPSC_G1T / out$EPSC_G1F
    # EPSC-Growth1Y / EPSC-GrowthY3toY2 
    out$RAT_EPSCGR1Y_Y3Y2 <- out$EPSC_G1F / out$EPSC_G2Y3
    # EPSC-GrowthY3toY2 / EPSC-GrowthY4toY3  
    out$RAT_EPSCY3toY2_Y4Y3 <- out$EPSC_G2Y3 / out$EPSC_G3Y4
    # EPSC-GrowthQ5toQ1 / EPSC-GrowthQ6toQ2
    out$RAT_EPSCQ5toQ1_Q6Q2 <- out$EPSC_G1Q5 / out$EPSC_G2Q6
    # EPSC12M / EPSCY1; and Y1/Y2, Y2/Y3, Y3/Y4, Y4/Y5
    out$RAT_EPSC12M_Y1 <- out$EPSCON_12M / out$EPSCON_Y1
    out$RAT_EPSCY1_Y2 <- out$EPSCON_Y1 / out$EPSCON_Y2
    out$RAT_EPSCY2_Y3 <- out$EPSCON_Y2 / out$EPSCON_Y3
    out$RAT_EPSCY3_Y4 <- out$EPSCON_Y3 / out$EPSCON_Y4
    out$RAT_EPSCY4_Y5 <- out$EPSCON_Y4 / out$EPSCON_Y5
    # DIV12M/DIVY1; and DIVY1/Y2, Y2/Y3, Y3/Y4, Y5/Y6, Y6/Y7
    out$RAT_DIV12M_Y1 <- out$DPS_12M / out$DPS_Y1
    out$RAT_DIVY1_Y2 <- out$DPS_Y1 / out$DPS_Y2
    out$RAT_DIVY2_Y3 <- out$DPS_Y2 / out$DPS_Y3
    out$RAT_DIVY3_Y4 <- out$DPS_Y3 / out$DPS_Y4
    out$RAT_DIVY4_Y5 <- out$DPS_Y4 / out$DPS_Y5
    out$RAT_DIVY5_Y6 <- out$DPS_Y5 / out$DPS_Y6
    out$RAT_DIVY6_Y7 <- out$DPS_Y6 / out$DPS_Y7
    # Instl Purch / Sales
    out$INSTP_S <- out$INSTPS / out$INSTSS
    # Ratio of 10D vol to 3M vol
    out$RATADV10D_3M <- out$AVD_10D / out$AVM_03M
    # From Greenblatt
    # Enterprise Value = MktCapQ1+LTDQ1+PfdStkQ1+STDQ1-CashQ1
    ENTVAL_Q1 <- out$MKTCAP + out$LTDEBT_Q1 + out$PREF_Q1 + out$STDEBT_Q1 - out$CASH_Q1
    ENTVAL_Q1[ENTVAL_Q1<=0]<-NA
    EBIT_12M <- out$PTI_12M + out$INT_12M
    EBIT_12M[EBIT_12M<=0]<-NA
    out$EBITEV <- 100* EBIT_12M / ENTVAL_Q1
    TANGIBLECAPITAL <- out$AR_Q1 + out$INV_Q1 + out$CASH_Q1 + out$AP_Q1
    TANGIBLECAPITAL[TANGIBLECAPITAL<=0]<-NA
    out$ROC<-100 * EBIT_12M / TANGIBLECAPITAL
    # Remove unneeded variables
    unneeded<-paste("EPSCON_Q1, EPSCON_Q2, EPSCON_Q5, EPSCON_Q6, EPSCON_12M, ",
                    "EPSCON_Y1, EPSCON_Y2, EPSCON_Y3, EPSCON_Y4, EPSCON_Y5, ",
                    "DPS_12M, DPS_Y1, DPS_Y2, DPS_Y3, DPS_Y4, DPS_Y5, DPS_Y6, DPS_Y7, ",
                    "EPS_12M, EPS_Y1, EPSDC_Y1, PTI_12M, INT_12M, AVD_10D, AVM_03M",
                    "LTDEBT_Q1, PREF_Q1, STDEBT_Q1, CASH_Q1, AR_Q1, INV_Q1, AP_Q1",sep="")
    unneeded<-unlist(strsplit(unneeded,", "))
    for (x in unneeded){
        out[,x]<-NULL    
    }
    #Get the next 1 month return
    ret<-yret(InstallNum,1,"Y_1M")
    out<-merge(out,ret,by="COMPANY_ID",all.x=T)
    #Get the next 3 month return
    ret<-yret(InstallNum,3,"Y_3M")
    # all.x=T keep companies that pass but don't have returns (avoid suvivorship bias)
    out<-merge(out,ret,by="COMPANY_ID",all.x = T)  
    ret<-yret(InstallNum,6,"Y_6M")
    out<-merge(out,ret,by="COMPANY_ID",all.x = T)
    ret<-yret(InstallNum,12,"Y_12M")
    out<-merge(out,ret,by="COMPANY_ID",all.x = T)
    out[,"PRICE_DATE"]<-as.Date(out[,"PRICE_DATE"],format="%Y%m%d",origin=as.Date("1970-01-01"))
    out$INSTALLDT <- strDate
    return(out)
}

yret<-function(InstallNum, nmonths,varname="RET"){
    # calculate the returns over the next nmonths AFTER InstallNum (not beginning with InstallNum)
    # in other words, calc the nmonth return after InstallNum
    if ((InstallNum+nmonths)>length(sipbInstallDates)){
        return(NA)  # this would be an error, trying to get return beyond what we have data for
    }
    if (InstallNum<=102 & (InstallNum+nmonths)>102){
        # we need to translate company_id because they change from install 102 to 103
        # let's do all returns before the company_id changes. 2 cases
        if (InstallNum<102){  # the install number is less than 102 so we can use yret_sub
            out.bef<-yret_sub(InstallNum+1,102,"RET")
        } else { # InstallNum is 102 and we need old company IDs. There is no return.
            out.bef<-subset(returns.monthly,INSTALLDT==sipbInstallDates[102])[,c("COMPANY_ID","RET")]
            out.bef$RET<-0
        }
        # let's do all return after the id changes
        out.aft<-yret_sub(103,InstallNum+nmonths,"RET")
        # now we merge
        temp.bef<-merge(out.bef,id_translate,by.x="COMPANY_ID",by.y = "COMPANY_ID.x")
        temp.aft<-merge(out.aft,id_translate,by.x="COMPANY_ID",by.y = "COMPANY_ID.y")
        temp<-merge(temp.bef,temp.aft,by.x<-"COMPANY_ID",by.y="COMPANY_ID.x",all.x=T)
        out<-data.frame(temp$COMPANY_ID,RET=100*(1+temp$RET.x/100)*(1+temp$RET.y/100)-100)
    } else {
        out<-yret_sub(InstallNum+1,InstallNum+nmonths,"RET")
    }
    colnames(out)<-c("COMPANY_ID",varname)
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
# Company IDs change from the 20110630 [#102] install to 20110729 [#103].  This impacts
# connecting companies with returns.  To handle this, we create a translation/lookup table
# between those to months.  First we create a data frame for the lookup. This table should be used whenever
# one needs a return from install #103+ for a install on or before #102.  So if #100 needs the next 12 months of 
# return, it will get returns from #101 and #102 without the table and #103-#112 with the table
create_102to103_lookuptable<-function(){
    # this produces id_translate_table so one can lookup old id and find new one
    fldlist<-c("COMPANY_ID","COMPANY","TICKER")
    X<-load_fields_from_file("si_ci",rdata.folder,sipbInstallDates[102],fldlist)
    Y<-load_fields_from_file("si_ci",rdata.folder,sipbInstallDates[103],fldlist)
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


#strDate<-sipbInstallDates[1]
#dataset<-get_data_asof(length(sipbInstallDates)-13)
#dataset<-lapply(1:(length(sipbInstallDates)-1),get_data_asof)  # can be no Y data for last install, so skip
dataset<-lapply(1:90,get_data_asof)
save(dataset,file=paste(rdata.folder,"dataset.rdata",sep=""))
library(beepr)
beep(3)
