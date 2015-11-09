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
get_ci_data<-function(strDate){ # get company information from sip data files
    #c("COMPANY_ID","TICKER","COMPANY","ADR","IND_2_DIG","OPTIONABLE","SP","EXCHANGE")
    si_ci_flds <- get_si_selected_fld_list()[["ci"]]  # field names must be delimited by a comma AND a space
    out <- load_fields_from_file("si_ci",rdata.folder,strDate,si_ci_flds)
    out$SP[out$SP=="NA"]<-"x"
    for (i in c("IND_2_DIG","SP","EXCHANGE")){ 
        out[is.na(out[,i]),i]<-"x"
    }
    # convert appropriate variables to factors
    for (x in c("IND_2_DIG","SP","EXCHANGE")){
        out[,x] <- factor(out[,x])
    }
    levels(out$SP) <- c("400","500","600","x") # needed to ensure consistency across all installs
    levels(out$EXCHANGE) <- c("A","M","N","O","x")
    levels(out$IND_2_DIG)<-c("01","02","03","04","05","06","07","08","09","10","11","12","x")
    out$ADR<-factor(out$ADR)
    out$OPTIONABLE<-factor(out$OPTIONABLE)
    return(out)  # in x exclude TICKER, COMPANY
}
get_mlt_data<-function(strDate,NAvalue=NA){ # get multiples data from sip data files
    si_mlt_flds <- get_si_selected_fld_list()[["mlt"]] # these are fields in first data file so fields added later won't be a problem
    si_mlt<-load_fields_from_file("si_mlt",rdata.folder,strDate,si_mlt_flds)
    # calculate new features
    si_mlt$PEv1T <- subforna(si_mlt$PE / si_mlt$PE_1T,NAvalue)
    si_mlt$PEvA3Y <- subforna(si_mlt$PE / si_mlt$PE_A3Y,NAvalue)
    si_mlt$PEvA5Y <- subforna(si_mlt$PE / si_mlt$PE_A5Y,NAvalue)
    si_mlt$PEvA7Y <- subforna(si_mlt$PE / si_mlt$PE_A7Y,NAvalue)
    si_mlt$PEvA_Y1 <- subforna(si_mlt$PE / si_mlt$PEA_Y1,NAvalue)
    si_mlt$PEvA_Y2 <- subforna(si_mlt$PE / si_mlt$PEA_Y2,NAvalue)
    si_mlt$PEvA_Y3 <- subforna(si_mlt$PE / si_mlt$PEA_Y3,NAvalue)
    si_mlt$PEvA_Y4 <- subforna(si_mlt$PE / si_mlt$PEA_Y4,NAvalue)
    si_mlt$PEvA_Y5 <- subforna(si_mlt$PE / si_mlt$PEA_Y5,NAvalue)
    si_mlt$PEvA_Y6 <- subforna(si_mlt$PE / si_mlt$PEA_Y6,NAvalue)
    si_mlt$PEvA_Y7 <- subforna(si_mlt$PE / si_mlt$PEA_Y7,NAvalue)
    si_mlt$PEvH_A3Y <- subforna(si_mlt$PE / si_mlt$PEH_A3Y,NAvalue)
    si_mlt$PEvH_A5Y <- subforna(si_mlt$PE / si_mlt$PEH_A5Y,NAvalue)
    si_mlt$PEvH_A7Y <- subforna(si_mlt$PE / si_mlt$PEH_A7Y,NAvalue)
    si_mlt$PEvL_A3Y <- subforna(si_mlt$PE / si_mlt$PEL_A3Y,NAvalue)
    si_mlt$PEvL_A5Y <- subforna(si_mlt$PE / si_mlt$PEL_A5Y,NAvalue)
    si_mlt$PEvL_A7Y <- subforna(si_mlt$PE / si_mlt$PEL_A7Y,NAvalue)
    si_mlt$PE <-subforna(si_mlt$PE,NAvalue)
    
    si_mlt$PBVPSv1T <- subforna(si_mlt$PBVPS / si_mlt$PBVPS_1T,NAvalue)
    si_mlt$PBVPSvA3Y <- subforna(si_mlt$PBVPS / si_mlt$PBVPS_A3Y,NAvalue)
    si_mlt$PBVPSvA5Y <- subforna(si_mlt$PBVPS / si_mlt$PBVPS_A5Y,NAvalue)
    si_mlt$PBVPSvA7Y <- subforna(si_mlt$PBVPS / si_mlt$PBVPS_A7Y,NAvalue)
    si_mlt$PBVPSvA_Y1 <- subforna(si_mlt$PBVPS / si_mlt$PBVPSA_Y1,NAvalue)
    si_mlt$PBVPSvA_Y2 <- subforna(si_mlt$PBVPS / si_mlt$PBVPSA_Y2,NAvalue)
    si_mlt$PBVPSvA_Y3 <- subforna(si_mlt$PBVPS / si_mlt$PBVPSA_Y3,NAvalue)
    si_mlt$PBVPSvA_Y4 <- subforna(si_mlt$PBVPS / si_mlt$PBVPSA_Y4,NAvalue)
    si_mlt$PBVPSvA_Y5 <- subforna(si_mlt$PBVPS / si_mlt$PBVPSA_Y5,NAvalue)
    si_mlt$PBVPSvA_Y6 <- subforna(si_mlt$PBVPS / si_mlt$PBVPSA_Y6,NAvalue)
    si_mlt$PBVPSvA_Y7 <- subforna(si_mlt$PBVPS / si_mlt$PBVPSA_Y7,NAvalue)
    si_mlt$PBVPS<-subforna(si_mlt$PBVPS,NAvalue)
    
    si_mlt$PSPSv1T <- subforna(si_mlt$PSPS / si_mlt$PSPS_1T,NAvalue)
    si_mlt$PSPSvA3Y <- subforna(si_mlt$PSPS / si_mlt$PSPS_A3Y,NAvalue)
    si_mlt$PSPSvA5Y <- subforna(si_mlt$PSPS / si_mlt$PSPS_A5Y,NAvalue)
    si_mlt$PSPSvA7Y <- subforna(si_mlt$PSPS / si_mlt$PSPS_A7Y,NAvalue)
    si_mlt$PSPSvA_Y1 <- subforna(si_mlt$PSPS / si_mlt$PSPSA_Y1,NAvalue)
    si_mlt$PSPSvA_Y2 <- subforna(si_mlt$PSPS / si_mlt$PSPSA_Y2,NAvalue)
    si_mlt$PSPSvA_Y3 <- subforna(si_mlt$PSPS / si_mlt$PSPSA_Y3,NAvalue)
    si_mlt$PSPSvA_Y4 <- subforna(si_mlt$PSPS / si_mlt$PSPSA_Y4,NAvalue)
    si_mlt$PSPSvA_Y5 <- subforna(si_mlt$PSPS / si_mlt$PSPSA_Y5,NAvalue)
    si_mlt$PSPSvA_Y6 <- subforna(si_mlt$PSPS / si_mlt$PSPSA_Y6,NAvalue)
    si_mlt$PSPSvA_Y7 <- subforna(si_mlt$PSPS / si_mlt$PSPSA_Y7,NAvalue)
    si_mlt$PSPS<-subforna(si_mlt$PSPS,NAvalue)
    
    si_mlt$PCFPSv1T <- subforna(si_mlt$PCFPS / si_mlt$PCFPS_1T,NAvalue)
    si_mlt$PCFPSvA3Y <- subforna(si_mlt$PCFPS / si_mlt$PCFPS_A3Y,NAvalue)
    si_mlt$PCFPSvA5Y <- subforna(si_mlt$PCFPS / si_mlt$PCFPS_A5Y,NAvalue)
    si_mlt$PCFPSvA7Y <- subforna(si_mlt$PCFPS / si_mlt$PCFPS_A7Y,NAvalue)
    si_mlt$PCFPSvA_Y1 <- subforna(si_mlt$PCFPS / si_mlt$PCFPSA_Y1,NAvalue)
    si_mlt$PCFPSvA_Y2 <- subforna(si_mlt$PCFPS / si_mlt$PCFPSA_Y2,NAvalue)
    si_mlt$PCFPSvA_Y3 <- subforna(si_mlt$PCFPS / si_mlt$PCFPSA_Y3,NAvalue)
    si_mlt$PCFPSvA_Y4 <- subforna(si_mlt$PCFPS / si_mlt$PCFPSA_Y4,NAvalue)
    si_mlt$PCFPSvA_Y5 <- subforna(si_mlt$PCFPS / si_mlt$PCFPSA_Y5,NAvalue)
    si_mlt$PCFPSvA_Y6 <- subforna(si_mlt$PCFPS / si_mlt$PCFPSA_Y6,NAvalue)
    si_mlt$PCFPSvA_Y7 <- subforna(si_mlt$PCFPS / si_mlt$PCFPSA_Y7,NAvalue)
    si_mlt$PCFPS<-subforna(si_mlt$PCFPS,NAvalue)
    
    si_mlt$PFCPSv1T <- subforna(si_mlt$PFCPS / si_mlt$PFCPS_1T,NAvalue)
    si_mlt$PFCPSvA3Y <- subforna(si_mlt$PFCPS / si_mlt$PFCPS_A3Y,NAvalue)
    si_mlt$PFCPSvA5Y <- subforna(si_mlt$PFCPS / si_mlt$PFCPS_A5Y,NAvalue)
    si_mlt$PFCPSvA7Y <- subforna(si_mlt$PFCPS / si_mlt$PFCPS_A7Y,NAvalue)
    si_mlt$PFCPSvA_Y1 <- subforna(si_mlt$PFCPS / si_mlt$PFCPSA_Y1,NAvalue)
    si_mlt$PFCPSvA_Y2 <- subforna(si_mlt$PFCPS / si_mlt$PFCPSA_Y2,NAvalue)
    si_mlt$PFCPSvA_Y3 <- subforna(si_mlt$PFCPS / si_mlt$PFCPSA_Y3,NAvalue)
    si_mlt$PFCPSvA_Y4 <- subforna(si_mlt$PFCPS / si_mlt$PFCPSA_Y4,NAvalue)
    si_mlt$PFCPSvA_Y5 <- subforna(si_mlt$PFCPS / si_mlt$PFCPSA_Y5,NAvalue)
    si_mlt$PFCPSvA_Y6 <- subforna(si_mlt$PFCPS / si_mlt$PFCPSA_Y6,NAvalue)
    si_mlt$PFCPSvA_Y7 <- subforna(si_mlt$PFCPS / si_mlt$PFCPSA_Y7,NAvalue)
    si_mlt$PFCPS<-subforna(si_mlt$PFCPS,NAvalue)
    
    si_mlt$YIELDv1T <- subforna(si_mlt$YIELD / si_mlt$YIELD_1T,NAvalue)
    si_mlt$YIELDvA3Y <- subforna(si_mlt$YIELD / si_mlt$YIELD_A3Y,NAvalue)
    si_mlt$YIELDvA5Y <- subforna(si_mlt$YIELD / si_mlt$YIELD_A5Y,NAvalue)
    si_mlt$YIELDvA7Y <- subforna(si_mlt$YIELD / si_mlt$YIELD_A7Y,NAvalue)
    si_mlt$YIELDvA_Y1 <- subforna(si_mlt$YIELD / si_mlt$YIELDA_Y1,NAvalue)
    si_mlt$YIELDvA_Y2 <- subforna(si_mlt$YIELD / si_mlt$YIELDA_Y2,NAvalue)
    si_mlt$YIELDvA_Y3 <- subforna(si_mlt$YIELD / si_mlt$YIELDA_Y3,NAvalue)
    si_mlt$YIELDvA_Y4 <- subforna(si_mlt$YIELD / si_mlt$YIELDA_Y4,NAvalue)
    si_mlt$YIELDvA_Y5 <- subforna(si_mlt$YIELD / si_mlt$YIELDA_Y5,NAvalue)
    si_mlt$YIELDvA_Y6 <- subforna(si_mlt$YIELD / si_mlt$YIELDA_Y6,NAvalue)
    si_mlt$YIELDvA_Y7 <- subforna(si_mlt$YIELD / si_mlt$YIELDA_Y7,NAvalue)
    si_mlt$YIELD<-subforna(si_mlt$YIELD,NAvalue)
    
    si_mlt$PE_TO_G5F<-subforna(si_mlt$PE_TO_G5F,NAvalue)
    si_mlt$PE_TO_G5E<-subforna(si_mlt$PE_TO_G5E,NAvalue)
    si_mlt$PE_TO_DG5F<-subforna(si_mlt$PE_TO_DG5F,NAvalue)
    si_mlt$PE_AEPS3Y<-subforna(si_mlt$PE_AEPS3Y,NAvalue)
    si_mlt$PGFPS<-subforna(si_mlt$PGFPS,NAvalue)
    si_mlt$IW_RV<-subforna(si_mlt$IW_RV,NAvalue)
    si_mlt$IW_EYIELD<-subforna(si_mlt$IW_EYIELD,NAvalue)
    si_mlt$PE_TO_YG5E<-subforna(si_mlt$PE_TO_YG5E,NAvalue)
    si_mlt$EYIELD_12M<-subforna(si_mlt$EYIELD_12M,NAvalue)
    si_mlt$THUMB<-subforna(si_mlt$THUMB,NAvalue)
    si_mlt$PEH_Y1<-subforna(si_mlt$PEH_Y1,NAvalue)
    si_mlt$PEH_Y2<-subforna(si_mlt$PEH_Y2,NAvalue)
    si_mlt$PEH_Y3<-subforna(si_mlt$PEH_Y3,NAvalue)
    si_mlt$PEH_Y4<-subforna(si_mlt$PEH_Y4,NAvalue)
    si_mlt$PEH_Y5<-subforna(si_mlt$PEH_Y5,NAvalue)
    si_mlt$PEL_Y1<-subforna(si_mlt$PEL_Y1,NAvalue)
    si_mlt$PEL_Y2<-subforna(si_mlt$PEL_Y2,NAvalue)
    si_mlt$PEL_Y3<-subforna(si_mlt$PEL_Y3,NAvalue)
    si_mlt$PEL_Y4<-subforna(si_mlt$PEL_Y4,NAvalue)
    si_mlt$PEL_Y5<-subforna(si_mlt$PEL_Y5,NAvalue)
    si_mlt$PERH_A5Y<-subforna(si_mlt$PERH_A5Y,NAvalue)
    si_mlt$PERL_A5Y<-subforna(si_mlt$PERL_A5Y,NAvalue)
    si_mlt$PERA_5Y<-subforna(si_mlt$PERA_5Y,NAvalue)
    si_mlt$PERV<-subforna(si_mlt$PERV,NAvalue)
    si_mlt$PERVP<-subforna(si_mlt$PERVP,NAvalue)
    si_mlt$PERAPE<-subforna(si_mlt$PERAPE,NAvalue)
    si_mlt$YIELDH_A7Y<-subforna(si_mlt$YIELDH_A7Y,NAvalue)
    si_mlt$PF_TO_F_GR<-subforna(si_mlt$PF_TO_F_GR,NAvalue)
    
    si_mlt$PE_EY0<-subforna(si_mlt$PE_EY0,NAvalue)
    si_mlt$PE_EY1<-subforna(si_mlt$PE_EY1,NAvalue)
    si_mlt$PE_EY2<-subforna(si_mlt$PE_EY2,NAvalue)
    
    # remove unwanted fields that were used to calculate new features
    for (x in c("PE_1T","PE_A3Y","PE_A5Y","PE_A7Y","PEA_Y1","PEA_Y2","PEA_Y3","PEA_Y4",
                "PEA_Y5","PEA_Y6","PEA_Y7","PEH_A3Y","PEH_A5Y","PEH_A7Y",
                "PEL_A3Y","PEL_A5Y","PEL_A7Y",
                "PBVPS_1T","PBVPS_A3Y","PBVPS_A5Y","PBVPS_A7Y","PBVPSA_Y1","PBVPSA_Y2","PBVPSA_Y3","PBVPSA_Y4",
                "PBVPSA_Y5","PBVPSA_Y6","PBVPSA_Y7",
                "PSPS_1T","PSPS_A3Y","PSPS_A5Y","PSPS_A7Y","PSPSA_Y1","PSPSA_Y2","PSPSA_Y3","PSPSA_Y4",
                "PSPSA_Y5","PSPSA_Y6","PSPSA_Y7",
                "PCFPS_1T","PCFPS_A3Y","PCFPS_A5Y","PCFPS_A7Y","PCFPSA_Y1","PCFPSA_Y2","PCFPSA_Y3","PCFPSA_Y4",
                "PCFPSA_Y5","PCFPSA_Y6","PCFPSA_Y7",
                "PFCPS_1T","PFCPS_A3Y","PFCPS_A5Y","PFCPS_A7Y","PFCPSA_Y1","PFCPSA_Y2","PFCPSA_Y3","PFCPSA_Y4",
                "PFCPSA_Y5","PFCPSA_Y6","PFCPSA_Y7",
                "YIELD_1T","YIELD_A3Y","YIELD_A5Y","YIELD_A7Y","YIELDA_Y1","YIELDA_Y2","YIELDA_Y3","YIELDA_Y4",
                "YIELDA_Y5","YIELDA_Y6","YIELDA_Y7")){
        si_mlt[,x]<-NULL
    }
    return(si_mlt)
}
get_gr_data<-function(strDate,NAvalue=NA){ # get growth data from sip data files
    si_gr_flds<-get_si_selected_fld_list()[["gr"]]
    si_gr<-load_fields_from_file("si_gr",rdata.folder,strDate,si_gr_flds)
    for (i in 1:ncol(si_gr)){
        si_gr[,i]<-subforna(si_gr[,i],NAvalue)
    }
    return(si_gr)
}
get_rat_data<-function(strDate,NAvalue=NA){ # get ratio data from sip files
    si_rat_flds <-get_si_selected_fld_list()[["rat"]]
    out<-load_fields_from_file("si_rat",rdata.folder,strDate,si_rat_flds)
    
    out$ARTURN_12M_Y1 <- subforna(out$ARTURN_12M - out$ARTURN_Y1,NAvalue)
    out$ARTURN_12M_Y2 <- subforna(out$ARTURN_12M - out$ARTURN_Y2,NAvalue)
    out$ARTURN_12M_Y3 <- subforna(out$ARTURN_12M - out$ARTURN_Y3,NAvalue)
    out$ARTURN_12M_Y4 <- subforna(out$ARTURN_12M - out$ARTURN_Y4,NAvalue)
    out$ARTURN_12M_Y5 <- subforna(out$ARTURN_12M - out$ARTURN_Y5,NAvalue)
    out$ARTURN_12M_Y6 <- subforna(out$ARTURN_12M - out$ARTURN_Y6,NAvalue)
    out$ARTURN_12M_Y7 <- subforna(out$ARTURN_12M - out$ARTURN_Y7,NAvalue)
    out$ARTURN_12M <- subforna(out$ARTURN_12M,NAvalue)
    out$ARTURN_Y1 <- subforna(out$ARTURN_Y1,NAvalue)
    
    out$CURR_Q1_Y1 <- subforna(out$CURR_Q1 - out$CURR_Y1,NAvalue)
    out$CURR_Q1_Y2 <- subforna(out$CURR_Q1 - out$CURR_Y2,NAvalue)
    out$CURR_Q1_Y3 <- subforna(out$CURR_Q1 - out$CURR_Y3,NAvalue)
    out$CURR_Q1_Y4 <- subforna(out$CURR_Q1 - out$CURR_Y4,NAvalue)
    out$CURR_Q1_Y5 <- subforna(out$CURR_Q1 - out$CURR_Y5,NAvalue)
    out$CURR_Q1_Y6 <- subforna(out$CURR_Q1 - out$CURR_Y6,NAvalue)
    out$CURR_Q1_Y7 <- subforna(out$CURR_Q1 - out$CURR_Y7,NAvalue)
    out$CURR_Q1 <- subforna(out$CURR_Q1,NAvalue)
    out$CURR_Y1 <- subforna(out$CURR_Y1,NAvalue)
    
    out$ERBV <- subforna(out$ERBV,NAvalue)
    out$FSCORE_12M <- subforna(out$FSCORE_12M,NAvalue)
    out$FSCORE_Y1 <- subforna(out$FSCORE_Y1,NAvalue)
    out$F_12M_Y1 <- subforna(out$FSCORE_12M -out$FSCORE_Y1,NAvalue)
    
    out$GPM_12M_A5Y <- subforna(out$GPM_12M - out$GPM_A5Y,NAvalue)
    out$GPM_12M_Y1 <- subforna(out$GPM_12M - out$GPM_Y1,NAvalue)
    out$GPM_12M_Y2 <- subforna(out$GPM_12M - out$GPM_Y2,NAvalue)
    out$GPM_12M_Y3 <- subforna(out$GPM_12M - out$GPM_Y3,NAvalue)
    out$GPM_12M_Y4 <- subforna(out$GPM_12M - out$GPM_Y4,NAvalue)
    out$GPM_12M_Y5 <- subforna(out$GPM_12M - out$GPM_Y5,NAvalue)
    out$GPM_12M_Y6 <- subforna(out$GPM_12M - out$GPM_Y6,NAvalue)
    out$GPM_12M_Y7 <- subforna(out$GPM_12M - out$GPM_Y7,NAvalue)
    out$GPM_A5Y <- subforna(out$GPM_A5Y,NAvalue)
    out$GPM_12M <- subforna(out$GPM_12M,NAvalue)
    out$GPM_Y1 <- subforna(out$GPM_Y1,NAvalue)
    
    out$INVTRN_12M_Y1 <- subforna(out$INVTRN_12M - out$INVTRN_Y1,NAvalue)
    out$INVTRN_12M_Y2 <- subforna(out$INVTRN_12M - out$INVTRN_Y2,NAvalue)
    out$INVTRN_12M_Y3 <- subforna(out$INVTRN_12M - out$INVTRN_Y3,NAvalue)
    out$INVTRN_12M_Y4 <- subforna(out$INVTRN_12M - out$INVTRN_Y4,NAvalue)
    out$INVTRN_12M_Y5 <- subforna(out$INVTRN_12M - out$INVTRN_Y5,NAvalue)
    out$INVTRN_12M_Y6 <- subforna(out$INVTRN_12M - out$INVTRN_Y6,NAvalue)
    out$INVTRN_12M_Y7 <- subforna(out$INVTRN_12M - out$INVTRN_Y7,NAvalue)
    out$INVTRN_12M <- subforna(out$INVTRN_12M,NAvalue)
    out$INVTRN_Y1 <- subforna(out$INVTRN_Y1,NAvalue)
    
    out$LTD_EQ_Q1_Y1 <- subforna(out$LTD_EQ_Q1 - out$LTD_EQ_Y1,NAvalue)
    out$LTD_EQ_Q1_Y2 <- subforna(out$LTD_EQ_Q1 - out$LTD_EQ_Y2,NAvalue)
    out$LTD_EQ_Q1_Y3 <- subforna(out$LTD_EQ_Q1 - out$LTD_EQ_Y3,NAvalue)
    out$LTD_EQ_Q1_Y4 <- subforna(out$LTD_EQ_Q1 - out$LTD_EQ_Y4,NAvalue)
    out$LTD_EQ_Q1_Y5 <- subforna(out$LTD_EQ_Q1 - out$LTD_EQ_Y5,NAvalue)
    out$LTD_EQ_Q1_Y6 <- subforna(out$LTD_EQ_Q1 - out$LTD_EQ_Y6,NAvalue)
    out$LTD_EQ_Q1_Y7 <- subforna(out$LTD_EQ_Q1 - out$LTD_EQ_Y7,NAvalue)
    out$LTD_EQ_Q1 <- subforna(out$LTD_EQ_Q1,NAvalue)
    out$LTD_EQ_Y1 <- subforna(out$LTD_EQ_Y1,NAvalue)
    
    out$LTD_TC_Q1_Y1 <- subforna(out$LTD_TC_Q1 - out$LTD_TC_Y1,NAvalue)
    out$LTD_TC_Q1_Y2 <- subforna(out$LTD_TC_Q1 - out$LTD_TC_Y2,NAvalue)
    out$LTD_TC_Q1_Y3 <- subforna(out$LTD_TC_Q1 - out$LTD_TC_Y3,NAvalue)
    out$LTD_TC_Q1_Y4 <- subforna(out$LTD_TC_Q1 - out$LTD_TC_Y4,NAvalue)
    out$LTD_TC_Q1_Y5 <- subforna(out$LTD_TC_Q1 - out$LTD_TC_Y5,NAvalue)
    out$LTD_TC_Q1_Y6 <- subforna(out$LTD_TC_Q1 - out$LTD_TC_Y6,NAvalue)
    out$LTD_TC_Q1_Y7 <- subforna(out$LTD_TC_Q1 - out$LTD_TC_Y7,NAvalue)
    out$LTD_TC_Q1 <- subforna(out$LTD_TC_Q1,NAvalue)
    out$LTD_TC_Y1 <- subforna(out$LTD_TC_Y1,NAvalue)
    
    out$LTD_WC_Q1 <- subforna(out$LTD_WC_Q1,NAvalue)
    
    out$NPM_12M_Y1 <- subforna(out$NPM_12M - out$NPM_Y1,NAvalue)
    out$NPM_12M_Y2 <- subforna(out$NPM_12M - out$NPM_Y2,NAvalue)
    out$NPM_12M_Y3 <- subforna(out$NPM_12M - out$NPM_Y3,NAvalue)
    out$NPM_12M_Y4 <- subforna(out$NPM_12M - out$NPM_Y4,NAvalue)
    out$NPM_12M_Y5 <- subforna(out$NPM_12M - out$NPM_Y5,NAvalue)
    out$NPM_12M_Y6 <- subforna(out$NPM_12M - out$NPM_Y6,NAvalue)
    out$NPM_12M_Y7 <- subforna(out$NPM_12M - out$NPM_Y7,NAvalue)
    out$NPM_12M <- subforna(out$NPM_12M,NAvalue)
    out$NPM_Y1 <- subforna(out$NPM_Y1,NAvalue)
    
    out$OPM_12M_A3Y <- subforna(out$OPM_12M - out$OPM_A3Y,NAvalue)
    out$OPM_12M_A5Y <- subforna(out$OPM_12M - out$OPM_A5Y,NAvalue)
    out$OPM_12M_Y1 <- subforna(out$OPM_12M - out$OPM_Y1,NAvalue)
    out$OPM_12M_Y2 <- subforna(out$OPM_12M - out$OPM_Y2,NAvalue)
    out$OPM_12M_Y3 <- subforna(out$OPM_12M - out$OPM_Y3,NAvalue)
    out$OPM_12M_Y4 <- subforna(out$OPM_12M - out$OPM_Y4,NAvalue)
    out$OPM_12M_Y5 <- subforna(out$OPM_12M - out$OPM_Y5,NAvalue)
    out$OPM_12M_Y6 <- subforna(out$OPM_12M - out$OPM_Y6,NAvalue)
    out$OPM_12M_Y7 <- subforna(out$OPM_12M - out$OPM_Y7,NAvalue)
    out$OPM_A3Y <- subforna(out$OPM_A3Y,NAvalue)
    out$OPM_A5Y <- subforna(out$OPM_A5Y,NAvalue)
    out$OPM_12M <- subforna(out$OPM_12M,NAvalue)
    out$OPM_Y1 <- subforna(out$OPM_Y1,NAvalue)
    
    out$PAYOUT_12M_A7Y <- subforna(out$PAYOUT_12M - out$PAYOUT_A7Y,NAvalue)
    out$PAYOUT_12M_Y1 <- subforna(out$PAYOUT_12M - out$PAYOUT_Y1,NAvalue)
    out$PAYOUT_12M_Y2 <- subforna(out$PAYOUT_12M - out$PAYOUT_Y2,NAvalue)
    out$PAYOUT_12M_Y3 <- subforna(out$PAYOUT_12M - out$PAYOUT_Y3,NAvalue)
    out$PAYOUT_12M_Y4 <- subforna(out$PAYOUT_12M - out$PAYOUT_Y4,NAvalue)
    out$PAYOUT_12M_Y5 <- subforna(out$PAYOUT_12M - out$PAYOUT_Y5,NAvalue)
    out$PAYOUT_12M_Y6 <- subforna(out$PAYOUT_12M - out$PAYOUT_Y6,NAvalue)
    out$PAYOUT_12M_Y7 <- subforna(out$PAYOUT_12M - out$PAYOUT_Y7,NAvalue)
    out$PAYOUT_A7Y <- subforna(out$PAYOUT_A7Y,NAvalue)
    out$PAYOUT_12M <- subforna(out$PAYOUT_12M,NAvalue)
    out$PAYOUT_Y1 <- subforna(out$PAYOUT_Y1,NAvalue)
    
    out$PR_BV_CHG <- subforna(out$PR_BV_CHG,NAvalue)
    out$PTM_12M <- subforna(out$PTM_12M,NAvalue)
    
    out$QUICK_Q1_Y1 <- subforna(out$QUICK_Q1 - out$QUICK_Y1,NAvalue)
    out$QUICK_Q1_Y2 <- subforna(out$QUICK_Q1 - out$QUICK_Y2,NAvalue)
    out$QUICK_Q1_Y3 <- subforna(out$QUICK_Q1 - out$QUICK_Y3,NAvalue)
    out$QUICK_Q1_Y4 <- subforna(out$QUICK_Q1 - out$QUICK_Y4,NAvalue)
    out$QUICK_Q1_Y5 <- subforna(out$QUICK_Q1 - out$QUICK_Y5,NAvalue)
    out$QUICK_Q1_Y6 <- subforna(out$QUICK_Q1 - out$QUICK_Y6,NAvalue)
    out$QUICK_Q1_Y7 <- subforna(out$QUICK_Q1 - out$QUICK_Y7,NAvalue)
    out$QUICK_Q1 <- subforna(out$QUICK_Q1,NAvalue)
    out$QUICK_Y1 <- subforna(out$QUICK_Y1,NAvalue)
    
    out$RDM_12M <- subforna(out$RDM_12M,NAvalue)
    
    out$ROA_12M_Y1 <- subforna(out$ROA_12M - out$ROA_Y1,NAvalue)
    out$ROA_12M_Y2 <- subforna(out$ROA_12M - out$ROA_Y2,NAvalue)
    out$ROA_12M_Y3 <- subforna(out$ROA_12M - out$ROA_Y3,NAvalue)
    out$ROA_12M_Y4 <- subforna(out$ROA_12M - out$ROA_Y4,NAvalue)
    out$ROA_12M_Y5 <- subforna(out$ROA_12M - out$ROA_Y5,NAvalue)
    out$ROA_12M_Y6 <- subforna(out$ROA_12M - out$ROA_Y6,NAvalue)
    out$ROA_12M_Y7 <- subforna(out$ROA_12M - out$ROA_Y7,NAvalue)
    out$ROA_12M <- subforna(out$ROA_12M,NAvalue)
    out$ROA_Y1 <- subforna(out$ROA_Y1,NAvalue)
    
    out$ROE_12M_A5Y <- subforna(out$ROE_12M - out$ROE_A5Y,NAvalue)
    out$ROE_12M_A7Y <- subforna(out$ROE_12M - out$ROE_A7Y,NAvalue)
    out$ROE_12M_Y1 <- subforna(out$ROE_12M - out$ROE_Y1,NAvalue)
    out$ROE_12M_Y2 <- subforna(out$ROE_12M - out$ROE_Y2,NAvalue)
    out$ROE_12M_Y3 <- subforna(out$ROE_12M - out$ROE_Y3,NAvalue)
    out$ROE_12M_Y4 <- subforna(out$ROE_12M - out$ROE_Y4,NAvalue)
    out$ROE_12M_Y5 <- subforna(out$ROE_12M - out$ROE_Y5,NAvalue)
    out$ROE_12M_Y6 <- subforna(out$ROE_12M - out$ROE_Y6,NAvalue)
    out$ROE_12M_Y7 <- subforna(out$ROE_12M - out$ROE_Y7,NAvalue)
    out$ROE_12M <- subforna(out$ROE_12M,NAvalue)
    out$ROE_Y1 <- subforna(out$ROE_Y1,NAvalue)
    out$ROE_A5Y <- subforna(out$ROE_A5Y,NAvalue)
    out$ROE_A7Y <- subforna(out$ROE_A7Y,NAvalue)
    
    out$TA_TRN_12M_Y1 <- subforna(out$TA_TRN_12M - out$TA_TRN_Y1,NAvalue)
    out$TA_TRN_12M_Y2 <- subforna(out$TA_TRN_12M - out$TA_TRN_Y2,NAvalue)
    out$TA_TRN_12M_Y3 <- subforna(out$TA_TRN_12M - out$TA_TRN_Y3,NAvalue)
    out$TA_TRN_12M_Y4 <- subforna(out$TA_TRN_12M - out$TA_TRN_Y4,NAvalue)
    out$TA_TRN_12M_Y5 <- subforna(out$TA_TRN_12M - out$TA_TRN_Y5,NAvalue)
    out$TA_TRN_12M_Y6 <- subforna(out$TA_TRN_12M - out$TA_TRN_Y6,NAvalue)
    out$TA_TRN_12M_Y7 <- subforna(out$TA_TRN_12M - out$TA_TRN_Y7,NAvalue)
    out$TA_TRN_12M <- subforna(out$TA_TRN_12M,NAvalue)
    out$TA_TRN_Y1 <- subforna(out$TA_TRN_Y1,NAvalue)
    
    out$TIE_12M_Y1 <- subforna(out$TIE_12M - out$TIE_Y1,NAvalue)
    out$TIE_12M_Y2 <- subforna(out$TIE_12M - out$TIE_Y2,NAvalue)
    out$TIE_12M_Y3 <- subforna(out$TIE_12M - out$TIE_Y3,NAvalue)
    out$TIE_12M_Y4 <- subforna(out$TIE_12M - out$TIE_Y4,NAvalue)
    out$TIE_12M_Y5 <- subforna(out$TIE_12M - out$TIE_Y5,NAvalue)
    out$TIE_12M_Y6 <- subforna(out$TIE_12M - out$TIE_Y6,NAvalue)
    out$TIE_12M_Y7 <- subforna(out$TIE_12M - out$TIE_Y7,NAvalue)
    out$TIE_12M <- subforna(out$TIE_12M,NAvalue)
    out$TIE_Y1 <- subforna(out$TIE_Y1,NAvalue)
    
    out$TL_TA_Q1_Y1 <- subforna(out$TL_TA_Q1 - out$TL_TA_Y1,NAvalue)
    out$TL_TA_Q1_Y2 <- subforna(out$TL_TA_Q1 - out$TL_TA_Y2,NAvalue)
    out$TL_TA_Q1_Y3 <- subforna(out$TL_TA_Q1 - out$TL_TA_Y3,NAvalue)
    out$TL_TA_Q1_Y4 <- subforna(out$TL_TA_Q1 - out$TL_TA_Y4,NAvalue)
    out$TL_TA_Q1_Y5 <- subforna(out$TL_TA_Q1 - out$TL_TA_Y5,NAvalue)
    out$TL_TA_Q1_Y6 <- subforna(out$TL_TA_Q1 - out$TL_TA_Y6,NAvalue)
    out$TL_TA_Q1_Y7 <- subforna(out$TL_TA_Q1 - out$TL_TA_Y7,NAvalue)
    out$TL_TA_Q1 <- subforna(out$TL_TA_Q1,NAvalue)
    out$TL_TA_Y1 <- subforna(out$TL_TA_Y1,NAvalue)    
    
    for (x in c(
            "ARTURN_Y2","ARTURN_Y3","ARTURN_Y4","ARTURN_Y5","ARTURN_Y6","ARTURN_Y7",
            "CURR_Y2","CURR_Y3","CURR_Y4","CURR_Y5","CURR_Y6","CURR_Y7",
            "GPM_Y2","GPM_Y3","GPM_Y4","GPM_Y5","GPM_Y6","GPM_Y7",
            "INVTRN_Y2","INVTRN_Y3","INVTRN_Y4","INVTRN_Y5","INVTRN_Y6","INVTRN_Y7",
            "LTD_EQ_Y2","LTD_EQ_Y3","LTD_EQ_Y4","LTD_EQ_Y5","LTD_EQ_Y6","LTD_EQ_Y7",
            "LTD_TC_Y2","LTD_TC_Y3","LTD_TC_Y4","LTD_TC_Y5","LTD_TC_Y6","LTD_TC_Y7",
            "NPM_Y2","NPM_Y3","NPM_Y4","NPM_Y5","NPM_Y6","NPM_Y7",
            "OPM_Y2","OPM_Y3","OPM_Y4","OPM_Y5","OPM_Y6","OPM_Y7",
            "PAYOUT_Y2","PAYOUT_Y3","PAYOUT_Y4","PAYOUT_Y5","PAYOUT_Y6","PAYOUT_Y7",
            "QUICK_Y2","QUICK_Y3","QUICK_Y4","QUICK_Y5","QUICK_Y6","QUICK_Y7",
            "ROA_Y2","ROA_Y3","ROA_Y4","ROA_Y5","ROA_Y6","ROA_Y7",
            "ROE_Y2","ROE_Y3","ROE_Y4","ROE_Y5","ROE_Y6","ROE_Y7",
            "TA_TRN_Y2","TA_TRN_Y3","TA_TRN_Y4","TA_TRN_Y5","TA_TRN_Y6","TA_TRN_Y7",
            "TIE_Y2","TIE_Y3","TIE_Y4","TIE_Y5","TIE_Y6","TIE_Y7",
            "TL_TA_Y2","TL_TA_Y3","TL_TA_Y4","TL_TA_Y5","TL_TA_Y6","TL_TA_Y7",
            "FSCORE_Y3","FSCORE_Y5")) {
        out[,x] <- NULL
    }
    return(out)
}
get_psd_data<-function(strDate,NAvalue=NA){
    si_psd_flds<-get_si_selected_fld_list()[["psd"]]
    out<-load_fields_from_file("si_psd",rdata.folder,strDate,si_psd_flds)
    
    out$PR_PRL_52W <-  subforna(100*(out$PRICE / out$PRICEL_52W-1),NAvalue)
    out$SHR_AQ1_AQ2 <- subforna(out$SHR_AQ1 / out$SHR_AQ2,NAvalue)
    out$SHR_AQ1_AQ3 <- subforna(out$SHR_AQ1 / out$SHR_AQ3,NAvalue)
    out$SHR_AQ1_AQ4 <- subforna(out$SHR_AQ1 / out$SHR_AQ4,NAvalue)
    out$SHR_AQ1_AQ5 <- subforna(out$SHR_AQ1 / out$SHR_AQ5,NAvalue)
    out$SHR_AQ1_AQ6 <- subforna(out$SHR_AQ1 / out$SHR_AQ6,NAvalue)
    out$SHR_AQ1_AQ7 <- subforna(out$SHR_AQ1 / out$SHR_AQ7,NAvalue)
    out$SHR_AQ1_AQ8 <- subforna(out$SHR_AQ1 / out$SHR_AQ8,NAvalue)
    out$SHR_AQ1_AY1 <- subforna(out$SHR_AQ1 / out$SHR_AY1,NAvalue)
    out$SHR_AQ1_AY2 <- subforna(out$SHR_AQ1 / out$SHR_AY2,NAvalue)
    out$SHR_AQ1_AY3 <- subforna(out$SHR_AQ1 / out$SHR_AY3,NAvalue)
    out$SHR_AQ1_AY4 <- subforna(out$SHR_AQ1 / out$SHR_AY4,NAvalue)
    out$SHR_AQ1_AY5 <- subforna(out$SHR_AQ1 / out$SHR_AY5,NAvalue)
    out$SHR_AQ1_AY6 <- subforna(out$SHR_AQ1 / out$SHR_AY6,NAvalue)
    out$SHR_AQ1_AY7 <- subforna(out$SHR_AQ1 / out$SHR_AY7,NAvalue)
    out$AVD10_SHRQ1 <- subforna(out$AVD_10D / out$SHR_AQ1,NAvalue)
    out$AVM03_SHRQ1 <- subforna(out$AVM_03M / out$SHR_AQ1,NAvalue)
    out$AVD10_AVM03 <- subforna(out$AVD_10D / out$AVM_03M,NAvalue)
    out$INSTPS_SHRAQ1 <- subforna(100*(out$INSTPS / out$SHR_AQ1),NAvalue)
    out$INSTSS_SHRAQ1 <- subforna(100*(out$INSTSS / out$SHR_AQ1),NAvalue)
    out$INSTNET_SHRAQ1 <- subforna(100*(out$INSTPS-out$INSTSS) / out$SHR_AQ1,NAvalue)
    out$INSDPS_SHRAQ1 <- subforna(100*(out$INSDPS / out$SHR_AQ1),NAvalue)
    out$INSDSS_SHRAQ1 <- subforna(100*(out$INSDSS / out$SHR_AQ1),NAvalue)
    out$SHRINST <-  subforna(out$SHRINST, NA)
    out$SHRINSD <-  subforna(out$SHRINSD, NA)
    out$SHRINSTN <-  subforna(out$SHRINSTN, NA)
    out$INSDPT <- subforna(out$INSDPT,NAvalue)
    out$INSDST <- subforna(out$INSDST,NAvalue)
    out$INSDTNET <- subforna(out$INSDPT - out$INSDST,NAvalue)
    out$INS_PR_SHR <-  subforna(out$INS_PR_SHR,NAvalue)
    out$FLOAT <-  subforna(out$FLOAT,NAvalue)
    out$BETA <-  subforna(out$BETA,NAvalue)
    out$MKTCAP <-  subforna(out$MKTCAP,NAvalue)
    out$LOGMKTCAP <- subforna(log(out$MKTCAP),NAvalue)
    out$PRCHG_AM3Y <- subforna(out$PRCHG_AM3Y,NAvalue)
    out$PRCHG_GM3Y <- subforna(out$PRCHG_GM3Y,NAvalue)
    out$PRCHG_SD3Y <- subforna(out$PRCHG_SD3Y,NAvalue)
    out$PRP_2YH <- subforna(out$PRP_2YH,NAvalue)
    out$DD_A3M <- subforna(out$DD_A3M,NAvalue)
    out$DD_A3M_MKTCAP <- subforna(out$DD_A3M/out$MKTCAP,NAvalue)
    out$PRICE<-subforna(out$PRICE,NAvalue)
    out$SHRINSD<-subforna(out$SHRINSD,NAvalue)
    out$SHRINST<-subforna(out$SHRINST,NAvalue)
    for (x in c(
        "PRCHG_04W","PRCHG_13W","PRCHG_26W","PRCHG_52W","RS_04W","RS_13W","RS_26W","RS_52W",
        "RSW_4Q","PR_PRH_52W")){
        out[,x]<-subforna(out[,x],NAvalue)
    }        
    for (x in c(
        "PRICEL_52W","SHR_AQ1","SHR_AQ2","SHR_AQ3","SHR_AQ4","SHR_AQ5","SHR_AQ6","SHR_AQ7","SHR_AQ8",
        "SHR_AY1","SHR_AY2","SHR_AY3","SHR_AY4","SHR_AY5","SHR_AY6","SHR_AY7", "AVD_10D", "AVM_03M",
        "INSTPS", "INSTSS","INSDPS", "INSDSS"
    )){
      out[,x]<-NULL  
    }
    return(out)
}
get_ee_data<-function(strDate,NAvalue=NA){
    si_ee_flds <- get_si_selected_fld_list()[["ee"]]
    out<-load_fields_from_file("si_ee",rdata.folder,strDate,si_ee_flds)
    for (x in c("EPS_EG5","EPS_EY0EY1","EPSPM_EG5","EPSH_EG5","EPSL_EG5",
                "EPSN_EG5","EPSN_EQ0","EPSN_EQ1","EPSN_EY0","EPSN_EY1","EPSN_EY2",
                "EPSUM_EG5","EPSUM_EQ0","EPSUM_EQ1","EPSUM_EY0","EPSUM_EY1","EPSUM_EY2",
                "EPSDM_EG5","EPSDM_EQ0","EPSDM_EQ1","EPSDM_EY0","EPSDM_EY1","EPSDM_EY2",
                "EPSSD_EG5","EPSSD_EQ0","EPSSD_EQ1","EPSSD_EY0","EPSSD_EY1","EPSSD_EY2",
                "EPSDMP_EQ0","EPSDMP_EQ1","EPSDMP_EY0","EPSDMP_EY1","EPSDMP_EY2",
                "QS_PERC","QS_SD","QS_SUE_Q1","EPS_GH1E0","EPS_EY0EY1","EPS_EY1EY2")){
        out[,x] <- subforna(out[,x],NAvalue)
    }
    out$EPSSD_EY0 <- subforna(out$EPSSD_EY0,0)    
    return(out)
}
get_perc_data<-function(strDate,NAvalue=NA){
    si_perc_flds <- get_si_selected_fld_list()[["perc"]]
    out<-load_fields_from_file("si_perc",rdata.folder,strDate,si_perc_flds)
    for (x in si_perc_flds){
        out[,x] <- subforna(out[,x],NAvalue)
    }    
    return(out)
}
get_gross_profit<-function(strDate,NAvalue=NA){
    si_flds <- get_si_selected_fld_list()[["isa"]]
    isa<-load_fields_from_file("si_isa",rdata.folder,strDate,si_flds)
    si_flds <- get_si_selected_fld_list()[["bsa"]]
    bsa<-load_fields_from_file("si_bsa",rdata.folder,strDate,si_flds)
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

yret<-function(InstallNum, nmonths,varname="RET"){
    # calculate the returns over the next nmonths AFTER InstallNum (not beginning with InstallNum)
    # in other words, calc the nmonth return after InstallNum
    iBefore<-103
    iAfter<-104
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
    iBefore<-103
    iAfter<-104
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
    ret<-yret(InstallNum,1,"Y_1M")
    if ((InstallNum+1)>length(sipbInstallDates)){
        ydata$Y_1M<-NA
    } else {
        ydata<-merge(ydata,ret,by="COMPANY_ID",all.x=T)    
    }
    #Get the next 3 month return
    ret<-yret(InstallNum,3,"Y_3M")
    if ((InstallNum+3)>length(sipbInstallDates)){
        ydata$Y_3M<-NA
    } else {
        ydata<-merge(ydata,ret,by="COMPANY_ID",all.x = T)        
    }
    # all.x=T keep companies that pass but don't have returns (avoid suvivorship bias)
    ret<-yret(InstallNum,6,"Y_6M")
    if ((InstallNum+6)>length(sipbInstallDates)){
        ydata$Y_6M<-NA
    } else {
        ydata<-merge(ydata,ret,by="COMPANY_ID",all.x = T)    
    }
    ret<-yret(InstallNum,12,"Y_12M")
    if ((InstallNum+12)>length(sipbInstallDates)){
        ydata$Y_12M<-NA
    } else {
        # adjust price return by dividends
        isa<-load_fields_from_file("si_isa",rdata.folder,sipbInstallDates[InstallNum+12],c("COMPANY_ID","DPS_12M"))
        psd<-load_fields_from_file("si_psd",rdata.folder,sipbInstallDates[InstallNum+12],c("COMPANY_ID","PRICE"))
        temp<-merge(isa,psd,by="COMPANY_ID")
        temp<-merge(out,ret,by="COMPANY_ID")
        temp$Y_12M <- 100*((temp$PRICE+temp$DPS_12M) / (temp$PRICE/(1+ret$Y_12M/100))-1)
        ret<-temp[,c("COMPANY_ID","Y_12M")]
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

create_all_x_and_y_files<-function(NAvalue=NA){
    load(paste(rdata.folder,"returnsmonthly.rdata",sep=""))
    for (i in 1:length(sipbInstallDates)){
        xdata<-create_xdata_file(i,NAvalue)
        ydata<-create_ydata_file(i)
    }
}

#create_all_x_and_y_files<-function(){  #Not used because this seems to use too much memory
#    lapply(seq_along(sipbInstallDates),create_xdata_file,NAvalue="median")
#    lapply(seq_along(sipbInstallDates),create_ydata_file)
#}

