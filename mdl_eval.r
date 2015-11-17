# Create data files (X,Y)
library(reshape2)
setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
source("SMWUtilities.r")
init_environment()

# This function is for evaluating original random forests
# The idea is that as more models are created, we will need to compare them. 
# To start I am looking at the results (mse and rsq) of the first 12 months
rf_mdl_eval<-function(i){
    library(randomForest)
    load(paste0(rdata.folder,"rf1mret",sipbInstallDates[i],".rdata"))
    out<-c(rf1$mse[rf1$ntree],rf1$rsq[rf1$ntree])
    load(paste0(rdata.folder,"rf12mret",sipbInstallDates[i],".rdata"))
    out<-c(out,rf1$mse[rf1$ntree],rf1$rsq[rf1$ntree])
    names(out)<-c("mse1","rsq1","mse12","rsq12")
    # rsq <- 1 - sum((y-predicted)^2)/sum((y-mean(y))^2)
    return(out)
}
rf_mdl_stats<-t(sapply(seq(1,12),rf_mdl_eval))
rownames(rf_mdl_stats)<-sipbInstallDates[1:12]
saveRDS(rf_mdl_stats,file="rf_mdl_stats.rds")
