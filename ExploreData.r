setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
load("sipbInstallDates.rdata")
rdata.folder <- "D:/SIPro/rdata/"

load(paste(rdata.folder,"testdata.rdata",sep=""))
summary(x2)

mktcap.rank<-rank(x2$MKTCAP)
x2.3000<-x2[mktcap.rank>(nrow(x2)-2999),]
summary(x2$MKTCAP)
summary(x2.3000$MKTCAP)
summary(x2.3000)

aapl.idx <- x90$TICKER=="AAPL"
x90[aapl.idx,]
x90[aapl.idx,c("PRICE","PRICE_DATE","INSTALLDT")]
