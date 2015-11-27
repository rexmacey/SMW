setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
source("smwutilities.r")
init_environment()

i<-1
load(paste(rdata.folder,"xdata",sipbInstallDates[i],".rdata",sep = "")) # load xdata file
na.pct<-colSums(is.na(xdata))/nrow(xdata)
write.csv(na.pct,file="na_pct.csv")


uri <- "C:/Users/Rex/Documents/Quant Trading/SMW/proflddefs.pdf"
if(all(file.exists(Sys.which(c("pdfinfo", "pdftotext"))))) {
    pdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = uri),
                                                     language = "en",
                                                     id = "id1")
    content(pdf)[1:13]
} else {
    print("files do not exist")
}

flddefs<-data.frame(name=character(),table=character(),category=character(),type=character(),
                    percentrank=character(),ismedian=character(),desc=character(),stringsAsFactors = FALSE)

#ignore first line which is title
pdf$content[1]
fldnum<-1
i<-1

colontrim<-function(s){
    p<-regexpr(":",s)
    s<-substr(s,p+2,nchar(s))
    return(s)
}
while (TRUE){
    i<-i+1
    if (i>length(pdf$content)) break
    if (substr(pdf$content[i],1,10) == "Data Table") {
        flddefs[fldnum,"name"]<-colontrim(pdf$content[i-1])
        flddefs[fldnum,"table"]<-colontrim(pdf$content[i])
        flddefs[fldnum,"category"]<-colontrim(pdf$content[i+1])
        flddefs[fldnum,"type"]<-colontrim(pdf$content[i+2])
        flddefs[fldnum,"percentrank"]<-colontrim(pdf$content[i+3])
        flddefs[fldnum,"ismedian"]<-colontrim(pdf$content[i+4])
        flddefs[fldnum,"desc"]<-pdf$content[i+6]
        i<-i+7
        if (i>length(pdf$content)) break
        while(pdf$content[i] != ""){
            flddefs[fldnum,"desc"]<-paste(flddefs[fldnum,"desc"],pdf$content[i])
            i<-i+1
            if (i>length(pdf$content)) break
        }
        fldnum<-fldnum+1
    }
}

