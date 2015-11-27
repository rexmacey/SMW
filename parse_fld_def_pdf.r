setwd("C:/Users/Rex/Documents/Quant Trading/SMW")
source("smwutilities.r")
init_environment()

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
data<-pdf$content
idx<-regexpr("Stock Investor Pro Field Definitions",data)
data<-data[idx<=0] #remove page headers
idx<-data==""
data<-data[!idx] #remove blank lines

fldnum<-1
i<-0

colontrim<-function(s){
    p<-regexpr(":",s)
    s<-substr(s,p+2,nchar(s))
    return(s)
}
while (TRUE){
    i<-i+1
    if (i>length(data)) break
    if (substr(data[i],1,10) == "Data Table") {
        flddefs[fldnum,"name"]<-colontrim(data[i-1])
        flddefs[fldnum,"table"]<-colontrim(data[i])
        flddefs[fldnum,"category"]<-colontrim(data[i+1])
        flddefs[fldnum,"type"]<-colontrim(data[i+2])
        flddefs[fldnum,"percentrank"]<-colontrim(data[i+3])
        flddefs[fldnum,"ismedian"]<-colontrim(data[i+4])
        flddefs[fldnum,"desc"]<-""
        i<-i+5
        while (TRUE){
            flddefs[fldnum,"desc"]<-paste(flddefs[fldnum,"desc"],data[i])
            i<-i+1
            if (i > length(data)) break
            if (i+1 > length(data)) break
            if (substr(data[i+1],1,10)=="Data Table") break
        }
        fldnum<-fldnum+1
    }
}

saveRDS(flddefs,file="SIP_field_defs.rds")
write.csv(flddefs,file="SIP_field_defs.csv")

