library(tm)

uri <- "proflddefs.pdf"
pdfReader <- tm::readPDF(control = list(text = "-layout"))
pdfParsed <- pdfReader(elem = list(uri = uri),
                       language = "en",
                       id = "id1")

flddefs<-data.frame(name=character(),table=character(),category=character(),type=character(),
                    percentrank=character(),ismedian=character(),desc=character(),stringsAsFactors = FALSE)

data <- pdfParsed$content
data <- gsub("\xad", "-", data) # fix some weird dashes
data <- gsub("\xae", "-", data) # remove copyright character
data <- data[!grepl("Stock Investor Pro Field Definitions", data)] # remove page header
data <- data[data != ""] # remove blank lines
data <- sub("Data Table Name ", "Data Table Name:", data) # fixes one bad record

# identify the blocks of data defining each field and split into a list
table_idx <- grep("Data Table Name:", data)
name_idx  <- table_idx - 1L
stopifnot(1L %in% name_idx)
is_new_field <- seq_along(data) %in% name_idx
field_idx <- cumsum(is_new_field)
field_list <- split(data, field_idx)

# a function to check our assumptions for the data of each field
is_valid_field <- function(dat) {
   grepl("Data Table Name:",         dat[2]) &
   grepl("Data Category:",              dat[3]) &
   grepl("Field Type:",                 dat[4]) &
   grepl("Percent Rank:",               dat[5]) &
   grepl("(Industry|Company)/Sector Median[s]?:", dat[6]) &
   !any(grepl("Data Table Name", tail(dat, -6)))
}
bad_data <- Filter(Negate(is_valid_field), field_list)
if (length(bad_data) > 0L) {print(bad_data); stop("BAD DATA!!! See above")}
good_data <- Filter(is_valid_field, field_list)

# a function to process the data of a field into a one row data.frame
parse_field <- function(dat) {
   colontrim <- function(s) sub(".*: ", "", s)
   data.frame(name        = colontrim(dat[1]),
              table       = colontrim(dat[2]),
              category    = colontrim(dat[3]),
              type        = colontrim(dat[4]),
              percentrank = colontrim(dat[5]),
              ismedian    = colontrim(dat[6]),
              desc        = paste(tail(dat, -6), collapse = " "),
              stringsAsFactors = FALSE)
}

flddefs_list <- lapply(good_data, parse_field)
flddefs <- do.call(rbind, flddefs_list)

saveRDS(flddefs, file = "SIP_field_defs.rds")
write.csv(flddefs, file = "SIP_field_defs.csv")

