
# Loop for A Single Year
# 
library(pdftools)
require(stringr)
source("functions.R")
files <- list.files(path ="pdfs", "\\.pdf")
file_dates <- files
text <- files
df <- as.data.frame(x = c(extract_vacant2(
  pdf_text(paste0("pdfs/", files[[1]]))), "NULL", 
  paste0(
    str_extract(files[[1]], "\\d{4}"), "2009", 
    sep = "")), 
  col.names = c("Supervisor", "Status", "Date"))

for(i in 1:length(files)){
  
  present <- if(detect_present(
    pdftools::pdf_text(paste0("pdfs/",files[[i]]))) == "TRUE"){
    as.data.frame(x = c(extract_present(
      pdf_text(paste0("pdfs/", files[[i]]))), "present", 
      paste0(
        str_extract(files[[i]], "\\d{4}"), "2009", 
        sep = "")), 
      col.names = c("Supervisor", "Status", "Date"))
  }
  
  absent <- if(detect_absent(
    pdftools::pdf_text(paste0("pdfs/",files[[i]]))) == "TRUE"){
    as.data.frame(x = c(extract_absent(
      pdf_text(paste0("pdfs/", files[[i]]))), "absent", 
      paste0(
        str_extract(files[[i]], "\\d{4}"), "2009", 
        sep = "")), 
      col.names = c("Supervisor", "Status", "Date"))
  }
  
  vacant <- if(detect_vacant(
    pdftools::pdf_text(paste0("pdfs/",files[[i]]))) == "TRUE"){
    as.data.frame(x = c(extract_vacant2(
      pdf_text(paste0("pdfs/", files[[i]]))), "vacant", 
      paste0(
        str_extract(files[[i]], "\\d{4}"), "2009", 
        sep = "")), 
      col.names = c("Supervisor", "Status", "Date"))
  }
  
  df <- rbind(df, present, absent, vacant)
}



