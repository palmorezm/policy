
require(pdftools)
require(stringr)
source("functions.R")
files <- list.files(path ="pdfs", "\\.pdf")
# file_dates <- files
text <- files
df <- as.data.frame(x = c(extract_vacant2(
  pdf_text(paste0("pdfs/", files[[1]]))), "NULL", 
  paste0(
    str_extract(files[[1]], "\\d{4}"), "2009", 
    sep = "")), 
  col.names = c("Supervisor", "Status", "Date"))
df2 <- df 

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
  
  df <- rbind(df, present)
  
}


for(i in 1:length(files)){
  
  present <- if(detect_present2(
    pdftools::pdf_text(paste0("pdfs/",files[[i]]))) > 0){
    as.data.frame(x = c(extract_present2(
      pdf_text(paste0("pdfs/", files[[i]]))), "present", 
      paste0(
        str_extract(files[[i]], "\\d{4}"), "2009", 
        sep = "")), 
      col.names = c("Supervisor", "Status", "Date"))
  }
  
  df2 <- rbind(df2, present)
  
}


library(dplyr)
df <- data.frame(matrix(ncol = 3)) %>% 
  rename("Supervisor" = "X1", 
         "Status" = "X2", 
         "Date" = "X3")
# Check Files 1, 2, 4, 12, 13
# Should return District 1, NA, NA, District 12, and District #12 respectively 
files[[1]]
length(detect_present2(pdftools::pdf_text(paste0("pdfs/",files[[i]])))) > 0

if(length(detect_present2(pdftools::pdf_text(paste0("pdfs/",files[[2]])))) > 0){
  extract_present3(pdftools::pdf_text(paste0("pdfs/",files[[2]])))
}

extract_present3(pdftools::pdf_text(paste0("pdfs/",files[[13]])))

