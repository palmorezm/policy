
require(pdftools)
require(stringr)
source("functions2.R")
files <- list.files(path ="pdfs", "\\.pdf")
# file_dates <- files
text <- files
df2 <- as.data.frame(x = c(extract_vacant2(
  pdf_text(paste0("pdfs/", files[[1]]))), "NULL", 
  paste0(
    str_extract(files[[1]], "\\d{4}"), "2009", 
    sep = "")), 
  col.names = c("Supervisor", "Status", "Date"))

for(i in 1:length(files)){
  
  absent <- if(detect_absent(
    pdftools::pdf_text(paste0("pdfs/",files[[i]]))) == "TRUE"){
    as.data.frame(x = c(extract_absent(
      pdf_text(paste0("pdfs/", files[[i]]))), "absent", 
      paste0(
        str_extract(files[[i]], "\\d{4}"), "2009", 
        sep = "")), 
      col.names = c("Supervisor", "Status", "Date"))
  }
  
  df2 <- rbind(df, absent)
  
}


if(length(detect_absent2(pdftools::pdf_text(paste0("pdfs/",files[[12]])))) > 0){
  extract_absent2(pdftools::pdf_text(paste0("pdfs/",files[[12]])))
}


if(length(detect_present2(pdftools::pdf_text(paste0("pdfs/",files[[12]])))) > 0){
      extract_present3(pdftools::pdf_text(paste0("pdfs/",files[[12]])))
    }

if(length(detect_absent2(pdftools::pdf_text(paste0("pdfs/",files[[12]])))) > 0){
      extract_absent2(pdftools::pdf_text(paste0("pdfs/",files[[12]])))
    }

source("functions2.R")
files <- list.files(path ="pdfs", "\\.pdf")
is.na(extract_vacant(pdftools::pdf_text(paste0("pdfs/",files[[13]]))))
vacant <- list(
if(is.na(
  extract_vacant(
    pdftools::pdf_text(
      paste0("pdfs/",files[[13]])))) == "FALSE"){
 extract_vacant(
   pdftools::pdf_text(
     paste0("pdfs/",files[[13]]))) 
}
)




list(
  
  vacant=(
    if(sum(is.na(
  extract_vacant(
    pdftools::pdf_text(
      paste0("pdfs/",files[[13]]))))) == 0){
  extract_vacant(
    pdftools::pdf_text(
      paste0("pdfs/",files[[13]])))
  }),
  
  absent=(
    if(sum(is.na(
    extract_absent(
      pdftools::pdf_text(
        paste0("pdfs/",files[[1]]))))) == 0){
    extract_absent(
      pdftools::pdf_text(
        paste0("pdfs/",files[[1]])))
  }),
  
  present=(
    if(sum(is.na(
      extract_present(
        pdftools::pdf_text(
          paste0("pdfs/",files[[1]]))))) == 0){
      extract_present(
        pdftools::pdf_text(
          paste0("pdfs/",files[[1]])))
    })
)


# Place this in a for loop
# Only runs pdf reader 1 time 
file_path <- paste0("pdfs/",files[[13]])
text <- pdftools::pdf_text(file_path)

list(
  # Extract the date from the pdf
  date = c("date"), # not an if statement bc every pdf should have it
  
  vacant = (if(sum(is.na(extract_vacant(text))) == 0){
    extract_vacant(text)
  }),
  absent = (if(sum(is.na(extract_absent(text))) == 0){
    extract_absent(text)
  }),
  present = (if(sum(is.na(extract_present(text))) == 0){
    extract_present(text)
  })
  
)




