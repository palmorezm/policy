
# extract date 
require(pdftools)
require(stringr)
source("functions2.R")
source("test_text.R")

require(lubridate)
extract_date <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- str_extract(txt1, "JANESVILLE, WISCONSIN\n.+.?+, \\d{4}\n")
  txt3 <- str_remove(txt2, "JANESVILLE, WISCONSIN\n")
  txt4 <- str_remove_all(txt3, "\n")
  txt5 <- na.omit(str_squish(txt4))
  return(txt5[1])
}

file_path <- paste0("pdfs/",files[[5]])
text <- pdftools::pdf_text(file_path)
extract_date(text)

for(i in 1:length(files)){
  file_path <- paste0("pdfs/",files[[i]])
  text <- pdftools::pdf_text(file_path)
  tmp <- extract_date(text)
  print(tmp)
}
