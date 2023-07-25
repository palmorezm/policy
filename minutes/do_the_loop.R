
# Do the loop! 
# 2010 - 2021 (After Testing each individually in loop_2012.R)
require(pdftools)
require(stringr)
source("functions3.r")
folder <- "all"
files <- list.files(path =folder, "\\.pdf")
file_path <- paste0(folder, "/", files[[2]])
text <- pdftools::pdf_text(file_path)

status <- list(
  vacant = (if(!is.null(
    if(sum(is.na(extract_vacant(text))) == 0){
      extract_vacant(text)})
  ){extract_vacant(text)} else{
    NA
  }),
  
  absent = (if(!is.null(
    if(sum(is.na(extract_absent(text))) == 0){
      extract_absent(text)})
  ){extract_absent(text)} else{
    NA
  }),
  
  present = (if(!is.null(
    if(sum(is.na(extract_present(text))) == 0){
      extract_present(text)})
  ){extract_present(text)} else{
    NA
  })
)

vacant <- data.frame(supervisor = status$vacant, 
                     date = extract_date(text), 
                     status ="vacant")
absent <- data.frame(supervisor = status$absent, 
                     date = extract_date(text),
                     status ="absent")
present <- data.frame(supervisor = status$present, 
                      date = extract_date(text),
                      status ="present")
df <- rbind(vacant, absent, present)

for(i in 2:length(files)){
  file_path <- paste0(folder, "/", files[[i]])
  text <- pdftools::pdf_text(file_path)
  status <- list(
    vacant = (if(!is.null(
      if(sum(is.na(extract_vacant(text))) == 0){
        extract_vacant(text)})
    ){extract_vacant(text)} else{
      NA
    }),
    
    absent = (if(!is.null(
      if(sum(is.na(extract_absent(text))) == 0){
        extract_absent(text)})
    ){extract_absent(text)} else{
      NA
    }),
    
    present = (if(!is.null(
      if(sum(is.na(extract_present(text))) == 0){
        extract_present(text)})
    ){extract_present(text)} else{
      NA
    })
  )
  vacant <- data.frame(supervisor = status$vacant, 
                       date = extract_date(text), 
                       status ="vacant")
  absent <- data.frame(supervisor = status$absent, 
                       date = extract_date(text),
                       status ="absent")
  present <- data.frame(supervisor = status$present, 
                        date = extract_date(text),
                        status ="present")
  df <- rbind(df, vacant, absent, present)
  print(paste("PDF", i, "was processed"))
}

rm(list=c(ls()[which(ls() != "df")]))
# rm(list = c("absent", "present", "vacant", "status"))
# rm(list = c("file_path", "files", "folder", "i", "text"))
# rm(list = c("extract_absent", "extract_date", 
#             "extract_present", "extract_vacant"))
