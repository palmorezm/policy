
# List Loop to Data Frame
require(pdftools)
require(stringr)
source("functions2.r")
folder <- "pdf2"
files <- list.files(path =folder, "\\.pdf")
file_path <- paste0(folder, "/", files[[1]])
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
}

# df$date2 <- as.Date(df$date, "%B %d, %Y")
# df$supervisor <- factor(df$supervisor)
# levels(df$supervisor)

 

  
