
# Run the loop on all the pdfs
require(pdftools)
require(stringr)
source("functions2.R")
files <- list.files(path ="all", "\\.pdf")
file_path <- paste0("all/",files[[1]])
text <- pdftools::pdf_text(file_path)
status <- list(
  vacant = (if(!is.null(
    if(sum(is.na(extract_vacant(text))) == 0){
      extract_vacant(text)[[1]]})
  ){extract_vacant(text)} else{
    NA
  }),
  
  absent = (if(!is.null(
    if(sum(is.na(extract_absent(text))) == 0){
      extract_absent(text)[[1]]})
  ){extract_absent(text)} else{
    NA
  }),
  
  present = (if(!is.null(
    if(sum(is.na(extract_present(text))) == 0){
      extract_present(text)[[1]]})
  ){extract_present(text)} else{
    NA
  })
)
vacant <- data.frame(supervisor = status$vacant[[1]], 
                     date = extract_date(text), 
                     status ="vacant")
absent <- data.frame(supervisor = status$absent[[1]], 
                     date = extract_date(text),
                     status ="absent")
present <- data.frame(supervisor = status$present[[1]], 
                      date = extract_date(text),
                      status ="present")
df <- rbind(vacant, absent, present)

for(i in 2:length(files)){
  file_path <- paste0("all/",files[[i]])
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
  vacant <- data.frame(supervisor = status$vacant[[1]], 
                       date = extract_date(text), 
                       status ="vacant")
  absent <- data.frame(supervisor = status$absent[[1]], 
                       date = extract_date(text),
                       status ="absent")
  present <- data.frame(supervisor = status$present[[1]], 
                        date = extract_date(text),
                        status ="present")
  df <- rbind(df, vacant, absent, present)
}

if(!is.null(
  if(sum(is.na(extract_present(text))) == 0){
    extract_present(text)})
){extract_present(text)} else{
  NA
}

if(is.list(if(!is.null(
  if(sum(is.na(extract_present(text))) == 0){
    extract_present(text)})
){extract_present(text)} else{
  NA
}))

(if(!is.null(
    if(sum(is.na(extract_absent(text))) == 0){
      extract_absent(text)})
  ){extract_absent(text)} else{
    NA
})

text

extract_vacant(text)

file_path
