

# Do the loop! 
# 2010 - 2021 (After Testing each individually in loop_2012.R)
require(pdftools)
require(stringr)
source("functions3.r")
source("functions4.r")
folder <- "all"
files <- list.files(path =folder, "\\.pdf")
file_path <- paste0(folder, "/", files[[2]])
text <- pdftools::pdf_text(file_path)

output <- list(
  policy = (if(!is.null(
    if(sum(is.na(extract_nb(text))) == 0){
      extract_nb(text)})
  ){extract_nb(text)} else{
    NA
  })
  # AYES Votes Here
  
  # No Votes Here
  
  # Abstain? Here
)

policy <- data.frame(supervisor = output$policy, 
                     date = extract_date(text), 
                     item ="new_business")

df <- rbind(policy)

df <- data.frame()
for(i in 1:length(files)){
  file_path <- paste0(folder, "/", files[[i]])
  text <- pdftools::pdf_text(file_path)
  output <- list(
    policy = (if(!is.null(
      if(sum(is.na(extract_nb(text))) == 0){
        extract_nb(text)})
    ){extract_nb(text)} else{
      NA
    })
  )
  policy <- data.frame(supervisor = output$policy, 
                       date = extract_date(text), 
                       item ="new_business")
  df <- rbind(df, policy)
  # Inform Me of Status
  print(paste("PDF", i, "was processed"))
}

rm(list=c(ls()[which(ls() != "df")]))
