
# Loop for 2011
# List Loop to Data Frame
require(pdftools)
require(stringr)
source("functions2.R")
folder <- "pdf2011"
files <- list.files(path =folder, "\\.pdf")
file_path <- paste0(folder, "/", files[[2]])
text <- pdftools::pdf_text(file_path)

extract_date(text)

# Test - what if we just say "SUPERVISOR.+\\s+PRESENT"
#   Does this cause issues? Yes. It stops at the wrong "PRESENT"
text1 <- toupper(text) 
text2 <- str_squish(text1)
text3 <- str_extract(text2, "ROLL CALL\\. ?.+(?=QUORUM)")
str_extract(text3, "SUPERVISOR.+WERE+\\s+PRESENT")
text4 <- str_extract(text3, "SUPERVISOR.+WERE\\s+PRESENT")
text5 <- str_replace(text4, " AND", ",")
text6 <- str_remove(text5, "SUPERVISOR |SUPERVISORS ")
text7 <- str_remove(text6, "WERE PRESENT|WAS PRESENT")
text8 <- str_remove_all(text7, "\\s")
text9 <- str_split(text8, ",")
text9


extract_present(text)



extract_absent <- function(txt){
  txt1 <- toupper(txt) 
  # Remove whitespace around the txt and new lines (I think)
  txt2 <- str_squish(txt1)
  # Pull anything from "ROLL CALL." that is followed by "QUORUM"
  txt3 <- str_extract(txt2, "ROLL CALL\\. ?.+(?=QUORUM)")
  # Find the names of the supervisors who were present or the supervisor who was present 
  txt4 <- str_extract(txt3, "PRESENT\\..+WERE\\s+ABSENT|PRESENT\\..+WAS\\s+ABSENT")
  # Replace the word AND with a comma for seperation (will create an empty space)
  # This empty space will look like ", ," with or without the actual " ".
  txt5 <- str_replace_all(txt4, " AND", ",")
  # Remove the characters "PRESENT."
  txt6 <- str_remove(txt5, "PRESENT\\.")
  # Remove either the characters SUPERVISOR or SUPERVISORS
  txt7 <- str_remove(txt6, "SUPERVISOR |SUPERVISORS ")
  # Remove where it says either WERE ABSENT or WAS ABSENT
  txt8 <- str_remove(txt7, "WERE ABSENT|WAS ABSENT")
  # Remove all extra white space (It does not work the same with str_squish())
  txt9 <- str_remove_all(txt8, "\\s")
  # Split the string into smaller strings deliminated by commas
  txt10 <- str_split(txt9, ",")
  # Return the first set of values in the list of strings 
  return(txt10[[1]])
  # This still requires: 
  #   Every pdf must have the characters "ROLL CALL." followed by
  #     the supervisors who were present, absent, and/or vacant 
  #     and the characters "QUORUM" after all of it
  # This is how we select the roll call area
  #   Every pdf must say either WERE or WAS ABSENT 
  #   Every pdf must have someone present and it must be written "PRESENT."
  # Additionally:
  #   Removing any empty/blank names between commas
}

extract_absent(text)

extract_vacant <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- str_extract(txt3, "SUPERVISOR.+ VACANT")
  txt5 <- str_extract(txt4[[1]], "DISTRICT.+?VACANT")
  return(txt5)
}


# Does this work with 2010 pdfs? 
# Yes, and it helped reduce some cleaning in the review.R
# Does this work with 2012-2021 pdfs, if not, which ones?
# Checking 2012 now- Yes! It works with same problems from read in as 2011 (some pdfs are hard to read)

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





