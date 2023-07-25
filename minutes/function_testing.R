
require(pdftools)
require(stringr)
source("functions3.r")
folder <- "all"
files <- list.files(path =folder, "\\.pdf")
file_path <- paste0(folder, "/", files[[1]])
text <- pdftools::pdf_text(file_path)
files[[1]]

text1 <- toupper(text)
text2 <- str_squish(text1)
str_extract_all(text2, "NEW BUSINESS  ADJOURNMENT") 
str_sub(text2, start = "NEW BUSINESS", end = "ADJOURNMENT")
str_subset(text2, pattern = "NEW BUSINESS *.?")
str_extract_all(text2[[1]], "NEW BUSINESS [/S/s]?")
str_extract_all(text2, "NEW BUSINESS (?s:.)(?s:.)(?s:.)(?s:.)(?s:.)(?s:.)")
text3 <- str_split(text2, "NEW BUSINESS")
str_detect(text2, "NEW BUSINESS")
str_squish(text2)
?str_squish()

str_extract(str_squish(text2), "/(NEW BUSINESS)(.+?)(ADJOURNMENT)/s;")
str_length(text2)
index <- str_detect(text2, "NEW BUSINESS")
text3 <- text2[index]
print(text3)
text3 <- str_c(text2[[1]], text[[2]], text[[3]])
str_remove(text3, "(?<=NEW BUSINESS)*.?")
?str_subset()
str_extract(text2)
text3
str_extract(text3, pattern = "(?<=NEW)[[/s/S]]*?(?=ADJOURN)")
which(str_extract_all(text3[[1]], pattern = "(?s:.)") == "NEW")
str_match_all(text3, "NEW BUSINESS | ADJOURNMENT")
text3 <- str_split(string = text3, "NEW BUSINESS")
text3[1]
?str_split()
str_extract()
str_(text2, pattern = "NEW BUSINESS")
writeLines(text2)
text3 <- str_squish(text2)
text4 <- str_replace_all(text3, " ", "")
str_extract(text4, pattern = ".+(?=NEWBUSINESS)")
text5 <- str_remove(text4, ".+(?=NEWBUSINESS)")
text6 <- str_remove(text5, "(?=ADJOURNMENT).+")
text7 <- str_remove(text6, "PROCEEDINGSOFTHEROCKCOUNTYBOARDOFSUPERVISORS")
str_extract_all(text5, ".+(?=ADJOURNMENT)")

text6 <- str_c(text[1], text[2])
str_extract(str_replace_all(text6, "\n", ""), "(?=NEWBUSINESS).+")
text7 <- str_squish(text6)
text8 <- str_remove(text7, ".+(?=NEW BUSINESS)")
text9 <- str_extract(text8, "(?<=NEW BUSINESS).+(?=ADJOURNMENT)") # Got it!
text10 <- str_extract_all(text9, "[:alpha:][.]")
# Use text10 alphas to look for number between them
# N. is not a value - they must be in order A, b, c... 


text7 <- str_remove_all(text6, "PROCEEDINGSOFTHEROCKCOUNTYBOARDOFSUPERVISORS.*?,\\d{4}")
text7
str_extract(text4, pattern = "(?<=NEWBUSINESS).+")
print(text4)
extract_present <- function(txt){
  txt1 <- toupper(txt) 
  # Remove whitespace around the text and new lines (I think)
  txt2 <- str_squish(txt1)
  # Pull anything from "ROLL CALL." that is followed by "QUORUM"
  txt3 <- str_extract(txt2, "ROLL CALL\\. ?.+(?=QUORUM)")
  # Find the names of the supervisors who were present or the supervisor who was present 
  txt4 <- str_extract(txt3, "SUPERVISOR.+WERE\\s+PRESENT|SUPERVISOR.+WAS\\s+PRESENT")
  # Replace the word AND with a comma for seperation (will create an empty space)
  # This empty space will look like ", ," with or without the actuall " ".
  txt5 <- str_replace_all(txt4, " AND", ",")
  # Remove where it says either SUPERVISOR or SUPERVISORS
  txt6 <- str_remove(txt5, "SUPERVISOR |SUPERVISORS ")
  # Remove where it says either WERE PRESENT or WAS PRESENT
  txt7 <- str_remove(txt6, "WERE PRESENT|WAS PRESENT")
  # Remove all extra whitespace (It does not work the same with str_squish() )
  txt8 <- str_remove_all(txt7, "\\s")
  # Split the string into smaller strings deliminated by commas
  txt9 <- str_split(txt8, ",")
  # Return the first set of values in the list of strings 
  return(txt9[[1]])
  # This still requires: 
  #   Every pdf must have the characters "ROLL CALL." followed by
  #     the supervisors who were present, absent, and/or vacant 
  #     and the characters "QUORUM" after all of it
  # This is how we select the roll call area
  #   Every pdf must say either WERE or WAS PRESENT 
  # Additionally:
  #   Removing any empty/blank names between commas
  #   
}

