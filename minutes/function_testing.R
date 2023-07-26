
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
text3 <- str_split(text2, "NEW BUSINESS")
index <- str_detect(text2, "NEW BUSINESS")
text3 <- text2[index]
text3 <- str_c(text2[[1]], text[[2]], text[[3]])
text3 <- str_split(string = text3, "NEW BUSINESS")
text3 <- str_squish(text2)
text4 <- str_replace_all(text3, " ", "")
str_extract(text4, pattern = ".+(?=NEWBUSINESS)")
text5 <- str_remove(text4, ".+(?=NEWBUSINESS)")
text6 <- str_remove(text5, "(?=ADJOURNMENT).+")
text6 <- str_c(text[1], text[2])
str_extract(str_replace_all(text6, "\n", ""), "(?=NEWBUSINESS).+")
text7 <- str_squish(text6)
text8 <- str_remove(text7, ".+(?=NEW BUSINESS)")
text9 <- str_extract(text8, "(?<=NEW BUSINESS).+(?=ADJOURNMENT)") # Got it!
text10 <- str_extract_all(text9, "[:alpha:][.]")
# Use text10 alphas to look for number between them
# N. is not a value - they must be in order A, b, c... 

text1 <- str_c(text[1], text[2])
text2 <- str_squish(text1)
text3 <- str_remove(text2, ".+(?=NEW BUSINESS)")
text4 <- str_extract(text3, "(?<=NEW BUSINESS).+(?=ADJOURNMENT)")
text5 <- str_extract_all(text4, "[:alpha:][.]")

nb <- function(txt){
  txt1 <- str_c(txt[1], txt[2])
  txt2 <- str_extract(
    str_remove(str_squish(txt1), ".+(?=NEW BUSINESS)"), 
    "(?<=NEW BUSINESS).+(?=ADJOURNMENT)")
  txt3 <- str_extract_all(txt2, "[:alpha:][.]")
  return(txt3)
}

alphas <- nb(text)
alphas[[1]][1] # Due to list structure
text5 <- paste0(text4, "WQZ.")
# alphas <- factor(alphas, ordered = is.ordered(LETTERS))
alphas <- str_remove(alphas[[1]], "[.]")
alphas[length(alphas)+1] <- "WQZ"

# Demo 1
pattern <- paste0(alphas[3], "[.].+", alphas[4], "[.]")
str_extract(text5, pattern[1])

# Test for all alphas and Review
for(i in 1:length(alphas)){
  pattern <- paste0(alphas[i], "[.].+", alphas[(i+1)], "[.]")
  string <- str_extract(text5, pattern[1])
  print(string)
}

# Test with logical exclusion of letters that are not in A, B, C... order
alphas2 <- alphas[alphas[-length(alphas)] == LETTERS]
alphas2[length(alphas2)+1] <- "WQZ"

for(i in 1:length(alphas2)){
  pattern <- paste0(alphas2[i], "[.].+", alphas2[(i+1)], "[.]")
  string <- str_extract(text5, pattern[1])
  print(string)
}

# The function
alphas <- nb(text)
alphas <- str_remove(alphas[[1]], "[.]")
alphas <- alphas[alphas == LETTERS]
alphas[length(alphas)+1] <- "WQZ"

pattern <- paste0(alphas[2], "[.].+", alphas[(3)], "[.]")
str_extract(text5, pattern[1])

output <- data.frame()
for(i in 1:length(alphas)){
  pattern <- paste0(alphas[i], "[.].+", alphas[(i+1)], "[.]")
  string <- str_extract(text5, pattern[1])
  output <- rbind(output, string)
}

pattern <- paste0(alphas[1], "[.].+", alphas[(2)], "[.]")
removal_pattern <- paste0(alphas[2], "[.]")
# str_remove(str_extract(text5, pattern[1]), removal_pattern)
string <- str_extract(text5, pattern[1])
alpha_subs <- str_extract_all(string, "\\d[.]")
alpha_subs <- str_remove_all(alpha_subs[[1]], "[.]")
nums <- seq(1, length(alpha_subs), by = 1)
alpha_subs <- alpha_subs[alpha_subs == nums]
alpha_subs[length(alpha_subs)+1] <- "WQZ"
string <- paste(string, "WQZ.")
pattern_subs <- paste0(alpha_subs[1], "[.].+", alpha_subs[2], "[.]")
str_extract(string, pattern_subs)

alphas <- extract_alphas(text)
alphas <- str_remove_all(alphas[[1]], "[.]")
pattern <- paste0(alphas[1], "[.].+", alphas[(2)], "[.]")

string <- str_extract(text5, "A[.].+B[.]")
str_extract_all(string, "\\d[.]")


alphansubs <- str_extract_all(
  str_extract(text5, pattern[1]), "[:alnum:][.]")

str_extract(text4, "[:alnum:][.].+[:alnum:][.]")
items <- str_extract_all(text4, "[:alnum:][.]")
nums <- str_extract(items[[1]], "\\d")
nums
items[[1]][is.na(str_extract(items[[1]], "[:alpha:][.]"))]


for(i in 1:length(alphas)){
  pattern <- paste0(alphas[i], "[.].+", alphas[(i+1)], "[.]")
  string <- str_extract(text5, pattern[1])
  output <- rbind(output, string)
}


string1 <- str_remove_all(
  str_extract_all(
    str_extract(text4, "A[.].+B[.]"), "[:alnum:][.]")[[1]], "[.]")

# If the first number is a 1 after the first letter
# then extract everything between the letter and the first number
# then everything between the number and the second number and so on
# until you read the next letter

string1 <- str_remove_all(
  str_extract_all(
    str_extract(text4, "A[.].+B[.]"), "[:alnum:][.]")[[1]], "[.]")

if(string1[2] == "1"){
  str_extract(str_extract(text4, "A[.].+B[.]"), "A[.].+1[.]")
} else {
  str_extract(text4, "A[.].+B[.]")
}

# Identify Alpha(s)
alphas <- extract_alphas(text)
alphas <- str_remove_all(alphas[[1]], "[.]")
alphas <- alphas[alphas == LETTERS]
pattern <- paste0(alphas[1], "[.].+", alphas[(2)], "[.]")
alphansubs <- str_extract_all(
  str_extract(text4, pattern[1]), "[:alnum:][.]")

string1 <- str_remove_all(
  str_extract_all(
    str_extract(text4, "A[.].+B[.]"), "[:alnum:][.]")[[1]], "[.]")




for(i in 1:length(alphas)){
  pattern <- paste0(alphas[i], "[.].+", alphas[(i+1)], "[.]")
  string <- str_extract(text5, pattern[1])
  output <- rbind(output, string)
}



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

