
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

# Identify Alpha(s)
alphas <- extract_alphas(text)
alphas <- str_remove_all(alphas[[1]], "[.]")
alphas <- alphas[alphas == LETTERS]
pattern <- paste0(alphas[1], "[.].+", alphas[(2)], "[.]")
alphansubs <- str_extract_all(
  str_extract_all(text4, "[:alnum:][.]"), "[:alnum:][.]")
string2 <- str_remove_all(alphansubs[[1]], "[.]")
string2[[2]]

string2 <- str_remove_all(alphansubs[[1]], "[.]")

alphas[length(alphas)]
text2_pattern <- paste0(string2[1], "[.].+", string2[2], "[.]")
str_extract(str_extract(text4, ), text2_pattern[1])

# Append a letter to the end of the text4 
# Add this letter to the alphas
# loop through each letter to extract its betweens
text4z <- paste(text4, "Z.")
string2[length(string2)+1] <- "Z"
alphas[length(alphas)+1] <- "Z"

# Extract everything between alphas
# Identify sub_alpha numbers
# Extract everything from alpha A to sub_alpha 1
# Repeat until reaching B

# Extract everything between alphas
alpha_pattern <- paste0(alphas[1], "[.].+", alphas[(1+1)], "[.]")
alpha_text <- str_extract(text4z, alpha_pattern[1])
# Identify sub_alpha numbers
string <- str_remove_all(str_extract_all(alpha_text, "[:digit:][.]|[:digit:]\\)")[[1]], "[.]")
paste(alpha[1], "")

string2_alphas <- str_extract_all(string2, "[:alpha:]")
string2_alphas <- unlist(string2_alphas)
string2_alphas

if(string2[2] == "1"){
  str_extract(str_extract(text4, "A[.].+B[.]"), "A[.].+1[.]")
} else {
  str_extract(text4, "A[.].+B[.]")
} 

# Test # Ah?

alpha_pattern <- paste0(alphas[1], "[.].+", alphas[(2)], "[.]")
string <- str_remove_all(
  str_extract_all(
    str_extract(text4z, alpha_pattern[1]), "[:alnum:][.]")[[1]], "[.]")
text_pattern <- paste0(string[1], "[.].+", string[2], "[.]")
str_extract(str_extract(text4z, alpha_pattern[1]), text_pattern[1])

for(i in 1:length(alphas)){
  alpha_pattern <- paste0(alphas[i], "[.].+", alphas[(i+1)], "[.]")
  string <- str_remove_all(
    str_extract_all(
      str_extract(text4z, alpha_pattern[1]), "[:alnum:][.]")[[1]], "[.]")
  text_pattern <- paste0(string[1], "[.].+", string[2], "[.]")
  txt <- str_extract(str_extract(text4z, alpha_pattern[1]), text_pattern[1])
  print(txt)
}

alphas



# Bring together all 
require(pdftools)
require(stringr)
source("functions3.r")
folder <- "all"
files <- list.files(path =folder, "\\.pdf")
file_path <- paste0(folder, "/", files[[1]])
text <- pdftools::pdf_text(file_path)

# Clean up text for extraction
text1 <- str_c(text[1], text[2])
text2 <- str_squish(text1)
text3 <- str_remove(text2, ".+(?=NEW BUSINESS)")
text4 <- str_extract(text3, "(?<=NEW BUSINESS).+(?=ADJOURNMENT)")
text5 <- str_extract_all(text4, "[:alpha:][.]")

# Need to get the last bit from H. to Z. and then Z. to [something here]
# We already appended a letter to the end of the text4
# Added this letter to the alphas
text4z <- paste(text4, "Z.")

extract_alphas <- function(txt){
  # Combine page 1 and 2 since the new business often span the two
  txt1 <- str_c(txt[1], txt[2]) 
  # Extract everything between "NEW BUSINESS" and "ADJOURNMENT"
  txt2 <- str_extract( 
    str_remove(str_squish(txt1), ".+(?=NEW BUSINESS)"), 
    "(?<=NEW BUSINESS).+(?=ADJOURNMENT)")
  # Extract all alpha characters that are followed by a "."
  txt3 <- str_extract_all(txt2, "[:alpha:][.]")
  # Return the result as a list 
  return(txt3)
}

alphas <- str_remove_all(extract_alphas(text)[[1]], "[.]")
alphas[length(alphas)+1] <- "Z"
# alpha_pattern <- paste0(alphas[1], "[.].+", alphas[(2)], "[.]")
# string <- str_remove_all(
#   str_extract_all(
#     str_extract(text4z, alpha_pattern[1]), "[:alnum:][.]")[[1]], "[.]")
# text_pattern <- paste0(string[1], "[.].+", string[2], "[.]")
# txt <- str_extract(str_extract(text4z, alpha_pattern[1]), text_pattern[1])
# print(txt)

for(i in 1:length(alphas)){
  alpha_pattern <- paste0(alphas[i], "[.].+", alphas[(i+1)], "[.]")
  string <- str_remove_all(
    str_extract_all(
      str_extract(text4z, alpha_pattern[1]), "[:alnum:][.]")[[1]], "[.]")
  text_pattern <- paste0(string[1], "[.].+", string[2], "[.]")
  txt <- str_extract(str_extract(text4z, alpha_pattern[1]), text_pattern[1])
  print(txt)
}




output <- ""
for(i in 1:length(alphas)){
  alpha_pattern <- paste0(alphas[i], "[.].+", alphas[(i+1)], "[.]")
  string1 <- str_remove_all(
    str_extract_all(
      str_extract(text4, alpha_pattern[1]), "[:alnum:][.]")[[1]], "[.]")
  for(z in 1:length(string1)){
    text_pattern <- paste0(string1[z], "[.].+", string1[z+1], "[.]")
    text <- str_extract(
      str_extract(text4, alpha_pattern[1]),
      text_pattern[1])
    output <- rbind(output, text)
  }
}

results <- list()
for(i in 1:length(alphas)){
  alpha_pattern <- paste0(alphas[i], "[.].+", alphas[(i+1)], "[.]")
  string1 <- str_remove_all(
    str_extract_all(
      str_extract(text4, alpha_pattern[1]), "[:alnum:][.]")[[1]], "[.]")
  results <- rbind(results, string1)
}

string2 <- str_remove_all(
  str_extract_all(
    str_extract_all(text4, "[:alnum:][.]"), "[:alnum:][.]")[[1]], "[.]")
string2


test <- na.omit(output)

test <- na.omit(output)[,1]

extract_nb(text)

