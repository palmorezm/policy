
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

output <- ""
for(i in 1:length(alphas)){
  alpha_pattern <- paste0(alphas[i], "[.].+", alphas[(i+1)], "[.]")
  string <- str_remove_all(
    str_extract_all(
      str_extract(text4z, alpha_pattern[1]), "[:alnum:][.]")[[1]], "[.]")
  text_pattern <- paste0(string[1], "[.].+", string[2], "[.]")
  txt <- str_extract(
    str_extract(text4z, alpha_pattern[1]), text_pattern[1])
  output <- rbind(output, txt)
}



