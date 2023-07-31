
# functions4 
# Extracting Policy Information 

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


extract_nb <- function(txt){
  # create an object for storage of output
  output <- ""
  # identify the area of interest (the agenda)
  txt1 <- str_c(txt[1], txt[2])
  txt2 <- str_squish(txt1)
  txt3 <- str_remove(txt2, ".+(?=NEW BUSINESS)")
  txt4 <- str_extract(txt3, "(?<=NEW BUSINESS).+(?=ADJOURNMENT)")
  txt4z <- paste(txt4, "Z.") # Add 
  # identify the alpha characters for searching and extracting
  alphas <- str_remove_all(extract_alphas(txt)[[1]], "[.]")
  alphas[length(alphas)+1] <- "Z" # append z for last search
  # for every alpha present extract the new business
  for(i in 1:length(alphas)){
    # Set up pattern that adapts for every alpha present
    alpha_pattern <- paste0(alphas[i], "[.].+", alphas[(i+1)], "[.]")
    # get alpha numerics with a decimal 
    string <- str_remove_all(
      str_extract_all(
        str_extract(txt4z, alpha_pattern[1]), "[:alnum:][.]")[[1]], "[.]")
    txt_pattern <- paste0(string[1], "[.].+", string[2], "[.]")
    result <- str_extract(
      str_extract(txt4z, alpha_pattern[1]), txt_pattern[1])
    output <- rbind(output, result)
  }
  return(na.omit(data.frame(output)))
}



