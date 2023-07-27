
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

