


extract_present <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- str_extract(txt3, "SUPERVISOR.+ WERE PRESENT|WAS PRESENT")
  txt5 <- str_remove(txt4, "SUPERVISOR |SUPERVISORS ")
  txt6 <- str_remove(txt5, "WERE PRESENT|WAS PRESENT")
  txt7 <- str_replace(txt6, " AND", ",")
  txt8 <- na.omit(txt7)
  txt9 <- str_remove_all(txt8, " ")
  txt10 <- str_split(txt9, ",")
  return(txt10[[1]])
}

extract_absent <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\\n")
  txt3 <- str_extract_all(txt2[[1]], "SUPERVISOR.*ABSENT")
  txt4 <- gsub(".*SUPERVISOR.+WERE PRESENT|.*SUPERVISOR.+WAS PRESENT", "", txt3)
  txt5 <- str_extract(txt4, "SUPERVISOR.+?ABSENT")
  txt6 <- str_remove_all(txt5, "ABSENT|WAS|WERE")
  txt7 <- str_remove(txt6, "SUPERVISOR.*? ")
  txt8 <- str_replace_all(txt7, "AND", ",")
  txt9 <- str_replace_all(txt8, " ", "")
  txt10 <- str_split(txt9, ",")
  return(txt10[[1]])
}

extract_vacant <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- str_extract(txt3, "SUPERVISOR.+ VACANT")
  txt5 <- str_extract(txt4[[1]], "DISTRICT.+?VACANT")
  return(txt5)
}

# Extract date
extract_date <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- str_extract(txt1, "JANESVILLE, WISCONSIN\n.+.?+, \\d{4}\n")
  txt3 <- str_remove(txt2, "JANESVILLE, WISCONSIN\n")
  txt4 <- str_remove_all(txt3, "\n")
  txt5 <- na.omit(str_squish(txt4))
  return(txt5[1])
}
