
# Extraction Functions
# For PDFs

#########################
# ----- Roll Call ----- #
#########################
extract_present <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- stringr::str_extract(txt3, ".*QUORUM")
  txt5 <- str_extract(txt4, "SUPERVISORS.* WERE PRESENT")
  txt6 <- stringr::str_remove(txt5, "SUPERVISORS ")
  txt7 <- stringr::str_remove(txt6, " WERE PRESENT")
  txt8 <- na.exclude(txt7)
  txt9 <- str_replace(txt8, " AND", ",")
  txt10 <- str_remove_all(txt9, " ")
  txt10 <- str_split(txt10, ",")
  return(txt10)
}

# extract_present2 <- function(txt){
#   txt1 <- toupper(txt) 
#   txt2 <- stringr::str_remove_all(txt1, "\n")
#   txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
#   # str_remove(txt3, "ROLL CALL(?=PRESENT)")
#   txt4 <- str_extract(txt3, "SUPERVISOR.+ WERE PRESENT|WAS PRESENT")
#   txt5 <- str_remove(txt4, "SUPERVISOR |SUPERVISORS ")
#   txt6 <- str_remove(txt5, "WERE PRESENT|WAS PRESENT")
#   txt7 <- str_replace(txt6, " AND", ",")
#   txt8 <- na.omit(txt7)
#   txt9 <- str_remove_all(txt8, " ")
#   txt10 <- str_split(txt9, ",")
#   return(txt10)
# }

extract_present2 <- function(txt){
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
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- stringr::str_extract(txt3, ".*QUORUM")
  # txt5 <- str_extract(txt4, "SUPERVISORS.? WERE ABSENT")
  txt5 <- substr(txt4, 
                 str_locate_all(txt4, "SUPERVISOR")[[1]][2,][[1]],
                 str_locate(txt4, "WERE ABSENT")[1,][[2]])
  txt6 <- str_remove(txt5, "^SUPERVISOR?.")
  txt7 <- str_remove(txt6, "WERE ABSENT")
  txt8 <- na.exclude(txt7)
  txt9 <- str_replace(txt8, " AND", ",")
  txt10 <- str_remove_all(txt9, " ")
  txt10 <- str_split(txt10, ",")
  return(txt10)
}

extract_absent2 <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  # str_remove(txt3, "ROLL CALL(?=PRESENT)")
  txt4 <- str_extract(txt3, "SUPERVISOR.+ WERE ABSENT|WAS ABSENT")
  txt5 <- str_remove(txt4, "SUPERVISOR |SUPERVISORS ")
  txt6 <- str_extract(txt5[1], "SUPERVISOR.+(?=WERE ABSENT|WAS ABSENT)")
  txt7 <- str_replace(txt6, " AND", ",")
  txt8 <- na.omit(txt7)
  txt9 <- str_remove(txt8, "SUPERVISOR |SUPERVISORS ")
  txt10 <- str_remove_all(txt9, " ")
  txt11 <- str_split(txt10, ",")
  return(txt11[[1]])
}


extract_vacant <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- stringr::str_extract(txt3, ".*QUORUM")
  txt5 <- str_extract(txt4, "VACANT")
  txt5 <- str_extract(txt4, "WERE ABSENT.*VACANT\\.")
  txt6 <- str_remove(txt5, "WERE ABSENT.")
  txt7 <- str_extract(txt6, "DISTRICT.*\\d")
  txt8 <- na.exclude(txt7)
  txt9 <- str_replace(txt8, " AND", ",")
  txt10 <- str_remove_all(txt9, " ")
  txt10 <- str_split(txt10, ",")
  return(txt10)
}

# Check Files 1, 2, 4, 12, 13
# Should return District 1, NA, NA, District 12, and District #12 respectively 
extract_vacant2 <- function(txt){
  txt1 <- toupper(txt)
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- stringr::str_extract(txt3, ".*QUORUM")
  txt5 <- stringr::str_extract(txt4, "DISTRICT.+\\d")
  # txt6 <- str_extract(txt5, "DISTRICT.+\\d{2}")
  txt6 <- na.exclude(txt5)
  txt8 <- str_replace(txt6, " AND", ",")
  # txt9 <- str_remove_all(txt8, " ")
  txt10 <- str_split(txt8, ",")
  return(txt10)
}


# Logical Detection in Roll Call
detect_vacant <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- stringr::str_extract(txt3, ".*QUORUM")
  detected <- na.omit(str_detect(txt4, "VACANT"))
  return(detected[[1]])
}

detect_absent <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- stringr::str_extract(txt3, ".*QUORUM")
  detected <- na.omit(str_detect(txt4, "ABSENT"))
  return(detected[[1]])
}

detect_absent2 <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- str_extract(txt3, "SUPERVISOR.+ WERE ABSENT|WAS ABSENT")
  txt5 <- str_remove(txt4, "SUPERVISOR |SUPERVISORS ")
  txt6 <- str_extract(txt5[1], "SUPERVISOR.+(?=WERE ABSENT|WAS ABSENT)")
  txt7 <- str_replace(txt6, " AND", ",")
  txt8 <- na.omit(txt7)
  txt9 <- str_remove(txt8, "SUPERVISOR |SUPERVISORS ")
  txt10 <- str_remove_all(txt9, " ")
  txt11 <- str_split(txt10, ",")
  return(txt11[[1]])
}

detect_present <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- stringr::str_extract(txt3, ".*QUORUM")
  detected <- na.omit(str_detect(txt4, "PRESENT"))
  return(detected[[1]])
}


detect_present2 <- function(txt){
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



