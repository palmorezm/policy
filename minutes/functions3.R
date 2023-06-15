
# Third iteration of functions (started with 2011)

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

extract_vacant <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- stringr::str_remove_all(txt1, "\n")
  txt3 <- stringr::str_extract(txt2, "ROLL CALL?.*")
  txt4 <- str_extract(txt3, "SUPERVISOR.+ VACANT")
  txt5 <- str_extract(txt4[[1]], "DISTRICT.+?VACANT")
  return(txt5)
}

extract_date <- function(txt){
  txt1 <- toupper(txt) 
  txt2 <- str_extract(txt1, "JANESVILLE, WISCONSIN\n.+.?+, \\d{4}\n")
  txt3 <- str_remove(txt2, "JANESVILLE, WISCONSIN\n")
  txt4 <- str_remove_all(txt3, "\n")
  txt5 <- na.omit(str_squish(txt4))
  return(txt5[1])
}


