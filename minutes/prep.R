
# prep

# Extract the data from the pdfs
source("do_the_loop.R")

# Assign a "No Vacancy" to records/dates/minutes where all districts were filled 
df[which(is.na(df$supervisor) & df$status == "vacant"),][1] <- "NO VACANCY"

# Remove the empty supervisor records (they were collected by mistake / extraction method)
df <- df[which(df$supervisor != ""),]

# Create a date object from the string of dates
df$date2 <- as.Date(df$date, "%B %d, %Y")

# Change supervisor names to factors for computation
df$supervisor <- factor(df$supervisor)
