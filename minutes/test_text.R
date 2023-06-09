
# Install and load the 'pdftools' package
# install.packages("pdftools")
library(pdftools)

# Specify the path to your PDF file
pdf_file <- "supervisors_minutes_010820.pdf"

# Extract text from the PDF
text <- pdf_text(pdf_file)

# Print the extracted text
# cat(text)
text <- toupper(text)
# Files with text
files <- list.files(path ="pdfs", "\\.pdf")
