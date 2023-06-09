
# Packages
import PyPDF2

def extract_text_from_pdf(file_path):
    text = ""
    with open(file_path, 'rb') as file:
        reader = PyPDF2.PdfReader(file)
        num_pages = len(reader.pages)
        for page_num in range(num_pages):
            page = reader.pages[page_num]
            text += page.extract_text()

    return text

# Provide the path to your PDF file
pdf_file_path = 'C:/Users/Zachary.Palmore/GitHub/policy/2010/Pete/supervisors_minutes_010820.pdf'
parsed_text = extract_text_from_pdf(pdf_file_path)
print(parsed_text)

