
# Packages
import PyPDF2 # Install with `pip install PyPDF2`
import json # Built in for Python

# Function to Extract Text Info from PDF
def extract_text_from_pdf(pdf_path):
    with open(pdf_path, 'rb') as file:
        pdf_reader = PyPDF2.PdfReader(file)
        num_pages = len(pdf_reader.pages)
        extracted_text = ""
        for page_number in range(num_pages):
            page = pdf_reader.pages[page_number]
            extracted_text += page.extract_text()
        return extracted_text

# Function to use Extract Function then Convert to JSON
def convert_to_json(pdf_path, json_path):
    extracted_text = extract_text_from_pdf(pdf_path)
    json_data = {
        "file_name": pdf_path,
        "text": extracted_text
    }
    with open(json_path, 'w') as json_file:
        json.dump(json_data, json_file, indent=4)

# Provide the path to the PDF file and the desired path for the JSON output
pdf_file_path = 'C:/Users/Zachary.Palmore/GitHub/policy/2010/Pete/supervisors_minutes_010820.pdf'
json_output_path = 'C:/Users/Zachary.Palmore/GitHub/policy/2010/Pete/output_supervisors_minutes_010820.json'

# Run It
convert_to_json(pdf_file_path, json_output_path)



