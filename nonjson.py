import json # built in 

def read_json_file(json_path):
    with open(json_path, 'r') as json_file:
        json_data = json.load(json_file)
        return json_data

# Provide the path to the JSON file
json_file_path = "C:/Users/Zachary.Palmore/GitHub/policy/2010/Pete/output_supervisors_minutes_010820.json"
json_data = read_json_file(json_file_path)
# print(json_data)

