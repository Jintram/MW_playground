

from bs4 import BeautifulSoup
import json
import pandas as pd 
import csv 


page1_path = '/Users/m.wehrens/Desktop/webextracttest/Een Toyota occasion_ All-in prijzen met de beste garanties..html'
output_path = '/Users/m.wehrens/Desktop/webextracttest/page1.csv'

# Load the contents of the HTML file
with open(page1_path, 'r', encoding='utf-8') as file:
    html_content = file.read()

# Initialize BeautifulSoup to parse the HTML content
soup = BeautifulSoup(html_content, 'html.parser')


# Step 3: Find the script tag containing JSON (adjust selector accordingly)
# script_tag = soup.find('script', type='application/json')
script_tag = soup.find('script', id='__NEXT_DATA__') # this is the script tag I found by inspecting the page

# Step 4: Extract and load the JSON content
if script_tag:
    json_data = json.loads(script_tag.string)

# json data is nested, here are top level keys:
# json_data.keys()

for akey in list(json_data.keys()):
    print('_______')
    print('key:', akey)    
    try:
        
        print(json_data[akey].keys())
    except:
        print('fail')
       
json_data['props']['pageProps'].keys()
json_data['props']['pageProps']['initialState'].keys()
        
# Now you can work with json_data
print(json_data)


#########

def print_keys(d, indent=0):
    for key, value in d.items():
        print('  ' * indent + str(key))
        if isinstance(value, dict):
            print_keys(value, indent + 1)
        elif isinstance(value, list):
            for item in value:
                if isinstance(item, dict):
                    print_keys(item, indent + 1)

# Print the hierarchy of keys
print_keys(json_data)


#########

fields_to_extract = ["mileage", "year", "type", "price"]

# return all parent keys
def find_parent_keys(d, target_key, parent_keys=None):
    if parent_keys is None:
        parent_keys = []
    for key, value in d.items():
        if key == target_key:
            return parent_keys + [key]
        elif isinstance(value, dict):
            result = find_parent_keys(value, target_key, parent_keys + [key])
            if result:
                return result
        elif isinstance(value, list):
            for item in value:
                if isinstance(item, dict):
                    result = find_parent_keys(item, target_key, parent_keys + [key])
                    if result:
                        return result
    return None

find_parent_keys(json_data, 'mileage')
find_parent_keys(json_data, 'year')
find_parent_keys(json_data, 'type')
find_parent_keys(json_data, 'price')

json_data_sub = json_data['props']['pageProps']['initialState']['results']['results']

# now subselect the fields_to_extract keys

json_data_sub = json_data['props']['pageProps']['initialState']['results']['results']

entries_of_interest = ['type','price', 'year', 'mileage']
df2 = pd.DataFrame(columns=['type','price', 'year', 'mileage'])

for idx, entry in enumerate(json_data['props']['pageProps']['initialState']['results']['results']):
    #df2.loc[idx, 'type'] = entry['type']
    for key in entries_of_interest:
        df2.loc[idx, key] = entry[key]
        
# SUCCES!!!!

# now export the df to an excel file
df2.to_excel(output_path, index=False)

df2.to_csv(output_path)





####################################################################

#########


# Function to extract specific fields
def extract_fields(data, fields):
    extracted_data = []
    
    def extract(d, parent_key=''):
        if isinstance(d, dict):
            for key, value in d.items():
                full_key = f"{parent_key}.{key}" if parent_key else key
                if key in fields:
                    extracted_data.append((full_key, value))
                if isinstance(value, dict):
                    extract(value, full_key)
                elif isinstance(value, list):
                    for item in value:
                        if isinstance(item, dict):
                            extract(item, full_key)
                            
    extract(data)
    return extracted_data

json_data['props']['initialState']['results']['results']
json_data['props']['initialState']['results']['results']

# Fields to extract
fields_to_extract = ["mileage", "year", "type", "price"]

# Extract the fields
extracted_data = extract_fields(json_data, fields_to_extract)

# Convert to a list of dictionaries
data_to_write = [{field: value for field, value in extracted_data}]

df = pd.DataFrame([data_to_write])

# Write to CSV
csv_file = output_path
with open(csv_file, mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=fields_to_extract)
    writer.writeheader()
    for row in data_to_write:
        writer.writerow(row)

print(f"Data has been written to {csv_file}")



########



key1='address'
json_data[key1]

df = pd.DataFrame(json_data)






