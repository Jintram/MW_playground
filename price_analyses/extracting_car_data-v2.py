

########################################################################
# Libs

from bs4 import BeautifulSoup
import json
import pandas as pd 
import csv 

import matplotlib.pyplot as plt
import seaborn as sn

########################################################################
# Functions

# To print the whole key hierarchy
def print_keys(d, indent=0):
    for key, value in d.items():
        print('  ' * indent + str(key))
        if isinstance(value, dict):
            print_keys(value, indent + 1)
        elif isinstance(value, list):
            for item in value:
                if isinstance(item, dict):
                    print_keys(item, indent + 1)

# For a specific nested key, find the parent key hierarchy
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

########################################################################

# These paths are for the first test to determine where the data was located
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

# Print the hierarchy of keys
print_keys(json_data)


#########

# From inspecting the page, these are my fields of interest
fields_to_extract = ["mileage", "year", "type", "price"]

# Find out where my fields of interest are located in the json data
find_parent_keys(json_data, 'mileage')
find_parent_keys(json_data, 'year')
find_parent_keys(json_data, 'type')
find_parent_keys(json_data, 'price')

# Check whether I can retrieve the data
json_data_sub = json_data['props']['pageProps']['initialState']['results']['results']

# Now create a dataframe with the data
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
####################################################################
# Now let's make a version that collects this data from multiple
# pages.

# Input data
pagenumbers = list(range(1,16))+list(range(17,24)) # entry 16 was missing ..
list_path_html = ['/Users/m.wehrens/Documents/Personal/auto/corolla_data/page'+str(i)+'__Toyota occasion kopen_ Tweedehands auto\'s met garantie..html' for i in pagenumbers]

# Output path
output_path = '/Users/m.wehrens/Documents/Personal/auto/'

# Function that extracts the data from a single page
def get_df_w_info(path_html):

    # path_html = list_path_html[0]
    # path_html = list_path_html[1]

    # Load the contents of the HTML file
    with open(path_html, 'r', encoding='utf-8') as file:
        html_content = file.read()

    # Initialize BeautifulSoup to parse the HTML content
    soup = BeautifulSoup(html_content, 'html.parser')

    # Step 3: Find the script tag containing JSON (adjust selector accordingly)
    # script_tag = soup.find('script', type='application/json')
    script_tag = soup.find('script', id='__NEXT_DATA__') # this is the script tag I found by inspecting the page

    # Step 4: Extract and load the JSON content
    json_data = json.loads(script_tag.string)

    # Create df
    entries_of_interest = ['type','price', 'year', 'mileage']
    df2 = pd.DataFrame(columns=['type','price', 'year', 'mileage'])

    # Fill df
    for idx, entry in enumerate(json_data['props']['pageProps']['initialState']['results']['results']):
        #df2.loc[idx, 'type'] = entry['type']
        for key in entries_of_interest:
            df2.loc[idx, key] = entry[key]
    
    return df2

list_df_w_data = \
    [get_df_w_info(path_html) for path_html in list_path_html]

# now merge all the dfs into one
df_all = pd.concat(list_df_w_data)

# Also save this df to csv
df_all.to_csv(output_path+'corollaprices_webdata_2024.csv', index=False)

######################################################################

# Now make a scatter plot


# Make the plot
plt.figure(figsize=(10,6))
sn.scatterplot(data=df_all, x='mileage', y='price', hue='year')
plt.xlabel('mileage')
plt.ylabel('price')
plt.show()


#x=df_all['price'][0].values
#x.dtype

######################################################################
# Turns out I had the structure wrong, JSON data on all pages 
# is the same ..


