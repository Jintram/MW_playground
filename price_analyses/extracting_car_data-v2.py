
OUTPUTDIR='/Users/m.wehrens/Documents/Personal/auto/'

########################################################################
# Libs

from bs4 import BeautifulSoup
import json
import pandas as pd 
import csv 

import matplotlib.pyplot as plt
import seaborn as sn

cm_to_inch = 1/2.54

color_palette = [
    "#E69F00",  # Orange
    "#56B4E9",  # Sky Blue
    "#009E73",  # Bluish Green
    "#F0E442",  # Yellow
    "#0072B2",  # Blue
    "#D55E00",  # Vermillion
    "#CC79A7",  # Reddish Purple
    "#000000"   # Black
] 

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

pagenumbers = [1,2,7]
list_path_html = ['/Users/m.wehrens/Desktop/test2/page'+str(i)+'___Toyota occasion kopen_ Tweedehands auto\'s met garantie..html' for i in pagenumbers]

# Output path
output_path = '/Users/m.wehrens/Documents/Personal/auto/'

# Function that extracts the data from a single page
def get_df_w_info(path_html):

    # path_html = list_path_html[0]
    # path_html = list_path_html[1]
    print(path_html)

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

# A function that goes over all the keys in the json structure,
# into all of the nested keys, and returns the key hierarchy for 
# each of the keys which match the target key.

def find_key_hierarchy(data, target_key):
    def search_keys(data, target_key, current_path):
        if isinstance(data, dict):
            for key, value in data.items():
                new_path = current_path + [key]
                if key == target_key:
                    key_hierarchies.append(new_path)
                search_keys(value, target_key, new_path)
        elif isinstance(data, list):
            for index, item in enumerate(data):
                new_path = current_path + [index]
                search_keys(item, target_key, new_path)

    key_hierarchies = []
    search_keys(data, target_key, [])
    return key_hierarchies

entries_of_interest = ['type','price', 'year', 'mileage']

find_key_hierarchy(json_data, 'mileage')

# And it only holds information about the first page, which is weird
# the actual information is printed into divs..

######################################################################

# cross-ref with df2x

pagenumbers = [1,2,7]
list_path_html = ['/Users/m.wehrens/Desktop/test2/page'+str(i)+'___Toyota occasion kopen_ Tweedehands auto\'s met garantie..html' for i in pagenumbers]
test_path = list_path_html[2]

pagenumbers = list(range(1,16))+list(range(17,24)) # entry 16 was missing ..
list_path_html = ['/Users/m.wehrens/Documents/Personal/auto/corolla_data/page'+str(i)+'__Toyota occasion kopen_ Tweedehands auto\'s met garantie..html' for i in pagenumbers]

for my_path in list_path_html:

    # Load the file, and replace all div or spans with a newline+div or span
    with open(my_path, 'r', encoding='utf-8') as file:
        html_content = file.read()

    # this will take a while    
    html_content = html_content.replace('<div', '\n<div').replace('<span', '\n<span')

    # Save the new content to a new file
    with open(my_path.replace('.html', '_processed.html'), 'w', encoding='utf-8') as file:
        file.write(html_content)
    

# how things look

# mileage:
# <span class="mileage">36.531 km</span></div>

# year:
# <span class="icon icon-car"></span> 2023</div>

# type:
# <div class="sc-244ca38c-1 hNXGYI"><button type="button" class="btn-icon btn-icon--heart--dark btn-icon--heart btn btn-primary"></button><h5 class="sc-d765e439-0 jTBUSR">Toyota <strong>Corolla</strong></h5><p class="sc-85334b18-0 cKgWde">Touring Sports Hybrid 140 Active</p>

# price:
# <span class="sc-95c6ccfa-1 ZYBjx">All-in prijs</span>
# <span class="sc-95c6ccfa-0 iZzIUO">€&nbsp;28.845,-</span></div>
    
# Now extract the mileage, year, type, and price from the lines
# Also looping over the different pages    
mileages = []
years = []
types = []
prices = []    
pages = []
for pidx, my_path in enumerate(list_path_html):    
    
    # Now read the new file line by line, and check whether there's a match with "<span class="mileage">"
    with open(my_path.replace('.html', '_processed.html'), 'r', encoding='utf-8') as file:
        lines = file.readlines()        

    for idx, line in enumerate(lines):
        # check if <span class="mileage"> is in the line
        if '<span class="mileage">' in line:
            infoline=line
            # extract the mileage
            mileage = infoline.split('>')[1].split('<')[0].replace(' km', '').replace('.', '')
            # print(mileage)
            mileages.append(mileage)
            # also add a page number
            pages.append(pagenumbers[pidx])
        # now do the same for year, type, and price
        if '<span class="icon icon-car">' in line:
            infoline=line
            year = infoline.split('>')[2].split('<')[0].replace(' ', '')        
            print(year)
            years.append(year)
        # type
        if 'sc-85334b18-0' in line:
            infolinetype=line
            type = infolinetype.split('<p')[1].split('>')[1].split('<')[0]
            print(type)
            types.append(type)
        # price
        if '<span class="sc-95c6ccfa-1 ZYBjx">All-in prijs</span>' in line:
            infolineprice=lines[idx+1]
            price = infolineprice.split('€&nbsp;')[1].split('<')[0].replace('.', '').replace(',-', '')
            print(price)
            prices.append(price)
    
df3 = pd.DataFrame({'mileage': mileages, 'year': years, 'type': types, 'price': prices})

# convert mileage, price and year to integers
df3['Kilometerstand'] = df3['mileage'].astype(int)
df3['Prijs'] = df3['price'].astype(int)
df3['Bouwjaar'] = df3['year'].astype(int)

# save to csv
df3.to_csv(output_path+'corollaprices_webdata_2024_v2.csv', index=False)

################################################################################

# load from csv
df3 = pd.read_csv(OUTPUTDIR+'corollaprices_webdata_2024_v2.csv')

# now plot again like before
plt.rcParams.update({'font.size': 6})
fig, ax = plt.subplots(1,1, figsize=(13*cm_to_inch,7*cm_to_inch))
sn.scatterplot(data=df3, x='Kilometerstand', y='Prijs', hue='Bouwjaar', palette=color_palette, edgecolor='black', ax=ax)
ax.set_xlabel('Kilometerstand (km)')
ax.set_ylabel('Prijs (€)')
plt.tight_layout()
# plt.show()
fig.savefig(OUTPUTDIR+'corollaprices_webdata_2024_v2.pdf', dpi=300, bbox_inches='tight')
plt.close(fig)

# and plot with x=year and hue=mileage
plt.figure(figsize=(10,6))
sn.scatterplot(data=df3, x='Bouwjaar', y='Prijs', hue='Kilometerstand')
plt.xlabel('Bouwjaar')
plt.ylabel('Prijs')
plt.show()

# check correlation year-mileage by plotting that scatter
plt.figure(figsize=(10,6))
sn.scatterplot(data=df3, x='year', y='mileage')
plt.xlabel('Bouwjaar')
plt.ylabel('Kilometerstand')
plt.show()

plt.close('all')

###########################################################################################

# now fit a line through carrs with more than 15 000 km mileage
df3_sel = df3[df3['Kilometerstand']>15000]

# now fit a line
from scipy import stats
import numpy as np
slope, intercept, r_value, p_value, std_err = stats.linregress(df3_sel['Kilometerstand'], df3_sel['Prijs'])
# estimate
x_int = [np.min(df3_sel['Kilometerstand']), np.max(df3_sel['Kilometerstand'])]
x_float = np.array([float(x) for x in x_int])
y = slope * x_float + intercept

# plot the data and the line
plt.rcParams.update({'font.size': 6})
fig, ax = plt.subplots(1,1, figsize=(13*cm_to_inch,7*cm_to_inch))
_ = sn.scatterplot(data=df3, x='Kilometerstand', y='Prijs', hue='Bouwjaar', palette=color_palette, edgecolor='black', ax=ax)
#_ = sn.scatterplot(data=df3_sel, x='Kilometerstand', y='Prijs', color='black', ax=ax, s=10)
# now add the line
_ = ax.plot(x_float, y, color='black', linestyle='-', linewidth=2)
_ = ax.set_xlabel('Kilometerstand (km)')
_ = ax.set_ylabel('Prijs (€)')
plt.tight_layout()
# plt.show()
fig.savefig(OUTPUTDIR+'corollaprices_webdata_2024_v2-withline.pdf', dpi=300, bbox_inches='tight')
plt.close(fig)

# now plot the line again, but normalized for y-intersect = 100%
x_float2 = np.array([0, np.max(df3_sel['Kilometerstand'])], dtype=float)
y2 = slope * x_float2 + intercept
y2prime = 100*(slope/intercept * x_float2 + 1)
delta=100*(slope/intercept)*10000
fig, ax = plt.subplots(1,1, figsize=(7*cm_to_inch,7*cm_to_inch))
_ = ax.set_title(f'Prijsverloop tweedehands Toyota Corolla\nWaardeverlies per 10.000 km: {delta:.2f}% (linear regime)')
_ = ax.plot(x_float2, y2prime, color='black', linestyle='-', linewidth=2)
_ = ax.set_xlabel('Kilometerstand (km)')
_ = ax.set_ylabel('Prijs (%) (genormaliseerd)')
# set y limits to 0-100
_ = ax.set_ylim([0,105])
# set x limits to 0-max km
_ = ax.set_xlim([0, np.max(df3_sel['Kilometerstand'])])
# add minor ticks for x and y, every 10 for y, and every 10000 for x
_ = ax.set_xticks(np.arange(0, np.max(df3_sel['Kilometerstand']), 10000), minor=True)
_ = ax.set_yticks(np.arange(0, 105, 10), minor=True)
_ = ax.grid(which='both', axis='both', linestyle='--')
plt.tight_layout()
# plt.show()
fig.savefig(OUTPUTDIR+'corollaprices_webdata_2024_v3-percentueel.pdf', dpi=300, bbox_inches='tight')


# now also fit an exponential the same way
from scipy.optimize import curve_fit

def func(x, a, b):
    return a * b**(x/10000) #np.exp(- b * x)

estimate_a = df3['Prijs'].max()
estimate_b = .99
popt, pcov = curve_fit(func, df3_sel['Kilometerstand'], df3_sel['Prijs'], p0=[estimate_a, estimate_b])

# estimate
x_float3 = np.linspace(0, float(np.max(df3_sel['Kilometerstand'])),100)
y3 = func(x_float3, *popt)
# the original guess
# y3 = func(x_float3, estimate_a, estimate_b)
deprication_per10000 = popt[1]
# and the normalized estimate
y3norm = func(x_float3, 100, deprication_per10000)

# plot on the data
plt.rcParams.update({'font.size': 6})
fig, ax = plt.subplots(1,1, figsize=(13*cm_to_inch,7*cm_to_inch))
_ = sn.scatterplot(data=df3, x='Kilometerstand', y='Prijs', hue='Bouwjaar', palette=color_palette, edgecolor='black', ax=ax)
#_ = sn.scatterplot(data=df3_sel, x='Kilometerstand', y='Prijs', color='black', ax=ax, s=10)
# now add the line
_ = ax.plot(x_float3, y3, color='black', linestyle='-', linewidth=2)
_ = ax.set_xlabel('Kilometerstand (km)')
_ = ax.set_ylabel('Prijs (€)')
plt.tight_layout()
# plt.show()
fig.savefig(OUTPUTDIR+'corollaprices_webdata_2024_v2-withlineEXP.pdf', dpi=300, bbox_inches='tight')
plt.close(fig)

# now repeat the line plot, but show both linear and exponential fit
fig, ax = plt.subplots(1,1, figsize=(7*cm_to_inch,7*cm_to_inch))
perc_loss_log = -(1-deprication_per10000)*100
_ = ax.set_title(f'Prijsverloop tweedehands Toyota Corolla\nWaardeverlies per 10.000 km:'+\
                 f'\n{delta:.2f}% (linear regime)'+f'\n{perc_loss_log:.2f}% (exp regime)')
_ = ax.plot(x_float2, y2prime, color='blue', linestyle='--', linewidth=2, label='Lineair')
_ = ax.plot(x_float3, y3norm, color='black', linestyle='-', linewidth=2, label='Exponentieel')
_ = ax.set_xlabel('Kilometerstand (km)')
_ = ax.set_ylabel('Prijs (%) (genormaliseerd)')
# set y limits to 0-100
_ = ax.set_ylim([0,105])
# set x limits to 0-max km
_ = ax.set_xlim([0, np.max(df3_sel['Kilometerstand'])])
# add minor ticks for x and y, every 10 for y, and every 10000 for x
_ = ax.set_xticks(np.arange(0, np.max(df3_sel['Kilometerstand']), 10000), minor=True)
_ = ax.set_yticks(np.arange(0, 105, 10), minor=True)
_ = ax.grid(which='both', axis='both', linestyle='--')
_ = ax.legend()
plt.tight_layout()
plt.show()
fig.savefig(OUTPUTDIR+'corollaprices_webdata_2024_v3-percentueel2.pdf', dpi=300, bbox_inches='tight')
