

from bs4 import BeautifulSoup
import json
import pandas as pd 

page1_path = '/Users/m.wehrens/Desktop/webextracttest/Een Toyota occasion_ All-in prijzen met de beste garanties..html'
output_path = '/Users/m.wehrens/Desktop/webextracttest/page1.csv'

# Load the contents of the HTML file
with open(page1_path, 'r', encoding='utf-8') as file:
    html_content = file.read()

# Initialize BeautifulSoup to parse the HTML content
soup = BeautifulSoup(html_content, 'html.parser')


# Step 3: Find the script tag containing JSON (adjust selector accordingly)
script_tag = soup.find('script', type='application/json')
script_tag = soup.find('script', id='__NEXT_DATA__')

__NEXT_DATA__

# Step 4: Extract and load the JSON content
if script_tag:
    json_data = json.loads(script_tag.string)

# Now you can work with json_data
print(json_data)

json_data.keys()
json_data.

df = pd.DataFrame(json_data)







# Extract necessary fields from the document
cars = []
car_listings = soup.find_all('div', class_='car-listing')  # Adjust this to match the actual HTML structure

for car in car_listings:
    model = car.find('h2', class_='car-model').text.strip()  # Extract the car model
    build_year = car.find('span', class_='build-year').text.strip()  # Extract the build year
    price = car.find('span', class_='car-price').text.strip()  # Extract the price
    kilometers = car.find('span', class_='car-kilometers').text.strip()  # Extract kilometers
    
    cars.append([model, build_year, price, kilometers])

# Save to CSV
import csv
with open(output_path, mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(['Model', 'Build Year', 'Price', 'Number of Kilometers'])
    writer.writerows(cars)