

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Define the Bang Wong color palette
bang_wong_palette = {
    'blue': '#56B4E9',
    'orange': '#E69F00',
    'green': '#009E73',
    'yellow': '#F0E442',
    'red': '#D55E00',
    'purple': '#CC79A7',
    'brown': '#A52A2A',
    'pink': '#FF69B4',
    'gray': '#999999',
    'black': '#000000'
}
# misc
cm_to_inch = 1/2.54

# Define the file path
file_path = "/Users/m.wehrens/Documents/git_repos/MW_playing/macbookpricescomparison_20240920.xlsx"

# Read the Excel file
df = pd.read_excel(file_path)

# Display the first few rows of the dataframe
    # print(df.head())
    # df.keys()

# Normalize the 'processors' column to a range 0-1
df['processors_norm'] = (df['processors'] - df['processors'].min()) / (df['processors'].max() - df['processors'].min())
df['gpus_norm'] = (df['gpus'] - df['gpus'].min()) / (df['gpus'].max() - df['gpus'].min())
df['ram_norm'] = (df['ram'] - df['ram'].min()) / (df['ram'].max() - df['ram'].min())

###

# Plot the number of processors against "prijs"
fig, (ax,ax2) = plt.subplots(1,2, figsize=(20*cm_to_inch,10*cm_to_inch))
_=ax.scatter(df['processors_norm'], df['prijs'], color=bang_wong_palette['blue'], alpha=1, label='processors', s=15**2)
_=ax.scatter(df['gpus_norm'], df['prijs'], color=bang_wong_palette['orange'], alpha=1, label='gpus', s=12**2)
_=ax.scatter(df['ram_norm'], df['prijs'], color=bang_wong_palette['green'], alpha=1, label='ram', s=9**2)

# Draw lines between the datapoints that come from the same row
for i in range(len(df)):
    x = [df['processors_norm'].iloc[i], df['gpus_norm'].iloc[i], df['ram_norm'].iloc[i]]
    y = [df['prijs'].iloc[i]] * 3
    ax.plot(x, y, color='k', alpha=1)

# Add text labels from df['model'] to the right of the three points
for i in range(len(df)):
    model_description = f"Procs: {df['processors'].iloc[i]}, GPUs: {df['gpus'].iloc[i]}, RAM: {df['ram'].iloc[i]} -- Model: {df['model'].iloc[i]} "  # xloc = np.max([df['processors_norm'].iloc[i], df['gpus_norm'].iloc[i], df['ram_norm'].iloc[i]]) + 0.01
    _=ax2.text(0, df['prijs'].iloc[i], model_description, fontsize=9, color='k', ha='left', va='center')

ax2.axis('off')
_=ax.set_ylim([2500,4200])
_=ax2.set_ylim([2500,4200])
_=ax.set_xlabel('Normalized specs')
_=ax.set_ylabel('Price')
_=ax.grid(True)
_=ax.legend()

# save the figure as pdf to the desktop
#plt.show()
fig.savefig('/Users/m.wehrens/Desktop/macbook_prices.pdf', bbox_inches='tight')

plt.close('all')

