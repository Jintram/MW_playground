
# Some plots regarding the SILS tech day survey

import seaborn as sns
import pandas as pd
import numpy as np

from matplotlib import pyplot as plt

import os

cm_to_inch = 1/2.54

# Load the data, take sheet 1
df_survey = pd.read_excel('/Users/m.wehrens/Documents/PROJECTS/_BIO-DSC/SILS_technology_day_INPUT/bioDSC-SILS-techday-survey-responses-2024-11-20.xlsx', sheet_name=0)
OUTPUTDIR = '/Users/m.wehrens/Documents/PROJECTS/_BIO-DSC/SILS_technology_day_INPUT/pyplots/'
os.makedirs(OUTPUTDIR, exist_ok=True)

# look at the keys
df_survey.keys()

################################################################################
# Can be ignored

# loop over the data in column 1, and split the comma-separated values in each entry to a list and merge all data into one big list
data_col1 = df_survey.iloc[:,1].values
data_col1_list = [str(x).split(',') for x in data_col1]
# remove trailing or leading spaces from the strings
data_col1_list = [[x.strip() for x in sublist] for sublist in data_col1_list]
data_col1_list_flat = [item for sublist in data_col1_list for item in sublist]
# remove nan values
data_col1_list_flat = [x for x in data_col1_list_flat if x != 'nan']

# set font size to 8
plt.rcParams.update({'font.size': 8})

# Now create a histogram of the data
# create a data frame from this data again
df_col1 = pd.DataFrame({'Technologies_used':data_col1_list_flat})
# now sort the data frame by the number of occurrences of each technology
df_col1_hist = df_col1['Technologies_used'].value_counts()

# now make a histogram
fig, ax = plt.subplots(1,1, figsize=(5*cm_to_inch,5*cm_to_inch))
#sns.histplot(df_col1, y='Technologies_used', discrete=True, ax=ax)
# make a bar plot
sns.barplot(x=df_col1_hist.values, y=df_col1_hist.index, ax=ax)
ax.set_xlabel('Number of occurrences')
ax.set_ylabel('Which tech do you use?')
plt.tight_layout()
plt.savefig(OUTPUTDIR+'plt_Question1.pdf', dpi=300, bbox_inches='tight')
plt.close(fig)

################################################################################


# Now do the same for all relevant columns
ylabels = ['Data type generated',
           'Tools currently used', 
           'Support type appreciated most',
           'How to approach us?']
figheights = [5, 20, 7, 5]
for idx in range(1, 5):
    plt.rcParams.update({'font.size': 8})
    
    # idx=2
    data_col = df_survey.iloc[:,idx].values
    data_col_list = [str(x).split(',') for x in data_col]
    data_col_list = [[x.strip() for x in sublist] for sublist in data_col_list]
    data_col_list_flat = [item for sublist in data_col_list for item in sublist]
    data_col_list_flat = [x for x in data_col_list_flat if (x != 'nan' and x != '' and x != '...')]
    # convert all to lower case
    data_col_list_flat = [x.lower() for x in data_col_list_flat]
    
    # create a data frame from this data again
    df_col = pd.DataFrame({'Technologies_used':data_col_list_flat})
    # now sort the data frame by the number of occurrences of each technology
    df_col_hist = df_col['Technologies_used'].value_counts()
    
    # now make a histogram
    fig, ax = plt.subplots(1,1, figsize=(5*cm_to_inch,figheights[idx-1]*cm_to_inch))
    #sns.histplot(df_col, y='Technologies_used', discrete=True, ax=ax)
    # make a bar plot
    _=sns.barplot(x=df_col_hist.values, y=df_col_hist.index, ax=ax)
    _=ax.set_xlabel('Number of occurrences')
    _=ax.set_ylabel(ylabels[idx-1])
    plt.tight_layout()
    plt.savefig(OUTPUTDIR+f'plt_Question{idx}.pdf', dpi=300, bbox_inches='tight')
    plt.close(fig)