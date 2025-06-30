


import pandas as pd
import numpy as np

df_data = \
    pd.read_excel('/Users/m.wehrens/Documents/TEACHING/administration-stats/list_participants_all_workshops.xlsx')

df_data.columns

unique_participants = np.unique(df_data['Email'])

len(unique_participants)

# only select items that end with @uva.nl 
sel_employee = np.array([X.endswith('@uva.nl') for X in unique_participants])
unique_participants_employee = unique_participants[sel_employee]
unique_participants_other    = unique_participants[~sel_employee]

len(unique_participants_employee)
len(unique_participants_other)


