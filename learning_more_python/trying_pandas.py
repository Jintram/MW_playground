#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jan  8 11:09:45 2020

Walking trough the manual at 
https://pandas.pydata.org/pandas-docs/stable/getting_started/10min.html


@author: m.wehrens
"""

############################################################
# libs

import pandas as pd
import numpy as np

############################################################
# Pandas function to create a dataframe

my_df1 = pd.DataFrame(np.random.randn(5,5), index=range(5),columns=range(11,15+1))
my_df1

############################################################
# Using a dict to create a dataframe

# a dict in python works like this:
my_dict = {'A': [a for a in range(-2,2+1)],
                 'B': [44,33,22,11,00],
                 'C': ['234','adkljf','asd','adsg','gfs']}

my_df2 = pd.DataFrame(my_dict)
my_df2

# obtain types
my_df2.dtypes

# names of rows/columns
my_df2.index
my_df2.columns

# convert
my_df2.to_numpy()

# more stuff
my_df2.describe()
my_df2.T # transpose
my_df2.sort_values(by='B')

# selection of data by names of rows/columns
my_df2.loc[:, ['A', 'B']]
my_df2.loc[0:2, ['C']]
# selection of data by indices
my_df2.iloc[1:3,1:3]

############################################################
# More selection options

# Simple boolean selection
my_df2[my_df2.A > 0] # note if only 1 index is given rows are selected

# Can also apply to whole df
my_df1[my_df1 > 0] # note that False will return NA (roughly put)

# Selection by membership vector (list)
my_df2[my_df2['A'].isin([-1,-2])]


############################################################
# Other manipulation

# Adding a new column
my_df2['New'] = range(5)

# Setting data works as expected
# ..

# Note that missing data is represented by np.na values;
# they're not taken along in calculations. 
# Some methods exist to handle nan values
# e.g.: dropna, isna, fillna
# ..

# Other operations
my_df2.mean(0) # axis 1
my_df2.mean(1) # axis 2

# Similar to R, apply can be performed
my_df2.apply(np.cumsum)

# There's histogram functionality in the pd.Series 
my_series = pd.Series(np.round((np.random.randn(10)+1)/2*10))
my_series.value_counts()
 
# Also some string methods exist, e.g. str.lower()

############################################################
# Even more manipulations, such as reshaping

# Various ways exist to do (advanced) merging of dataframes.


# One can also "pivot" datafrmaes, meaning a table is constructed based
# on indexing parameters and values (basically the reverse of melt in R)

##################
# Grouping can be used to group by values and then perform actions
# (Similar to R)
# What happens here:
# A dataframe is created with two levels of indexing, 
# indexing can be used to group by these levels (ie ~categories)
arrays = [['Falcon', 'Falcon', 'Parrot', 'Parrot'],
          ['Captive', 'Wild', 'Captive', 'Wild']]
index = pd.MultiIndex.from_arrays(arrays, names=('Animal', 'Type'))
df = pd.DataFrame({'Max Speed': [390., 350., 30., 20.]},
                  index=index)
df

df.groupby(level=0).mean()
df.groupby(level='Animal').mean()

df.groupby(level=1).mean()

# diff will calculate the difference between rows,
# where it will calculate (row_[i] - row_[i-1])
df.groupby(level=0).diff()

##################

# There's something called "stacking" (??)


# There's special functions that can interact with time series (such as 
# frequency resampling)


# One can use categoricals


# df.plot automatically plots names of params etc..



############################################################
# Handling data

# Read CSV
#pd.read_csv('foo.csv')

# Write CSV
#df.to_csv('foo.csv')


# Read HDF5
#pd.read_hdf('blabla.h5')
# Write HDF5
#pd.to_hdf('blabla.h5')


# Similar for Excel, with some added convenient params
#pd.read_excel('blabla.xls', 'Sheet1', index_col=None, na_values=['NA'])
#pd.to_excel('blabla.xlsx', sheet_name='Sheet1')





























