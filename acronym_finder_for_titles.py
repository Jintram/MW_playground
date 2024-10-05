#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 29 15:49:17 2022

@author: m.wehrens
"""

import pandas as pd
import re

words_ = \
  pd.read_csv('/Users/m.wehrens/Data_notbacked/misc_data/all_english_words__corncob_lowercase.txt', header=None).loc[:,0].tolist()

# filter non-words
words = np.array([str(w) for w in words_])

myquery='uhcmmc'
myquery='udsmi'
myquery='cdsmi'

search_result = \
    words[np.where([all([l in w for l in myquery]) for w in words])]
    
    
#####    
    
myquery = ['^[c|u]','[h]','[s|m]','m','[i|c]']
myquery = ['^[c|u]','[h]','[s|m]','m','m']
myquery = ['^[c|u|m|e|d|r|s]','[h|c]','s','m','m']
myquery2 = '.*'.join(myquery)

[w for w in words if bool(re.search(myquery2, w))]
    
#####

myquery = ['^[c|u|m|e|d|r|s]','[h|c]','s','m']
myquery = ['^[c|u|m|e|d|r|s]','[h|c]','m','s','m']
myquery = ['^[c|u|m|e|d|r|s]','[h|c]','s','m']
myquery2 = '.*'.join(myquery)

[w for w in words if bool(re.search(myquery2, w))]

single cell transcriptomics to a new mouse model, with automatic microscopy validation    

myquery = ['^s','m','[c|u|m|e|d|r|s]','[h|c]']
myquery2 = '.*'.join(myquery)

[w for w in words if bool(re.search(myquery2, w))]

myquery = ['^[s|c|t]','m','[a|m|v]','[h|d]$']
myquery2 = '.*'.join(myquery)

[w for w in words if bool(re.search(myquery2, w))]

    
#####

myquery = ['^[i|f|p]','[d]','[h|c]','s','m']
myquery2 = '.*'.join(myquery)

[w for w in words if bool(re.search(myquery2, w))]

myquery = ['^[d]','s','m$']
myquery2 = '.*'.join(myquery)

[w for w in words if bool(re.search(myquery2, w))]


#####

# Manual match, in order

intermediate_result = \
        [
        ''.join([w[i] for i in np.where([l in myquery for l in w])[0]])
                         for w in words]

    # np.where([l in words[851] for l in myquery])

# np.where([r == myquery for r in intermediate_result])

final_result = \
    [words[i] for i in np.where([r == myquery for r in intermediate_result])]

final_result

#####


search_result

all([type(w)==str for w in words])

words[[type(w)==str for w in words]]

np.where([type(w)!=str for w in words])

words[34074]