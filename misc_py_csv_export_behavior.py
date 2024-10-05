#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 25 10:45:28 2021

@author: m.wehrens
"""

import pandas

df = pandas.DataFrame({'name': ['Raphael', 'Donatello'],
                   'mask': ['red', 'purple'],
                   'weapon': ['sai', 'bo staff']})
df.index = df.name.values
df.to_csv(index=False)


df.to_csv(path_or_buf='/Users/m.wehrens/Desktop/test.tsv',  index=True, sep='\t')
df.to_tsv(path_or_buf='/Users/m.wehrens/Desktop/test.csv',  index=True)