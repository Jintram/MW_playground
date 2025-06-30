
import pandas as pd
import csv 
import numpy as np

# simply find out if anagrams(?) mach some names

current_anagram=list('BIBIMORELO'.lower())
current_anagram=list('OLEKRAUNO'.lower())
current_anagram=list('MAYATHORSTYX'.lower())
namelist = pd.read_csv('/Users/m.wehrens/Desktop/first-names.txt')

with open('/Users/m.wehrens/Desktop/first-names.txt') as myscsv:
    reader  = csv.reader(myscsv)
    nameslist = [list(row[0].lower()) for row in reader]


# now make first selection
result = [''.join(name) for name in nameslist if np.all(np.isin(name, current_anagram))]

# now for each, check if it is a real anagram, and also keep
# track of which letters are left
def letter_count(mystr):
    output=np.zeros(26)
    for i in mystr:
        output[ord(i)-97]+=1
    return output
def nrs_to_letters(nrs):
    nrs = nrs.astype(int)
    return ''.join([''.join([chr(i+97) for d in range(nrs[i])]) for i in range(len(nrs)) if nrs[i]>0])

kept_names=[]
other_letters=[]
for current_name in result:    
    delta_letters = letter_count(current_anagram) - letter_count(current_name)
    
    if np.all(delta_letters>=0):
        kept_names.append(current_name)
        other_letters.append(nrs_to_letters(delta_letters))

for idx in range(len(kept_names)):
    print(kept_names[idx]+', '+other_letters[idx])
