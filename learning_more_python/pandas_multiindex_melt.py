

import pandas as pd

# Example MultiIndex DataFrame
df = pd.DataFrame({'A': {0: 'a', 1: 'b', 2: 'c'},
                   'B': {0: 1, 1: 3, 2: 5},
                   'C': {0: 2, 1: 4, 2: 6}})
df.columns = [list('ABC'), list('DEF')]

print("Original DataFrame:")
print(df)

# Melt the DataFrame
df_melted = pd.melt(df, col_level=0, id_vars=['A'], value_vars=['B', 'C'])
df_melted = pd.melt(df, col_level=0, id_vars=['A'])

print("\nMelted DataFrame:")
print(df_melted)