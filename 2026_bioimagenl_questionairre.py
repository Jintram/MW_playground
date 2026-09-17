
# %%

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

# %%

df_answers = pd.read_excel("/Users/m.wehrens/Documents/TEACHING/Adv-Img-Analysis_NL-BioImg_2026/Advanced NL-BioImaging_Course preparation questionaire.xlsx")

# %%

df_answers.head()

# %%

# [len(X) if isinstance(X, list) else 0 for X in df_answers.loc[:,"What Python tools/packages have you used?"].str.split(",")]
df_answers["lib_count"] = df_answers.loc[:,"What Python tools/packages have you used?"].str.split(",").str.len().fillna(0)

# %%

sns.histplot(df_answers, x="lib_count", 
             binrange=(-0.5, 10.5), bins=11)
plt.xlabel("Nr of libraries experience with")
plt.ylabel("Times seen")
plt.title("Questionnaire answers")

# %%
