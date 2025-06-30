

# Handling color bars in python

import seaborn as sns
import matplotlib.pyplot as plt

sns.set()
tips = sns.load_dataset("tips")
ax = sns.scatterplot(x="total_bill", y="tip", hue="size",
                     palette='RdBu', data=tips)

norm = plt.Normalize(tips['size'].min(), tips['size'].max())
sm = plt.cm.ScalarMappable(cmap="RdBu", norm=norm)
sm.set_array([])

# Remove the legend and add a colorbar
ax.get_legend().remove()
ax.figure.colorbar(sm)

plt.show()