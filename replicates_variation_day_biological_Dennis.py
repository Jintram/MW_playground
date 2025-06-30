
"""
If we assume that there are two independent sources of variation, var_biological, and var_day, then we can construct the following data:

            d1          d2          d3          d4          d5
r1  101.019344  101.247928  100.498027  100.873381  101.225801
r2   96.186912   96.993106   96.692930   96.521574   96.631966
r3  102.053294  102.358147  101.947377  102.146920  102.295915
r4  102.689795  102.576458  103.079509  103.022818  102.270860
r5   94.073679   93.247191   93.754499   93.485396   94.735852

With columns being days and rows being replicates, in which each datapoint is constructed by taking a pre-determined mean value (100), plus a biological replicate dependent value var_r1, which has variance 10, that is constant for all of the values for a replicate, and a day-today value var_day which has variance 0.1, and is different for every measurement.
For this synthetic data, we expect a day-to-day CV of sqrt(.1)/100 = 0.003162.

We can then estimate the variance per day as follows:
CV per replicate:
r1    0.003041
r2    0.003022
r3    0.001656
r4    0.003243
r5    0.006166

(Already they are in the right ballpark.)
Now the overall mean CV for the synthetic data is: 0.00343, pretty close to the theoretical expected value.

Had we calculated the daily averages, and calculated the CV from that, this would have yielded: 0.000900; which is not correct.

I'm not sure the assumptions hold, though.
Isn't every biological sample simply going through all kinds of possible states, independent from the other samples. 
And theoretically, all samples are equally likely to be in states the others can also be in. In that case, biological 
variation and day-to-day variation would only reflect a underlying memory and noise parameters, that explain respectively 
how fast each sample cycles through how much variation.
"""

# import argparse
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

def simulate(meanB, N: int, T: int, var_B: float, var_D: float, seed: int):
    """
    I don't think this model is well thought-trough, this is just compound
    noise from two sources, which is probably not the biological situation.
    ---
    Returns (replicate_means, data_matrix).

    replicate_means : shape (N,)      – mean value for each biological replicate
    data_matrix     : shape (N, T)    – day-level measurements
    """
    # N=4; T=5; var_B=.25; var_D = 5; seed = 21
    rng = np.random.default_rng(seed)

    # 1. Between-replicate means
    replicate_means = rng.normal(loc=meanB, scale=np.sqrt(var_B), size=N)

    # 2. Day-to-day values around each mean
    data = rng.normal(loc=replicate_means[:, None],
                      scale=np.sqrt(var_D),
                      size=(N, T))

    return replicate_means, data

    
# Make the simulation data
myN=10_000
myT=10_000
replicate_means, data = simulate(meanB=100, N=myN, T=myT, var_B=10, var_D = .1, seed = 42)

# double check variance is as expected:
df_data.T.var()
np.mean(df_data.T.var()) # biological variance
np.mean(df_data.var()) # day-to-day variance
    # numbers are ±as expected

# plot some data
plt.plot(data[0:5].T)
plt.show(); plt.close()

# Convert to dataframe
df_data = pd.DataFrame(data)
df_data.columns = ['d'+str(X+1) for X in range(myT)]
df_data.index =   ['r'+str(X+1) for X in range(myN)]


# Calculations on the full data (e.g. myN=10_000, myT=10_000)
#
# Strategy 1 (incorrect)
# first calculate daily means
means_d = df_data.mean()
# then calculate day-to-day CV
np.sqrt(np.var(means_d))/np.mean(means_d)
#
# Strategy 2 (correct)
# first calculate CV per biological replicate
CVs = (np.sqrt(df_data.T.var())/df_data.T.mean())
# then take mean
np.mean(CVs)
#
# theoretical value
np.sqrt(.1)/100


### For a smaller subset (only 5 replicates and days)
df_data_subset = df_data.iloc[0:5, 0:5]
# Strategy 1, incorrect 
means_d_subset = df_data_subset.mean()
np.sqrt(np.var(means_d_subset))/np.mean(means_d_subset)
# Strategy 2, correct
CVs = (np.sqrt(df_data_subset.T.var())/df_data_subset.T.mean())
np.mean(CVs)



################################################################################
# Unfinished


def simulate_better(N, noise_level):
    '''
    Now let's try a model where I have different assumption, ie there is
    day-to-day variation, which has memory, and explores all of phase space;
    observed biological and daily variation are both just a result of this.
    dX/dt = sigma*Noise -theta*(X0-X)
    
    Multiple biological replicates are simply multiple processes that start 
    with different initial conditions (or have run for a while).
    '''
    
    rng = np.random.default_rng(21)
    
    for idx_t in range(N):
        
    XXX