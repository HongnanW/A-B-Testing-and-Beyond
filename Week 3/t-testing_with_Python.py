## Instagram t-test Example

## Import necessary packages
import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import scipy.stats as sp


## Change working directory
os.chdir("/Users/ntstevens/Dropbox/Data Institute/DOE Certificate/Week 3")

## Read in the data
data = pd.read_csv('instagram.csv')
cond1 = data["Condition_1"]
cond2 = data["Condition_2"]

## Plot the data
plt.subplot(2,1,1)
x_lo = np.floor(np.min([np.min(cond1), np.min(cond2)]))
x_hi = np.ceil(np.max([np.max(cond1), np.max(cond2)]))
plt.hist(cond1, range = [x_lo, x_hi], color = "white")
plt.axvline(x= np.mean(cond1), color = "red", linewidth = 2)
plt.title("MInutes Engaged")
plt.xlabel("Condition 1")
plt.ylabel("Frequency")
plt.subplot(2,1,2)
plt.hist(cond2, range = [x_lo, x_hi], color = "white")
plt.axvline(x= np.mean(cond2), color = "red", linewidth = 2)
plt.xlabel("Condition 2")
plt.ylabel("Frequency")

## Perform the t-test
## Ho: mu1 = mu2 vs. Ha: mu1 != mu2
sp.ttest_ind(cond1, cond2, equal_var = True)

## Unfortunately ttest_ind() only calculates p-values for two-sided tests.
## In order to calculate p-values associated with one-sided tests, we have to 
## do it manually. That said, it is not very difficult since the p-value 
## associated with one-sided tests is closely related to the p-value from the
## two-sided test

## Ho: mu1 <= mu2 vs. Ha: mu1 > mu2
t, p = sp.ttest_ind(cond1, cond2, equal_var = True)
## The test statistic asspciated with this test is the same as the two-sided test:
t
## Because Ha involves the 'greater than' sign, the p-value in this case is one half 
## of the two-sided p-value:
p/2
## This can also be calculated as P(T>=t) where T follows the t-distribution with 
## 998 degrees of freedom. Notice that CDF's calculate probabilities of the form
## P(X<=x), and so P(T>t) = 1 - CDF(t):
1 - sp.t(df = 998).cdf(t)

## Ho: mu1 >= mu2 vs. Ha: mu1 < mu2
t, p = sp.ttest_ind(cond1, cond2, equal_var = True)
## The test statistic asspciated with this test is the same as the other two:
t
## Because Ha involves the 'less than' sign, the p-value in this case is one-minus one-half 
## of the two-sided p-value:
1 - (p/2)
## This can also be calculated as P(T<=t) where T follows the t-distribution with 
## 998 degrees of freedom. Notice that CDF's calculate probabilities of the form
## P(X<=x), and so P(T<=t) = CDF(t):
sp.t(df = 998).cdf(t)

