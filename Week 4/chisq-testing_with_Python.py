## Optimizely Chi-Squared test Example

## Import necessary packages
import scipy.stats as sp

## The observed number of conversions in condition 1 was 280 and in condition 2 it was 399. These conditions 
## respectively contained n1 = 8872 and n2 = 8642 experimental units which means the observed number of non-
## conversions were 8592 in condition 1 and 8243 in condition 2. The observed frequencies of conversions and
## non-conversions is all the information we need to perform the chi-squared test in Python. We don't need the 
## raw data itself, just these summaries.

# Create an array consisting of the frequencies found in a contingency table
data_optim = [[280, 399],[8592, 8243]]

## Perform the test
## Ho: pi1 = pi2 vs. Ha: pi1 != pi2
t, p, df, exp = sp.chi2_contingency(data_optim)
print("Test statistic =", t, ", df =", df, ", p-value =", p)

## Ho: pi1 <= pi2 vs. Ha: pi1 > pi2
print("Test statistic =", t, ", df =", df, ", p-value =", 1-p/2)

## Ho: pi1 >= pi2 vs. Ha: pi1 < pi2
print("Test statistic =", t, ", df =", df, ", p-value =", p/2)


###################################
## Nike SB Chi-Squared test Example

## The observed number of views in conditions 1 to 5 were respectively, 160, 95, 141, 293, and 196 which means 
## there were respectively 4854, 4876, 4889, 4714, and 4783 non-views in each condition. Using this only this 
## information we can now  perform the chi-squared test in Python. 

## Perform the test
## Ho: pi1 = pi2 = pi3 = pi4 = pi5 vs. Ha: pij != pik for some k != j
data_nike = [[160, 95, 141, 293, 197], [4854, 4876, 4889, 4714, 4783]]
t, p, df, exp = sp.chi2_contingency(data_nike)
print("Test statistic =", t, ", df =", df, ", p-value =", p)
