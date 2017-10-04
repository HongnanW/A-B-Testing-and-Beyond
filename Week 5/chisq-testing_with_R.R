######################################
## Optimizely Chi-Squared test Example

## The observed number of conversions in condition 1 was 280 and in condition 2 it was 399. These conditions respectively
## contained n1 = 8872 and n2 = 8642 experimental units. This is all the information we need to perform the chi-squared test
## in R. We don't need the raw data itself, just these summaries.

## Perform the test
## Ho: pi1 = pi2 vs. Ha: pi1 != pi2
prop.test(x = c(280, 399), n = c(8872, 8642), alternative = "two.sided", correct = F)

## Ho: pi1 <= pi2 vs. Ha: pi1 > pi2
prop.test(x = c(280, 399), n = c(8872, 8642), alternative = "greater", correct = F)

## Ho: pi1 >= pi2 vs. Ha: pi1 < pi2
prop.test(x = c(280, 399), n = c(8872, 8642), alternative = "less", correct = F)



###################################
## Nike SB Chi-Squared test Example

## The observed number of views in conditions 1 to 5 were respectively, 160, 95, 141, 293, and 196. These conditions 
## contained n1 = 5014, n2 = 4971, n3 = 5030, n4 = 5007, and n5 = 4980 experimental units. Using this only this information 
## we can now  perform the chi-squared test in R. 

## Perform the test
## Ho: pi1 = pi2 = pi3 = pi4 = pi5 vs. Ha: pij != pik for some k != j
prop.test(x = c(160, 95, 141, 293, 197), n = c(5014, 4971, 5030, 5007, 4980), correct = F)

