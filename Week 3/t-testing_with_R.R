## Instagram t-test Example

## Change working directory
setwd(dir = "/Users/ntstevens/Dropbox/Data Institute/DOE Certificate/Week 3/")

## Read in the data
data <- read.csv(file = "instagram.csv", header = T)
cond1 <- data$Condition_1
cond2 <- data$Condition_2

## Plot the data
par(mfrow=c(2,1))
xmin <- min(min(cond1), min(cond2))
xmax <- max(max(cond1), max(B=cond2))
hist(cond1, xlim = c(xmin, xmax), main = "Minutes Engaged", xlab = "Condition 1")
abline(v = mean(cond1), col = "red", lwd = 2)
hist(cond2, xlim = c(xmin, xmax), main = "", xlab = "Condition 2")
abline(v = mean(cond2), col = "red", lwd = 2)

## Perform the test
## Ho: mu1 = mu2 vs. Ha: mu1 != mu2
t.test(x = cond1, y = cond2, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

## Ho: mu1 <= mu2 vs. Ha: mu1 > mu2
t.test(x = cond1, y = cond2, alternative = "greater", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

## Ho: mu1 >= mu2 vs. Ha: mu1 < mu2
t.test(x = cond1, y = cond2, alternative = "less", mu = 0, paired = F, var.equal = T, conf.level = 0.95)


