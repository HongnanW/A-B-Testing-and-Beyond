## Instagram Factorial Analysis

## Get the data
setwd("/Users/ntstevens/Dropbox/Data Institute/DOE Certificate/Week 5/")
data <- read.csv(file = "instagram-factorial.csv", header = T)
Time <- data$Time
Frequency <- factor(data$Frequency, levels = c(0,1,2,3), labels = c("None", "7:1", "4:1", "1:1"))
Type <- factor(data$Type, levels = c(1,2), labels = c("Photo", "Video"))

## Numerical summaries of the data
summary(data.frame(Time, Frequency, Type))
sd(Time)

## Graphical Summaries of the data
library(gplots)
par(mfrow = c(1,2))
boxplot(Time ~ Frequency, main = "Boxplot of Session Duration by Ad Frequency", xlab = "Ad Frequency", ylab = "Session Duration (min)")
plotmeans(Time ~ Frequency, main = "Main Effect Plot for Ad Frequency", xlab = "Ad Frequency", ylab = "Mean Session Duration (min)")
boxplot(Time ~ Type, main = "Boxplot of Session Duration by Ad Type", xlab = "Ad Type", ylab = "Session Duration (min)")
plotmeans(Time ~ Type, main = "Main Effect Plot for Ad Type", xlab = "Ad Type", ylab = "Mean Session Duration (min)")
par(mfrow = c(1,1))
interaction.plot(Frequency, Type, Time, main = "Interaction Plot for Ad Frequency and Ad Type", ylab = "Mean Session Duration (min)", xlab = "Ad Frequency")

## Marginal Means
print(paste("No ads -- ", round(mean(data[data$Frequency==0,]$Time), 2), sep = ""))
print(paste("7:1 frequency -- ", round(mean(data[data$Frequency==1,]$Time), 2), sep = ""))
print(paste("4:1 frequency -- ", round(mean(data[data$Frequency==2,]$Time), 2), sep = ""))
print(paste("1:1 frequency -- ", round(mean(data[data$Frequency==3,]$Time), 2), sep = ""))

print(paste("Photo -- ", round(mean(data[data$Type==1,]$Time), 2), sep = ""))
print(paste("Video -- ", round(mean(data[data$Type==2,]$Time), 2), sep = ""))

## Notes:
# Frequency seems to be have a large and significant main effect
# Type seems to have a small and maybe significant main effect
# Interaction between Frequency and Type is existent but minimal
# Thus, the main effects seem to drive variation in the response (engagement time)

## Formal Analysis of Variance
model <- lm(Time ~ Frequency * Type)
summary(model)
anova(model)

## Notes:
# Our intuition based on the plots was correct: the interaction is minimal but significant, and 
# both main effects are, with "Frequency" explaining most of the variation in engagement time.

## The "effect" of Frequency when ad type is a photo
timeBYprev1 <- data.frame(Prev0 = data$Time[data$Frequency==0 & data$Type==1], Prev1 = data$Time[data$Frequency==1 & data$Type==1], Prev2 = data$Time[data$Frequency==2 & data$Type==1], Prev3 = data$Time[data$Frequency==3 & data$Type==1])
timeBYprev1 <- data.matrix(timeBYprev1)
apply(timeBYprev1, 2, mean) # average engagement time by ad Frequency category

effect.mat1 <- matrix(0, nrow = 4, ncol = 4)
pvalue.mat1 <- matrix(0, nrow = 4, ncol = 4)
prev <- 0:3
for (i in 1:4){
  for (j in 1:4){
    ttest <- t.test(data$Time[data$Frequency==prev[i] & data$Type==1], data$Time[data$Frequency==prev[j] & data$Type==1])
    effect.mat1[i,j] <- as.numeric(diff(ttest$estimate))
    pvalue.mat1[i,j] <- as.numeric(ttest$p.value)
  }
}
# The following matrix shows the expected change in engagement time when going from one level of Frequency to another
# The rows and columns correspond to levels 0, 1, 2, 3 (none, 7:1, 4:1, 1:1)
effect.mat1
# The following matrix shows the two sided p-values associated with the comparisons in the previous matrix
# The rows and columns correspond to levels 0, 1, 2, 3 (none, 7:1, 4:1, 1:1)
pvalue.mat1

## The "effect" of Frequency when ad Type is a video
timeBYprev2 <- data.frame(Prev0 = data$Time[data$Frequency==0 & data$Type==2], Prev1 = data$Time[data$Frequency==1 & data$Type==2], Prev2 = data$Time[data$Frequency==2 & data$Type==2], Prev3 = data$Time[data$Frequency==3 & data$Type==2])
timeBYprev2 <- data.matrix(timeBYprev2)
apply(timeBYprev2, 2, mean) # average engagement time by ad Frequency category

effect.mat2 <- matrix(0, nrow = 4, ncol = 4)
pvalue.mat2 <- matrix(0, nrow = 4, ncol = 4)
prev <- 0:3
for (i in 1:4){
  for (j in 1:4){
    ttest <- t.test(data$Time[data$Frequency==prev[i] & data$Type==2], data$Time[data$Frequency==prev[j] & data$Type==2])
    effect.mat2[i,j] <- as.numeric(diff(ttest$estimate))
    pvalue.mat2[i,j] <- as.numeric(ttest$p.value)
  }
}
# The following matrix shows the expected change in engagement time when going from one level of Frequency to another
# The rows and columns correspond to levels 0, 0.1, 0.2, 0.5
effect.mat2
# The following matrix shows the two sided p-values associated with the comparisons in the previous matrix
# The rows and columns correspond to levels 0, 1, 2, 3 (none, 7:1, 4:1, 1:1)
pvalue.mat2

## Condition Means
cond01 <- mean(data[data$Frequency==0 & data$Type == 1,]$Time)
cond02 <- mean(data[data$Frequency==0 & data$Type == 2,]$Time)
cond11 <- mean(data[data$Frequency==1 & data$Type == 1,]$Time)
cond12 <- mean(data[data$Frequency==1 & data$Type == 2,]$Time)
cond21 <- mean(data[data$Frequency==2 & data$Type == 1,]$Time)
cond22 <- mean(data[data$Frequency==2 & data$Type == 2,]$Time)
cond31 <- mean(data[data$Frequency==3 & data$Type == 1,]$Time)
cond32 <- mean(data[data$Frequency==3 & data$Type == 2,]$Time)

## Plots for notes
par(mfrow = c(1,2))
plotmeans(Time ~ Frequency, main = "", xlab = "Ad Frequency", ylab = "Mean Session Duration (min)", ylim = c(1,7))
plotmeans(Time ~ Type, main = "", xlab = "Ad Type", ylab = "Mean Session Duration (min)", ylim = c(1,7))
interaction.plot(Frequency, Type, Time, main = "", ylab = "Mean Session Duration (min)", xlab = "Ad Frequency")
points(x=c(1,1,2,2,3,3,4,4), y = c(cond01, cond02, cond11, cond12, cond21, cond22, cond31, cond32))
interaction.plot(Type, Frequency, Time, main = "", ylab = "Mean Session Duration (min)", xlab = "Ad Type")
points(x=c(1,1,1,1,2,2,2,2), y = c(cond01, cond11, cond21, cond31, cond02, cond12, cond22, cond32))

