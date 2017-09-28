## Candy Crush F-test Example

## Change working directory
setwd(dir = "/Users/ntstevens/Dropbox/Data Institute/DOE Certificate/Week 4/")

## Read in the data
candy <- read.csv(file = "candycrush.csv", header = T)

## Plot the data
boxplot(time ~ booster, data = candy, xaxt = "n", ylab = "Length of Game Play (minutes)", xlab = "Condition")
axis(side = 1, at = c(1,2,3), labels = c("Lollipop Hammer", "Jelly Fish", "Color Bomb"))

## Perform the test by fitting a linear regression model
model <- lm(time ~ factor(booster), data = candy)
summary(model)

