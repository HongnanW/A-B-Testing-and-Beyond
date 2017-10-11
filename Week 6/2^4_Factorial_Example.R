## 2^4 Factorial Example with Logistic Regression

## Get the data
setwd("/Users/ntstevens/Dropbox/Data Institute/DOE Certificate/Week 6/")
data <- read.csv(file = "creditcard_example.csv", header = T)

## Fit a full model with all main effects and interaction terms
model <- glm(y ~ x1 * x2 * x3 * x4, family = binomial(link = "logit"), data = data)
summary(model)

## Fit a reduced model with just the main effects and interactions that 
## appear to be significant
model_red <- glm(y ~ x1 + x2 + x3 + x4 + x1:x2 + x3:x4, family = binomial(link = "logit"), data = data)
summary(model_red)


## Are the full and reduced models significantly different?
a <-anova(model_red, model)
pval <- pchisq(q = a$Deviance[2], df = a$Df[2], lower.tail = F)
pval

## Main Effects plots
library(gplots)
par(mfrow=c(2,2), oma = c(0,0,2,0)) 
# par(mfrow = c(2,2), oma = c(0,0,0,0))
plotmeans(formula = y~x1, ylab = "Conversion Rate", xlab = "Annual Fee (x1)", data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("Current", "Lower"))
plotmeans(formula = y~x2, ylab = "Conversion Rate", xlab = "Account-opening Fee (x2)", data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("No", "Yes"))
plotmeans(formula = y~x3, ylab = "Conversion Rate", xlab = "Initial Interest Rate (x3)", data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("Current", "Lower"))
plotmeans(formula = y~x4, ylab = "Conversion Rate", xlab = "Long-term Interest Rate (x4)", data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("Low", "High"))
mtext("Main Effect Plots", outer = TRUE, cex = 1.5)

## Interaction Plots
x1 <- data$x1
x2 <- data$x2
x3 <- data$x3
x4 <- data$x4
y <- data$y
par(mfrow=c(1,2), oma = c(0,0,2,0))
# par(mfrow=c(1,2), oma = c(0,0,0,0))
interaction.plot(x1, x2, y, ylab = "Conversion Rate", xlab = "Annual Fee (x1)", main = "", legend = F, xaxt = "n", data = data)
points(x = c(1,1), y = c(mean(data[data$x1==-1 & data$x2==-1,]$y), mean(data[data$x1==-1 & data$x2==1,]$y)), pch = 1)
points(x = c(2,2), y = c(mean(data[data$x1==1 & data$x2==-1,]$y), mean(data[data$x1==1 & data$x2==1,]$y)), pch = 1)
axis(side = 1, at = c(1,2), labels = c("Current", "Lower"))
legend("topleft", legend = c("Opening Fee (x2)","Yes", "No"), lty = c(1,1,2), col=c("white", "black", "black"), cex = 0.75)
interaction.plot(x3, x4, y, ylab = "Conversion Rate", xlab = "Initial Interest Rate (x3)", main = "", legend = F, xaxt = "n")
points(x = c(1,1), y = c(mean(data[data$x3==-1 & data$x4==-1,]$y), mean(data[data$x3==-1 & data$x4==1,]$y)), pch = 1)
points(x = c(2,2), y = c(mean(data[data$x3==1 & data$x4==-1,]$y), mean(data[data$x3==1 & data$x4==1,]$y)), pch = 1)
axis(side = 1, at = c(1,2), labels = c("Current", "Lower"))
legend("bottomright", legend = c("Long-term Rate (x4)","High", "Low"), lty = c(1,1,2), col=c("white", "black", "black"), cex = 0.75)
mtext("Interaction Plots", outer = TRUE, cex = 1.5)

## So which condition(s) is (are) best?
X <- data.frame(x1 = kronecker(rep(1,8), c(-1,1)), x2 = rep(kronecker(c(-1,1), rep(1,2)), 4), x3 = rep(kronecker(c(-1,1), rep(1,4)), 2), x4 = kronecker(c(-1,1), rep(1,8)))
pred <- data.frame(X, fit = predict(model_red, newdata = X, type = "response"))
pred[order(-pred$fit),]

## Conditions 6, 2, and 14 all have a low annual fee and no account-opening fee
cond6 <- sum(data[data$Cond == 6, ]$y)
cond2 <- sum(data[data$Cond == 2, ]$y)
cond14 <- sum(data[data$Cond == 14, ]$y)
prop.test(x = c(cond6, cond2, cond14), n = rep(7500, 3))

## None of these is significantly different -- so choose the one that
## maximizes profits for the credit card company

