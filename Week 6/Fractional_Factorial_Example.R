## 2^(8-4) Fractional Factorial Example

## Get the data
setwd("/Users/ntstevens/Dropbox/Data Institute/DOE Certificate/Week 6/")
data <- read.csv(file = "chehalem.csv", header = T)
A <- factor(data$A, levels = c(-1,1), labels = c("Pommard", "Wadenswil"))
B <- factor(data$B, levels = c(-1,1), labels = c("Allier", "Troncais"))
C <- factor(data$C, levels = c(-1,1), labels = c("Old", "New"))
D <- factor(data$D, levels = c(-1,1), labels = c("Champagne", "Montrachet"))
E <- factor(data$E, levels = c(-1,1), labels = c("None", "All"))
F <- factor(data$F, levels = c(-1,1), labels = c("Light", "Medium"))
G <- factor(data$G, levels = c(-1,1), labels = c("None", "10%"))
H <- factor(data$G, levels = c(-1,1), labels = c("Low", "High"))
y <- data$y

## Figure out what has been aliased
library(FrF2)
model.2fi <- lm(y~(A+B+C+D+E+F+G+H)^2, data = data) # Create a model with up to 2-factor interactions
summary(model.2fi) # Notice not everything was estimated
aliases(model.2fi) # This gives us the aliasing structure

model.3fi <- lm(y~(A+B+C+D+E+F+G+H)^3, data = data) # Create a model with up to 3-factor interactions
summary(model.3fi) # Notice not everything was estimated
aliases(model.3fi) # This gives us the aliasing structure

model.4fi <- lm(y~(A+B+C+D+E+F+G+H)^4, data = data) # Create a model with up to 4-factor interactions
summary(model.4fi) # Notice not everything was estimated
aliases(model.4fi) # This gives us the aliasing structure

## Complete aliasing structure:
aliases(lm(y~(A+B+C+D+E+F+G+H)^8, data = data)) # use 8 here because k=8 is the number of factors being investigated and is the largest interaction possible

## Fit the "full" model with all main effects and two-factor interactions
model.full <- lm(y ~ A+B+C+D+E+F+G+H+A:B+A:C+A:D+A:E+A:F+A:G+A:H, data = data)
summary(model.full)

## Identify the most influential factors
effects <- 2*model.full$coefficients[2:length(model.full$coefficients)]
abs(q$y)[order(-abs(q$y))] #figure out which effects are the ones lying away from the line

## Note: it appears as though factors D, E, F, G have the largest main effects and the 2-factor 
##       interactions AC = DF, AH = FG, and AD = EG are most important. Let's try fitting a reduced model 
##       with just these terms
model.red <- lm(y ~ A+ B + C + D + E + F + G + D:F + F:G + E:G, data = data)
summary(model.red)
anova(model.red, model.full)

## Main Effects plots
library(gplots)
par(mfrow=c(2,2), oma = c(0,0,2,0)) 
# par(mfrow=c(2,2), oma = c(0,0,0,0)) 
plotmeans(formula = y~A, ylab = "Tasting Score", xlab = "Pinot Clone (A)", ylim = c(1, 16), data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("Pommard", "Wadenswil"))
plotmeans(formula = y~B, ylab = "Tasting Score", xlab = "Oak Type (B)", ylim = c(1, 16), data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("Allier", "Troncais"))
plotmeans(formula = y~C, ylab = "Tasting Score", xlab = "Barrel Age (C)", ylim = c(1, 16), data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("Old", "New"))
plotmeans(formula = y~D, ylab = "Tasting Score", xlab = "Yeast Contact (D)", ylim = c(1, 16), data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("Champagne", "Montrachet"))
mtext("Main Effect Plots 1", outer = TRUE, cex = 1.5)
plotmeans(formula = y~E, ylab = "Tasting Score", xlab = "Stems (E)", ylim = c(1, 16), data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("None", "All"))
plotmeans(formula = y~F, ylab = "Tasting Score", xlab = "Barrel Toast (F)", ylim = c(1, 16), data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("Light", "Medium"))
plotmeans(formula = y~G, ylab = "Tasting Score", xlab = "Whole Cluster (G)", ylim = c(1, 16), data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("None", "10%"))
plotmeans(formula = y~H, ylab = "Tasting Score", xlab = "Fermentation Temp (H)", ylim = c(1, 16), data = data, xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("Low", "High"))
mtext("Main Effect Plots 2", outer = TRUE, cex = 1.5)
## Notes: It is clear that yeast type (D) and the amount of whole clusters (G) used during fermentation are 
##        most important, with no whole clusters and Montrachet yeast producing a better tasting wine. Although 
##        not significant, medium barrel toast (F) and no stems (E) seem to correspond to a  better tasting 
##        Pinot Noir.

## Interaction Plots
par(mfrow=c(1,3), oma = c(0,0,2,0))
# par(mfrow=c(1,3), oma = c(0,0,0,0))
interaction.plot(D, F, y, ylab = "Mean Response Rate", xlab = "Yeast Type (D)", main = "", ylim = c(1, 16), legend = FALSE)
points(x = c(1,1), y = c(mean(data[data$D==-1 & data$F==-1,]$y),mean(data[data$D==-1 & data$F==1,]$y)), pch = 1)
points(x = c(2,2), y = c(mean(data[data$D==1 & data$F==-1,]$y),mean(data[data$D==1 & data$F==1,]$y)), pch = 1)
legend("bottomleft", legend = c("Toast (F)","Medium", "Light"), lty = c(1,1,2), col=c("white", "black", "black"), cex = 0.75)
interaction.plot(F, G, y, ylab = "Mean Response Rate", xlab = "Barrel Toast (F)", main = "", ylim = c(1, 16), legend = FALSE)
points(x = c(1,1), y = c(mean(data[data$F==-1 & data$G==-1,]$y),mean(data[data$F==-1 & data$G==1,]$y)), pch = 1)
points(x = c(2,2), y = c(mean(data[data$F==1 & data$G==-1,]$y),mean(data[data$F==1 & data$G==1,]$y)), pch = 1)
legend("bottomleft", legend = c("Whole Cluster (G)","10%", "None"), lty = c(1,1,2), col=c("white", "black", "black"), cex = 0.75)
interaction.plot(E, G, y, ylab = "Mean Response Rate", xlab = "Stems (E)", main = "", ylim = c(1, 16), legend = FALSE)
points(x = c(1,1), y = c(mean(data[data$E==-1 & data$G==-1,]$y),mean(data[data$E==-1 & data$G==1,]$y)), pch = 1)
points(x = c(2,2), y = c(mean(data[data$E==1 & data$G==-1,]$y),mean(data[data$E==1 & data$G==1,]$y)), pch = 1)
legend("bottomleft", legend = c("Whole Cluster (G)","10%", "None"), lty = c(1,1,2), col=c("white", "black", "black"), cex = 0.75)
mtext("Interaction Plots", outer = TRUE, cex = 1.5)
## Notes: If yeast type is Montrachet, barrel toast doesn't matter much, but if yeast type is Champagne, a medium 
##        barrell toast is best. And if barrel toast is chosen to be medium, then not including any whole-clusters
##        is best. If using none of the stems, then don't use whole clusters.

## Conclusions: This study was able to identify a small number of important factors and interactions that influence 
##              a Pinot Noir's flavor. Perhaps importantly, it has identified which factors do not have a significant
##              influence. In line with the philosophy of sequential experimentation, this information can then be 
##              exploited in future studies.

