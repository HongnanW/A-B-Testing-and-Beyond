## Candy Crush F-test Example

## Import necessary packages
import os
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf


## Change working directory
os.chdir("/Users/ntstevens/Dropbox/Data Institute/DOE Certificate/Week 4")

## Read in the data
candy = pd.read_csv('candycrush.csv')


## Plot the data
y_1 = candy[candy["booster"]==1]["time"]
y_2 = candy[candy["booster"]==2]["time"]
y_3 = candy[candy["booster"]==3]["time"]
y_booster = [y_1, y_2, y_3]
fig = plt.figure()
plt.boxplot(y_booster, labels = ["Lollipop Hammer", "Jelly Fish", "Color Bomb"])
plt.xlabel("Condition")
plt.ylabel("Length of Game Play (minutes)")


## Perform the test by fitting a linear regression model
model = smf.ols('time ~ C(booster)', data = candy).fit()
model.summary()