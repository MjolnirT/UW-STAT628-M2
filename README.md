# Bodyfat Analysis
## Author
Group 7: Hao Qin, Jiacheng Mao, Qiaoyu Wang, Yuhan Meng

## Motivation
Nowadays,health becomes a prevalent topic around world. People pay more attention on their bodyfat percentage which is a measurement to evaluate their healthy condition. However,it is inconvenient to get an accurate bodyfat percentage in the daily life. Hence our group would like to find a simple,robust and aslo accurate way to estimate the bodyfat. Our analysis based on a real data set contains 252 males and their common body measurements.

## summary.ipynb

**summary.ipynb** is the file that summarizes all the code we used in the analysis and it includes all the detailed analysis as well as the diagnostics.


## Data
This folder contains two csv files. One is the raw dataset called Bodyfat and the other is the cleaned dataset.

The Bodyfat.csv contains 252 male observations and their body measurements. Here are the variables:

Percent body fat from Siri's (1956) equation  
Density determined from underwater weighing  
Age (years)  
Weight (lbs)  
Height (inches)  
Adiposity (bmi)
Neck circumference (cm)  
Chest circumference (cm)  
Abdomen 2 circumference (cm)  
Hip circumference (cm)  
Thigh circumference (cm)  
Knee circumference (cm)  
Ankle circumference (cm)  
Biceps (extended) circumference (cm)  
Forearm circumference (cm)  
Wrist circumference (cm)  

The cleaningdata.csv contains 248 male observations and some values of measures are changed according to data processing.

## Code
This folder contains all the code we used to complete the analysis. It includes:

**cleaning_data.R :** 			data cleaning

**diagnostic.R & ggplot.R:** plot diagnostic graphics for detecting outliers and influence points

**Crossvalidation.R :**		   Defining the function used for cross validation

**AIC_BIC.ipynb & AIC_BIC.R: **Using stepwise variable selection method to build model

**Adjusted r square.ipynb & Adjusted r square.R :**  Using adjusted r squared criterion to build model

**Mallow's cp.R & Mallow's cp.ipynb:** Using Mallow's cp criterion to fit the model

**Lasso_Elastic_Net.R :**     Using Lasso and elastic net to fit the model

**BeSS.ipynb:** Using BeSS method to select variables and fit model                 

## Images

This folder contains all the figures and tables we produced in our analysis.

## Shiny
link:
