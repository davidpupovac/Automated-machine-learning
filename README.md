# Automated machine learning - a machine for analysis of dichotomous response variables 

The following R code generates fully automated, interactive procedure for the analysis of dichotomous response variables. The procedure consists of four functions:

##	data_sort(x) 
- an interactive function which prompts user to specify dependent/response variable and categorical independent variables, creates dummy variables and produces training, testing and validation datasets;

##	descriptive_statistics(x) 
- a function which produces descriptive statistics, histograms and boxplots, estimates correlation of independent variables with dependent variable and performs principal components analysis;

## subset_selection(x) 
- a function which determines the optimal and parsimonious subset of explanatory/independent variables using logistic regression, ridge regression, the lasso and random forest; the function also produces error estimates based on training and test  datasets and generates relevant graphs;

##	subset_analysis(x,y)
- a function which compares the performance of k nearest neighbor, C5.0 tree algorithm, artificial neural network, support vector machine with radial kernel, and logistic regression on the selected subset of variables while varying available model parameters. 
