# Assignment 4, Neural Networks
# Data 630, Dr. Ed Herranz
# Wesley Clark

# This is an example of a Neural Network using R
# Table of contents
#
#
#
##

# Load the neuralnet library
library("neuralnet", lib.loc="~/Library/R/3.3/library")

# Set the working directory to the appropriate location
setwd("~/Documents/630_3_Dr.H/Assignment5")

# We must load the CSV
library(readr)
whas1 <- read_csv("~/Documents/630_3_Dr.H/Assignment5/whas1.csv")

# Rename the data HDS.  Short for Heart Disease Survival.
# Check to make sure it has properly changed

HDS <- whas1
View(HDS)

# Look at the data using summary, structure, head and names commands
str(HDS)
summary(HDS)
head(HDS)
names(HDS)

#  Our variables are already binary, we do not need to encode them.




# We have to split the data, and set the random seed
set.seed(1234)
ind <- sample(2, nrow(HDS), replace = TRUE, prob = c(0.7, 0.3))
train.data <- HDS[ind == 1, ]
test.data <- HDS[ind == 2, ]
# We make a formula, setting HDS$FSTAT as the Dependent Variable, and using all our other columns as independent variables.
# A tilde, implies every other column, however I decided to list each column individually as a variable.


WesFormula <- HDS$FSTAT ~ HDS$ID + HDS$AGE + HDS$SEX + HDS$CPK + HDS$SHO + HDS$CHF + HDS$MIORD + HDS$MITYPE + HDS$YEAR + HDS$YRGRP + HDS$LENSTAY + HDS$DSTAT + HDS$LENFOL
 
# The formula that is returned is:
# Fstat ~ ID + AGE + SEX + CPK + SHO + CHF + MIORD + MITYPE + YEAR...
# + YRGRP + LENSTAY + DSTAT + LENFOL
 
# We will train a neural network with a hidden layer and 5 nodes next
# hidden = 5, creating a neural net with a singel hidden layer and 10 nodes
# linear.output = FALSE creates a network with a regression output
 
WesNNL <- neuralnet(WesFormula, data = train.data, hidden = c(5), linear.output = FALSE)

# Print the mode
WesNNL

# Print the fomula used to call the model WesNNL
WesNNL$call

# This will print the vaalues of the variables that are used to train the model
WesNNL$response

# Covariate will display the values that were input into the model
WesNNL$covariate

# Model list wil show the name of the input variables and the target variables
WesNNL$model.list

# Net result [1:10] will show the predicted weights for the first 10 items
WesNNL$weights

# The weights after the final iteration are dislayed
WesNNL$net.result[[1]][1 : 10 ]

# The start weights are displayed
WesNNL$startweights

# The connection weights as well as the names of beginning and terminal nodes
WesNNL$result.matrix

# Use the plot function to plot the NNL
plot(WesNNL)

# We can create a new variable, Predict and Predict2.  
# Predict will be used to analyze the test set, and Predict 2 will be used to graph the net.result
# Then we will plot Predict2 on our test data for FSTAT

Predict <- compute(WesNNL, test.data[ , !names(test.data) %in% c('FSTAT')])
Predict2 <- Predict$net.result
plot(Predict2, test.data$FSTAT)

# I will tinker with the neural network, creating a second one with two hidden layers of 10 nodes each
WesNNL2 <- neuralnet(WesFormula, train.data, hidden = c(10, 10), linear.output = TRUE )
PredictNNL2 <- compute(WesNNL2, test.data[ , !names(test.data) %in% c('FSTAT')])
PredictNNL2 <- PredictNNL2$net.result

# Plot the second Neural Network

plot(PredictNNL2, test.data$FSTAT)

# Display the weights!

WesNNL2$weights
PredictNNL2 <- PredictNNL2$net.result
plot(PredictNNL2, test.data$FSTAT)

