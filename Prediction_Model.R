################### Start of header ######################
# Title: CA 4 Prediction Modelling (Crime in Ireland)
#  
# Description: 
#
# <This code is for CA 4, which is data
#  from the previous dataset in CA3 has been used
#  to build a prediction model. The prediction model
#  is built using linear regression.>
#
# Author: <Rituparna Parial>  
# Date: <30-05-2019>
#
################### End of header ########################

#----------LOADING AND EXPLORING DATASET-------------#

# Since the dataset is in Excel or xlsx format first 
# xlsx package is installed and data is loaded onto
# drunk.crime dataframe
install.packages("xlsx")
library("xlsx")
drunk.crime <- read.xlsx("Ddriving.xlsx", 1)

# To view structure of dataset
str(drunk.crime)

# Taking only variables on interest in a seperate dataset
variable.crime <- drunk.crime[c(3,4,5)]

# Viewing structure of new dataset variable.crime
str(variable.crime)

# Viewing first 5 values
head(variable.crime)

# In CA3 it was found that the null hypothesis was rejected
# and alternate was accepted where p value obtained using 
# the Spearman's Correlation Coefficient test was
# p-value = 4.25e-10 

# Before applying linear regression models, a verification is needed 
# that several assumptions are met.These assumptions are:
#  -Linearity
#  -Multiple colinearity
#  -Mean of residuals is 0
#  -homoscedasticity (even distribution of residuals)

# Scatterplot matrix (Visually representing the data)
pairs(variable.crime)

# Creating a correlation matrix
cor(variable.crime) 
summary(variable.crime)
# library(psych)
library(car)


#------------- Checking for non-linearity---------------- #
pairs.panels(variable.crime, scale = FALSE, col = "red")
# it is seen that unemployment rate doesn't have a clear linearity
# this is fixed by taking a log of the variable and transforming it
# Note: all values must strictly be positive for transforming to log

variable.crime$unemployment.rate <- log10(variable.crime$unemployment.rate)
pairs.panels(variable.crime, scale = FALSE, col = "red")

# To find the best model each variable is fit individually and 
# as a combination, their AIC values are then compared the 
# lower the AIC the better the model.

model1 <- lm(drunk.driving.cases ~ total.injuries, data = variable.crime)
summary(model1)
AIC(model1)


model2 <- lm(drunk.driving.cases ~ unemployment.rate, data = variable.crime)
summary(model2)
AIC(model2)


model3 <- lm(drunk.driving.cases ~ ., data = variable.crime)
summary(model3)
AIC(model3)

# the model3 has a higher R-squared and lower
# AIC value which makes it a better model than the other two.

# For prediction the dependent variable is drunk.driving.cases 
# which has to be predicted using the independent variables which 
# are total.injuries and unemployment.rate.

#------------Testing for Multiple colinearity----------------#
install.packages("fmsb")
library(fmsb)
VIF(model3)
# As a general rule, VIF < 5 is acceptable (VIF = 1 means there 
# is no multicollinearity), the value of VIF found for the model
# is 1.882736 which is < 5 hence there is no multicollinearity


#-------------Testing mean of residual for model-------------#
mean(model3$residuals)
# Since the mean of residuals is approximately zero, this assumption holds 
# true for this model.

#----------Global validation of linear model assumtions------#
gvlma::gvlma(model3)

# Therefore with the above tests we can confirm that model is fit
# to predict values in linear modelling

#-------------------------Prediction-------------------------#
# Create the training (development) and test (validation) 
# data samples from original data.

# setting seed to reproduce results of random sampling
set.seed(100) 


# the dataset is split into a 80:20 sample (training:test),
# then, model is built on 80% sample and thus is used
# to predict the dependent variable on test data.
# train_row_index is the row indices for training the data.
train_row_index <- sample(1:nrow(variable.crime), 0.8*nrow(variable.crime))

# model training data
trainingData <- variable.crime[train_row_index, ] 

# model testing data
testingData <- variable.crime[-train_row_index, ]

# Develop the model on the training data for
# predicting the drunk.driving.cases on test data
final_regression_model <- lm(drunk.driving.cases ~ ., data=trainingData)
Final_prediction <- predict(final_regression_model, testingData)

summary(final_regression_model)

# Call here shows the function call used to compute the regression model.
# Residuals - Provide a quick view of the distribution of the residuals, 
# which by definition have a mean zero. 
# Therefore, median should not be far from zero, and the minimum and maximum 
# should be roughly equal in absolute value.( for given model min max are roughly
# similar but median is much greater than 0 )

# Coefficients - show the regression beta coefficients and their statistical significance.
# The equation provided by summary is:
# drunk.driving.cases = (intercept) + (total.injuries)*X1 + (unemployment.rate)*X2
# drunk.driving.cases = (1.469e+03) + (5.590e-01)*X1 + (-1.968e+03)*X2

# Predictor variables that are significantly associated to the outcome variable, are marked by stars.
# Both our variables are significantly associated as they have 3 stars.
# Residual standard error (RSE), R-squared (R2) and the F-statistic are metrics that 
# are used to check how well the model fits to the data.
# Note: Model has a decent R-squared value of 0.5403


AIC(final_regression_model)
BIC(final_regression_model)

# Calculating prediction accuracy and error rate
# making an actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actual_value=testingData$drunk.driving.cases, 
                                  predicted_value=Final_prediction))  
# Checking correlation of actuals and predictions
cor(actuals_preds$actual_value, actuals_preds$predicted_value)
# hence the correlation obtained is 0.426 which is very high
# but is significant this implies actual and predicted values 
# have about 42.6% similar directional movement

# Viewing the predicted values against the actuals:
head(actuals_preds,20)

# Calculating the MinMaxAccuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy
# the higher MinMaxAccuracy the higher the value ( near to 1 perfect prediction)
# the better the prediction, here the value obtained is 0.4698 which is not bad

# Calculating the MAPE
mape <- mean(abs((actuals_preds$predicted_value 
                  - actuals_preds$actual_value))/actuals_preds$actual_value) 
mape
# MAPE(Mean Absolute Percentage error) --> 2.463548

#---------- Plotting model residuals ------------#
plot(final_regression_model, pch=16, which=1)
# the residuals plot shows lots of important points still 
# lying far away from the middle area of the graph and
# the line is not straight as it should be

#------Visualising data points of the model as an interactive 3D plot------# 
install.packages("predict3d")
install.packages("rgl")
library(predict3d)
library(rgl)

plot3d(variable.crime$drunk.driving.cases, variable.crime$total.injuries, 
       variable.crime$unemployment.rate, type="s", size=1, lit=TRUE, 
       xlab = "Drunk driving cases", ylab="Total injuries",zlab = "Unemployment rate",
       main = "Drunk driving cases V/s Total injuries V/S Unemployment Rate",sub="3-D Plot")


#----------------------------End of Code---------------------------------#
##########################################################################
