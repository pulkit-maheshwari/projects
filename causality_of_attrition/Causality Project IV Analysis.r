
################################################

# Load in the data

source("AttritionData_EDA.R")
################################################

library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)
library(stargazer)
library(AER)


# this function converts logits to probabilities. Useful for interpretation of glm.
# input: logit
# output: probability of logit
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

model1 <- glm(Attrition ~ MaritalStatus + Gender, data = AttritionData, family = "binomial")
summary(model1)
logit2prob(coef(model1))
# Based on this logistic regression, married people have a 37.5% probability of leaving their job, while unmarried people have
# a 62.5% probability of attrition, which is close to twice as likely

model2 <- glm(Attrition ~ MaritalStatus + Gender + Age + Gender*MaritalStatus, data = AttritionData, family = "binomial")
summary(model2)
logit2prob(coef(model2))
# Based on this logistic regression, married people have a 40% probability of leaving their job, while unmarried people have
# a 60% probability of attrition, which is close to 50% more likely


model_all <- glm(Attrition ~ MaritalStatus + ., data=AttritionData, family = "binomial")  # with all the independent variables in the dataframe
summary(model_all)
logit2prob(coef(model_all)[2])
# Based on this logistic regression, married people have a 43.6% probability of leaving their job, while unmarried people have
# a 57.4% probability of attrition, which is much closer to an equal probability than the other models


vif(model_all)
vif_values <- vif(model_all)           #create vector of VIF values

barplot(vif_values, main = "VIF Values", horiz = F, col = "steelblue", ylim = c(0,20), las = 2,cex.names = 0.5) #create horizontal bar chart to display each VIF value
abline(h = 10, lwd = 3, lty = 2)    #add vertical line

### These are the variables with a vif value greater than 10
vif_values[vif_values > 10]


model_new <- lm(Attrition ~ MaritalStatus + . -Department_Sales -`JobRole_Laboratory Technician` -`JobRole_Research Scientist` -`JobRole_Human Resources` -`Department_Human Resources`
                , data=AttritionData )  # with all the independent variables in the dataframe
summary(model_new)
# Keep DistanceFromHome, EnvironmentSatisfaction,JobInvolvement,JobSatisfaction,
# NumCompaniesWorked,OverTime,StockOptionLevel,BusinessTravel_Travel_Frequently, `JobRole_Sales Representative` 
logit2prob(coef(model_new)[2])
# Based on this logistic regression, married people have a 49.2% probability of leaving their job, while unmarried people have
# a 51.8% probability of attrition, which is nearly equal probability


keepers <- c(summary(model_new)$coefficients[,4][summary(model_new)$coefficients[,4] < 0.001])
keepers



# OLS - Logistic
ols <- glm(Attrition ~ MaritalStatus + Age, data=AttritionData, family = "binomial")
summary(ols)
logit2prob(coef(ols)[2])


#### Regression 2: Rerun the regression above, using Gender as an instrument controlling for Age

iv <- ivreg(Attrition ~ MaritalStatus + Age | Gender + Age, data=AttritionData)
summary(iv, diagnostics = T)



#####OLS REG WITH CONTROLS 
olsControls <- glm(Attrition ~ MaritalStatus + Gender + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + 
             NumCompaniesWorked + OverTime + StockOptionLevel + BusinessTravel_Travel_Frequently + 
             `JobRole_Sales Representative`, data=AttritionData, family = "binomial")
summary(olsControls)
logit2prob(coef(olsControls)[2])


###### IV REG WITH CONTROLS
iv2 <- ivreg(Attrition ~ MaritalStatus + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + 
              NumCompaniesWorked + OverTime + StockOptionLevel + BusinessTravel_Travel_Frequently + 
              `JobRole_Sales Representative` | Gender + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + 
              NumCompaniesWorked + OverTime + StockOptionLevel + BusinessTravel_Travel_Frequently + 
              `JobRole_Sales Representative`, data=AttritionData)
summary(iv2, diagnostics = T)


#### Recreating via 2SLS
##### Regression 3: first stage
ivfs <- glm(MaritalStatus ~ Gender + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + 
              NumCompaniesWorked + OverTime + StockOptionLevel + BusinessTravel_Travel_Frequently + 
              `JobRole_Sales Representative`,
            data=AttritionData, 
            family = "binomial")
summary(ivfs)

#### Regression 4: reduced form
ivrf <- glm(Attrition ~ Gender + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + 
              NumCompaniesWorked + OverTime + StockOptionLevel + BusinessTravel_Travel_Frequently + 
              `JobRole_Sales Representative`, 
            data=AttritionData, 
            family = "binomial")
summary(ivrf)
#### Use Regression 3 & 4 to recreate the IV estimator
ivrf$coefficients[2]/ivfs$coefficients[2]

#### Performing "manual" tsls 
#### Regression 5: First stage; same as before.
tslsfirst <- glm(MaritalStatus ~ Gender, 
                 data=AttritionData, 
                 family = "binomial")
summary(tslsfirst)

#### Regression 6: Second stage
tslssecond <- lm(Attrition ~ fitted(tslsfirst), 
                 data=AttritionData) 
summary(tslssecond)

#### Combine all the regressions into one form
stargazer(ols,iv,ivfs,ivrf,tslsfirst,tslssecond, type="text")


#############################################
# Testing for Heteroscedasticity
#############################################
bptest(ols)
## Low p-value: the residuals are heteroscedastic

#Adjust for Robust Std Errors
coeftest(ols)             # Coefficient estimates without adjustment
coeftest(ols,vcov=vcovHC) # Coefficient estimates with adjustment

#Interpretation: The parameter estimates remain about the same; however, there's a change in Std Errors. 
#Unfortunately, this change is negligible.

bptest(iv)
## Low p-value: the residuals are heteroscedastic

coeftest(iv)             # Coefficient estimates without adjustment
coeftest(iv,vcov=vcovHC) # Coefficient estimates with adjustment

bptest(ivfs)
# High p-value: the residuals are homoscedastic

bptest(olsControls)
# Low p-value: the residuals are heteroscedastic
coeftest(olsControls)             # Coefficient estimates without adjustment
coeftest(olsControls,vcov=vcovHC) # Coefficient estimates with adjustment

bptest(iv2)
# Low p-value: the residuals are heteroscedastic
coeftest(iv2)             # Coefficient estimates without adjustment
coeftest(iv2,vcov=vcovHC) # Coefficient estimates with adjustment


bptest(ivrf)
# High p-value: the residuals are homoscedastic

bptest(tslsfirst)
# High p-value: the residuals are homoscedastic

bptest(tslssecond)
# High p-value: the residuals are homoscedastic

