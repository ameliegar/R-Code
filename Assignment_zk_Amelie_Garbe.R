###########################################################
#                                                         #
#                 Assignment Task 1                       #
#                                                         #
###########################################################

#Hierarchical regression model to predict postoperative pain after wisdom tooth surgery

setwd("/Users/ameliegarbe/Documents/R_Daten/PSYP13_Lund")
getwd()

#load packages
library(psych) # for describe
library(lm.beta) # for lm.beta
library (lsr)
library(dplyr)
library(gsheet)
library(car) # for scatter3d, residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot

#load dataset 1
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")
View(data_sample_1)
attach(data_sample_1)

## data set diagnostics ##
#checking the variables for coding errors
summary(data_sample_1)
describe(data_sample_1) #obersavtion 15 - variable sex - identified as coding error
data_sample_1=data_sample_1[which(row.names(data_sample_1)!="15"), ] #delete row 15

#check for missing data
table(complete.cases(data_sample_1)) #dataset complete - no missing data

#Hierarchical regression
#Model 1 - predictors of pain: age and sex
mod1 <- lm(pain ~ age + sex, data = data_sample_1)
summary(mod1)

#Model 2 - predictors of pain: age, sex, STAI, pain catastrophizing, mindfulness, and cortisol serum and cortiso saliva
mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_1)
summary(mod2)

## Model diagnstic of mod2 ##
#Identify influecial outliers
#Plotting mod2 to identify outliers
plot(mod2, which=4) #plot for cooks distance
plot(mod2, which=5)
#extract the hatvalue
hatvalues(model = mod2) #extract the hatvalue
#identifiy influence of the observation with cooks distance - high influence oberservation is an outlier that has high leverage
cooks.distance(model = mod2) #greater than 1 is often considered large

#check for normally distributed residuals
plot(mod2, which=2) # qq-plot normal dirstributed - linear regression hold true
hist(x=residuals(mod2), xlab="Value of residual", main="", breaks=20)
shapiro.test(summary(mod2)$residuals) #Shapiro-Wilk normality test

#check for linearity
plot(mod2, which=1)
plot(fitted(mod2), residuals(mod2))
residualPlots(mod2) #shows a curved line for age and STAIT but the fitted values are overall linear

#check for homoscedasticity 
plot(mod2, which=3) #mostly constant
ncvTest(mod2)

#check for multicollinearity
vif(mod2) #variance inflation factor
pairs.panels(data_sample_1[,c("cortisol_serum", "cortisol_saliva")], col = "red", lm = T) #check for parwise correlation
                        
#Results of the model1
mod1 <- lm(pain ~ age + sex, data = data_sample_1)
summary(mod1)
confint(mod1) #Conf-interv
AIC(mod1) #AIC
lm.beta(mod1) #standardized coefficents

#Results of model 2.1 (without the predictor cortisol serum)
mod2.1 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_saliva + mindfulness, data = data_sample_1) # model 2 excluded Cortisol_saliva
summary(mod2.1)
confint(mod2.1)
AIC(mod2.1)
lm.beta(mod2.1)

#Compare the two models in terms of how much variance they explain of pain’s variability in the sample
anova(mod1, mod2.1)

#Report Akaike information criterion (AIC) for both models 
AIC(mod1, mod2.1)


###########################################################
#                                                         #
#                 Assignment Task 2                       #
#                                                         #
###########################################################

#Backward regression

#load packages
library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(cAIC4) # for cAIC
library(r2glmm) # for r2beta
library(influence.ME) # for influence
library(lattice) # for qqmath
library(reshape2) # for melt function

#load data set 1
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")
View(data_sample_1)
attach(data_sample_1)

## data set diagnostics ##
#checking the variables for coding errors
summary(data_sample_1)
describe(data_sample_1) #obersavtion 15 - variable sex - identified as coding error
data_sample_1=data_sample_1[which(row.names(data_sample_1)!="15"), ] #delete row 15

#check for missing data
table(complete.cases(data_sample_1)) #dataset complete - no missing data

#Hierarchical regression
#Model 3 - predictors of pain: age, sex, weight, STAI, pain_cat, mindfulness, cortisol serum
mod3 <- lm(pain ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1) #initial model
summary(mod3)
backward_model <- step(mod3, direction = "backward") #backward model of the initial model
summary(backward_model)

## Model diagnstic of mod2 ##
#Identify influecial outliers
#Plotting mod2 to identify outliers
plot(mod3, which=4) #plot for cooks distance
plot(mod3, which=5)
#extract the hatvalue
hatvalues(model = mod3) #extract the hatvalue
#identifiy influence of the observation with cooks distance - high influence oberservation is an outlier that has high leverage
cooks.distance(model = mod3) #greater than 1 is often considered large

#check for normally distributed residuals
plot(mod3, which=2) # qq-plot normal dirstributed - linear regression hold true
hist(x=residuals(mod3), xlab="Value of residual", main="", breaks=20)
shapiro.test(summary(mod3)$residuals)#Shapiro-Wilk normality test - p-value 0.71

#check for linearity
plot(mod3, which=1)
plot(fitted(mod3), residuals(mod3))
residualPlots(mod3) #fitted values are overall linear

#check for homoscedasticity 
plot(mod3, which=3) #mostly constant
ncvTest(mod3)

#check for multicollinearity
vif(mod3) #variance inflation factor 

# backward regression
backward_model <- lm(pain ~ age + pain_cat + cortisol_serum + mindfulness, data = data_sample_1) #backward model of the initial model
theory_based_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_saliva + mindfulness, data = data_sample_1) #Regression model mod2.1.from task 1
summary(backward_model)
confint(backward_model)
AIC(backward_model)
lm.beta(backward_model)
summary(theory_based_model)

#Compare these to models
anova(backward_model, theory_based_model)

AIC(backward_model, theory_based_model) #--> smaller AIC of the theory based model.

#test the two models on some new data
#load data set 2
data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv")
View(data_sample_2)
attach(data_sample_2)

#predictions on pain using the regression models or equations of the backward model and the theory-based model
backward_model2 <- lm(pain ~ age + pain_cat + cortisol_serum + mindfulness, data = data_sample_2) #backward model of the initial model
summary(backward_model2)
theory_based_model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_2) #Regression model mod2.1.from task 1
summary(theory_based_model2)
anova(backward_model2, theory_based_model2)
AIC(backward_model2, theory_based_model2) #smaller AIC of the backward_model2



###########################################################
#                                                         #
#                 Assignment Task 3                       #
#                                                         #
###########################################################

#Linear Mixed Effects

library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer

#load data set
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv")
View(data_sample_3)
attach(data_sample_3)

#describtive check
describe(data_sample_3)
names(data_sample_3)

#building histograms
hist(data_sample_3$pain1)
hist(data_sample_3$pain2)
hist(data_sample_3$pain3)
hist(data_sample_3$pain4) #the overall assumption with the residuals hold true

#data frame om wide to long format
repeated_variables = c("pain1",	"pain2", "pain3",	"pain4")
data_sample_3.1 = melt(data_sample_3, measure.vars=repeated_variables, variable.name = "time", value.name = "pain_rating") #restructure the variable names
data_sample_3.1 = data_sample_3.1[order(data_sample_3.1[,"ID"]),] 
data_sample_3.1$time = as.numeric(data_sample_3.1$time)
data_sample_3.1

# first model including the fixed effects (random intercept model)
#age, sex, weight, STAI, pain catastrophizing, mindfulness, serum cortisol, time (day of pain report)
#and a random intercept for participant ID (variable called ‘ID’ in the dataset).
mod_int = lmer(pain_rating ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + time + (1|ID), data = data_sample_3.1) #include the predictors # one and cluster variable (1|ID)
summary(mod_int)
r2beta(mod_int, method = "nsj", data = data_sample_3.1) #marginal R^2
summary
confint(mod_int)
stdCoef.merMod(mod_int) #beta
#second model containing the same fixed effects (random slope model)
#but also including a random slope over time for each participant (this model should also contian a random intercept).
mod_slope = lmer(pain_rating ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + time + (time|ID), data = data_sample_3.1) #includes the time as well - a differnt effect on time of wound healing
summary(mod_slope)
r2beta(mod_slope, method = "nsj", data = data_sample_3.1) #marginal R^2
summary
confint(mod_slope)
stdCoef.merMod(mod_slope) beta

#plotting the regression line (prediction)
data_sample_3.1$pred_int = predict(mod_int)
data_sample_3.1$pred_slope = predict(mod_slope)

#plotting random intercept model
ggplot(data_sample_3.1, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int, x=time))+
  facet_wrap( ~ ID, ncol = 5)

#plotting random slope model
ggplot(data_sample_3.1, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope, x=time))+
  facet_wrap( ~ ID, ncol = 5)

#comparing models with cAIC
cAIC(mod_int)$caic
cAIC(mod_slope)$caic

#quadratic term of time to the intercept model
mod_slope_quad = lmer(pain_rating ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + I(time^2) + (time|ID), data = data_sample_3.1)

#plotting random slope quad model
data_sample_3.1$pred_slope_quad = predict(mod_slope_quad)
ggplot(data_sample_3.1, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope_quad, x=time))+
  facet_wrap( ~ ID, ncol = 5)

#compare  with cAIC
cAIC(mod_slope)$caic
cAIC(mod_slope_quad)$caic 
#based on results it seems that the random slope model without the quadratic term of time would be the best model to choose


