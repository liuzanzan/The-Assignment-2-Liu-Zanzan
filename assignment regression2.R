#########################
#   backward regression #
#########################

library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(cAIC4) # for cAIC
library(r2glmm) # for r2beta
library(influence.ME) # for influence
library(lattice) # for qqmath
library(reshape2) # for melt function

#data
mydata = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")

### check data set for invalid data (e.g. coding errors)
# descriptive statistics
summary(mydata)


# according to the summary, there is an participant whose sex is "3", and an participant whose mindfulness value is negative. 
# use which function to find these participants and get rid of them
which(mydata$sex=="3")
which(mydata$mindfulness<0)
mydata1=mydata[-c(15,24),]
summary(mydata1)

# model 3
mod_pain3 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness 
                + weight, data = mydata1)
summary(mod_pain3)
AIC(mod_pain3)

### Model diagnostics
# Fit the final model 
mod_pain3 = lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum  + mindfulness + weight
               , data = mydata1)

# checking for influential outliers
plot(pain ~ factor(sex), data = mydata1)
plot(pain ~ age, data = mydata1)
abline(lm(pain ~ age, data = mydata1))
plot(mod_pain3, which = 4)
plot(mod_pain3, which = 5)

### checking assumptions
# normality assumption
# QQ plot
plot(mod_pain3, which = 2)
# skew and kurtosis
describe(residuals(mod_pain3))
# histogram
hist(residuals(mod_pain3), breaks = 20)

# linearity assumption
# predicted values against actual values
pred <- predict( object = mod_pain3 )
plot( x = pred, y = mydata1$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_pain3, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_pain3)

# homoscedasticty assumption (homogeneity of variance)
plot(mod_pain3, which = 3)
ncvTest(mod_pain3)



##############################
#    Backward regression     #
##############################
###which is the best predictor 
names(mydata1)

# there is some multicollinearity
vif(mod_pain3)


mod_pain3_back = step(mod_pain3, direction = "backward")
summary(mod_pain3_back)





###############################################################################################
backward_model=lm(formula = pain ~ age + pain_cat + cortisol_serum + mindfulness, 
                  data = mydata1)
theory_based_model = lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness 
                      , data = mydata1)
summary(backward_model)
summary(theory_based_model)
#the 95% CI 
confint(backward_model)
#beta
lm.beta(backward_model)
### Comparing models
## first, compare the initial model with the backward model
AIC(mod_pain3)
AIC(backward_model)
anova(backward_model, mod_pain3)

# compare with AIC
AIC(backward_model)
AIC(theory_based_model)

#anova
anova(backward_model, theory_based_model)


# training set 1
backward_model_train = lm(pain ~ age + pain_cat + cortisol_serum + mindfulness, 
                          data = mydata1)
summary(backward_model_train)

# training set 2
theory_based_model_train = lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness 
                              , data = mydata1)
summary(theory_based_model_train)

pred_backward_train = predict(backward_model_train)
pred_theory_train= predict(theory_based_model_train)
RSS_backward_train = sum((mydata1[1:158,"pain"] - pred_backward_train)^2)
RSS_theory_train = sum((mydata1[1:158,"pain"]  - pred_theory_train)^2)
RSS_backward_train
RSS_theory_train

# check model performance on the test set
test_data=read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv")
pred_backward_test <- predict(backward_model_train, test_data)
pred_theory_test <- predict(theory_based_model_train, test_data)
# calculate the sum of squared residuals 
RSS_backward_test = sum((test_data[1:160,"pain"] - pred_backward_test)^2)
RSS_theory_test = sum((test_data[1:160,"pain"] - pred_theory_test)^2)
RSS_backward_test
RSS_theory_test









