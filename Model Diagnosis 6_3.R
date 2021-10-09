#Writing state.x77 dataset as states. Built in dataset of R
states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy",
                                    "Income","Frost")])
head(states)

#Correlation between variables
cor(states)

#Scatter PLot Matrix
#install.packages('car')
library(car)
scatterplotMatrix(states,spread = FALSE, smoother.args = list(lty=2),
main="Scatter Plot Matrix (States)")

#Scatter plots and correlation
library(psych)
pairs.panels(states, 
             gap = 0,
             psh = 21)

#Defining Fit : Multilinear regression model
attach(states)

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)

summary(fit) #Summary of the regression


#Multipled R square = 0.567 that means this model accounts for 
#57% of variance in the data 

confint(fit) # Confidence interval

#Income and the frost has negligible effect on the murder variable

#Plot the regression summary 
par(mfrow = c(2,2))
plot(fit)

graphics.off()

#Normality  to understand the residual distribution
library(car)
qqPlot(fit,labels=row.names(states), id.method = "identify",
       simulate = TRUE, main = "Q-Q Plot")

#Component + Residual Plots (linearity)

crPlots(fit)


#Homosecdasticity (Non constant error variance)

ncvTest(fit)
#P value is 0.18632 states that there is no significant variance between the data variables

spreadLevelPlot(fit)

# Based on Spread level Plot we can identify Power transformation
#If the power transfromation is in the range of 0.5 then it is better to have a sqaure root in the variable
# Here it is 1 that means the model is good for this data

#Global test of linear model assumptions

#install.packages('gvlma')
library (gvlma)

gvmodel <- gvlma(fit)

summary(gvmodel)

# Evaluate Multicollinearity

vif(fit)

sqrt(vif(fit)) >2 #This function tells us whether multicollinearity exists or not

#Identifying outliers / influential observations

outlierTest(fit)

#Added - variable plots
avPlots(fit, ask = FALSE, id.method = 'identify')

# Influence plot

influencePlot(fit, id.method = 'idenntify', main = "Influence Plot",
              sub = "Circle size is proportionate to Cook's distance")

#Model comparison

# Comparing nested models using anova() function

attach(states)

fit1 <- lm(Murder~ Population +Illiteracy + Income + Frost , data = states)

fit2 <- lm(Murder~ Population +Illiteracy, data = states)

anova (fit1, fit2)

# As we can see model 2 not got a very significant outcome of P value
# SO model 2 is best model for this data

# Comparing models with the Aikaike Information Criteration (AIC)

fit1 <- lm(Murder~ Population +Illiteracy + Income + Frost , data = states)

fit2 <- lm(Murder~ Population +Illiteracy, data = states)

AIC(fit1, fit2)
# As we can see that the fit  has the degree of freedom of 6 
#and the fit 2 has the degree of freedom of 4
#So fit 2 is performing better. 

# Backward Stepwise selection

library(MASS)

attach(states)

fit <- lm(Murder~ Population +Illiteracy + Income + Frost , data = states)

stepAIC(fit, direction = "backward")

# All subsets regression
#install.packages("leaps")
library(leaps)

attach(states)

leaps <- regsubsets(Murder~ Population +Illiteracy + Income + Frost ,
                    data = states, nbest = 4)

plot(leaps, scale = 'adjr2', main = "Selecting the BEST regression")

subsets(leaps,statistic = "cp", 
        main = 'Cp plot for all subsets regression')
abline(1,1,lty =2, col = "red")

