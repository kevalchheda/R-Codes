
Data1 <- read.csv("bodyfat-reduced.csv")
head(Data1)

cor(Data1)

#Scatter PLot Matrix
#install.packages('car')
library(car)
scatterplotMatrix(Data1,spread = FALSE, smoother.args = list(lty=2),
                  main="Scatter Plot Matrix (Data1)")

#Scatter plots and correlation
library(psych)
pairs.panels(Data1, 
             gap = 0,
             psh = 21)

#Defining Fit : Multilinear regression model
attach(Data1)

fit <- lm(BodyFat ~ Weight + Chest + Abdomen + Hip + Thigh + Biceps, data = Data1)

summary(fit) #Summary of the regression

confint(fit) # Confidence interval

#Plot the regression summary 
par(mfrow = c(2,2))
plot(fit)

graphics.off()

#Normality  to understand the residual distribution
library(car)
qqPlot(fit,labels=row.names(Data1), id.method = "identify",
       simulate = TRUE, main = "Q-Q Plot")

#Component + Residual Plots (linearity)

crPlots(fit)


#Homosecdasticity (Non constant error variance)

ncvTest(fit)

spreadLevelPlot(fit)

#Global test of linear model assumptions

#install.packages('gvlma')
library (gvlma)

gvmodel <- gvlma(fit)

summary(gvmodel)

# Evaluate Multicollinearity

vif(fit)

sqrt(vif(fit)) > 4  #This function tells us whether multicollinearity exists or not

#Identifying outliers / influential observations

outlierTest(fit)

#Added - variable plots
avPlots(fit, ask = FALSE, id.method = 'identify')

# Influence plot

influencePlot(fit, id.method = 'idenntify', main = "Influence Plot",
              sub = "Circle size is proportionate to Cook's distance")

#Model comparison

# Comparing nested models using anova() function

attach(Data1)

fit1 <- lm(BodyFat~ Weight +Chest + Abdomen + Hip + Thigh + Biceps , data = Data1)

fit2 <- lm(BodyFat~ Weight + Abdomen, data = Data1)

anova (fit1, fit2)

# As we can see model 2 not got a very significant outcome of P value
# SO model 2 is best model for this data

# Comparing models with the Aikaike Information Criteration (AIC)

fit1 <- lm(BodyFat~ Weight +Chest + Abdomen + Hip , data = Data1)

fit2 <- lm(BodyFat~ Weight +Chest, data = Data1)

AIC(fit1, fit2)
# As we can see that the fit  has the degree of freedom of 6 
#and the fit 2 has the degree of freedom of 4
#So fit 2 is performing better. 

# Backward Stepwise selection

library(MASS)

attach(Data1)

fit <- lm(BodyFat~ Weight + Abdomen , data = Data1)

stepAIC(fit, direction = "backward")

# All subsets regression
#install.packages("leaps")
library(leaps)

attach(Data1)

leaps <- regsubsets(BodyFat~ Weight +Chest + Abdomen + Hip ,
                    data = Data1, nbest = 4)

plot(leaps, scale = 'adjr2', main = "Selecting the BEST regression")

subsets(leaps,statistic = "cp", 
        main = 'Cp plot for all subsets regression')
abline(1,1,lty =2, col = "red")

