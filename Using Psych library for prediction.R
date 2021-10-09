data("mtcars")
cars <- mtcars
View(mtcars)

library(dplyr)
library(ggplot2)

#install.packages("psych")

library(psych)

pairs.panels(cars[c("mpg", "hp")])
plot(cars[c("mpg" , "hp")])

summary(cars)
cor(cars[c("mpg", "hp")])

hist(cars$mpg)                #it shows dependent variable is slightly skewed
shapiro.test(cars$mpg)        #checking mpg column for normal distribution using shapiro wilk test

#here we can see that the correlation is not significant and
# there is negative corelation in hp and mpg i.e as hp increases mpg decreases
dev.off()
options(scipen = 0)
cars_model_1 <- lm(mpg ~ hp, data = cars)
summary(cars_model_1)

#find the confidence interval of the fit----
confint(cars_model_1)

#mpg =  ????0 + ????1*hp
#As we can see that the F statistic - P value is very small i.e 0.05
#We will reject the null hypothesis that all ???? are zeros.
#????1 has some significant value

#Now we will see the p value of individual parameters
#The P value of hp is again less than 0.05 that means hp is statistically significant
#It has 3 astrics so it is highly significant
#Similarly intercept is also very significant
#By seeing the estimate we can say that with 1 unit increase in
#hp, the mpg decreases by 0.06 units

#By seeing the residuals we can state that
#The model has underpredicted mpg by 8 .23 units
#50% of the errors are between -2.112 and 1.5819
#Also the majority of the predictions were above 1st quartile(-2.112) and under 3rd quartile(1.5819)
plot(cars_model_1)

testdata <- data.frame(hp = c(126, 167, 189, 211 , 272, 312))
predictions<- predict(cars_model_1, newdata =testdata)
predictions

#Hence we have obtained the predicted values of mpg with the given values of hp

cor(cars[c("mpg", "hp", "wt")])
pairs.panels(cars[c("mpg", "hp", "wt")])
plot(cars[c("mpg" , "hp", "wt")])



dev.off()
options(scipen = 0)
cars_model_2 <- lm(mpg ~ hp + wt, data = cars)
summary(cars_model_2)
#Here we can see that the  ????0 (Y - intercept) = 49.808 and it statistically significant
#The p values for ????1 , ????2(slope estimate) is less than 0.05. Therefore they are non zero
#Also the 3 stars indicate that the ????2 is most significant 
#And the 2 stars indicate that ????1 is less significant than ????2
#The value of ????1 and ????2 are both negative i.e they both decrease with increase in mpg
#The value of R^2 is 82.68% which is good but can be made better using interaction technique

#find the confidence interval of the fit----
confint(cars_model_2)

cars_model_3 <- lm(mpg ~ hp + wt + wt*hp, data = cars)
summary(cars_model_3)

#find the confidence interval of the fit----
confint(cars_model_3)
# The ????1 has become more significant as it gets 3 stars
#The value of R^2 has increased from 82.68% to 88.48% which is good for the model
#Also the Adjusted R-squared value is increased
#The P value of F statistic has also decreased.
#HEnce the model with interaction is better than the normal model and will perform better

testdata1 <- data.frame( hp = c(126, 167, 189, 211 , 272, 312) , wt = c(1.5, 2.2, 2.9, 3.2, 3.8, 4.2))
predictions1<- predict(cars_model_3, newdata = testdata1)
predictions1


#Hence we obtain the predicted values


