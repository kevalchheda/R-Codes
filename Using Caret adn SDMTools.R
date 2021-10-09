credit <- read.csv("CreditRev.csv", header = TRUE, sep = ",")
#install.packages("caret")
#install.packages("SDMTools")
#install.packages("http://cran.r-project.org/src/contrib/Archive/RNetLogo/RNetLogo_0.9-6.tar.gz", repo=NULL, type="source")
View(credit)
summary(credit)

boxplot(credit$balance)
#To check outliers

plot(credit)
#There is no correlation

#separate feature and class variable
test.feature.var <- credit[, -4]    #independent variables
head(test.feature.var)

test.class.var <- credit[, 4]       #target variable
head(test.class.var)


model.1 <- glm(default1 ~ student, train.data, family = "binomial")
model.1

model.2 <- glm(default1 ~ balance, train.data, family = "binomial")
model.2

model.3 <- glm(default1 ~ Age, train.data, family = "binomial")
model.3

model.4 <- glm(default1 ~ income, train.data, family = "binomial")
model.4

#Identify the B0 (Y - intercept) + B1(slope estimate)-----
model.1
model.2
model.3
model.4

#Use the summary values of p, AIC, etc.. to diagnose the model----
summary(model.1)
summary(model.2)
summary(model.3)
summary(model.4)

#1) Residual deviance is lower than the Null deviance. this is a good indiaction.
#2) Balance variable has the high significant variable based on P value
#3) median value is close to ZERO. this is a good indiction for this model
#3) Number of Fisher Scoring iterations no indicates, model has converzed to come to a solution.

#find the confidence interval of the fit----
confint(model.1)
confint(model.2)
confint(model.3)
confint(model.4)

#Compare the first four model of this assignment with a  multi logistic regression model of Default Vs Student + Balance + Age + Income----
lr.prediction <- glm(default1 ~ student + balance + Age + income, train.data, family = "binomial")

summary(lr.prediction)             #view model details

model <- c("model.1", "model.2", "model.3", "model.4", "lr.prediction")
model

AIC <-
  c(model.1$aic,
    model.2$aic,
    model.3$aic,
    model.4$aic,
    lr.prediction$aic)

deviance <-
  c(
    model.1$deviance,
    model.2$deviance,
    model.3$deviance,
    model.4$deviance,
    lr.prediction$deviance
  )

compare.models <- data.frame(model, AIC, deviance)   #Compare
compare.models


#multi logistic regression model has "lower AIC value & lower deviance value" compared to first four models.
#this indicates, multi logistic regression model has good performance and giving best results

#Residual deviance is lower than the Null deviance. this is a good.
#Balance variable has the high significant variable based on P value
#median value is close to ZERO. this is a good for this model

#find the confidence interval of the fit----
confint(lr.prediction)

#Predict the outcome (whether the person will default or not) using student + balance + age + income based on classification----
#with reference to the multi Logistic Regression model.
options(scipen = 999)

test.data2 <-
  data.frame(
    student = c("No", "Yes", "Yes", "No", "No", "No"),
    balance = c(1500, 1000, 2000, 2500, 1600, 1900),
    Age = c(34, 82, 71, 36, 68, 77),
    income = c(10000, 18000, 21000, 37000, 40000, 24000)
  )

summary(test.data2)

lr.prediction <- predict(lr.prediction, test.data2, type = "response")
lr.prediction


default <- ifelse(lr.prediction < 0.5, "No", "Yes")
default

test.data2$default <- cbind(default)

summary(test.data2)
test.data2


