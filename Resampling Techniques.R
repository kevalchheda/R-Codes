library(dplyr)
library(boot)
library(ggplot2)

#Read Data
Auto <- read.csv("Auto.csv", sep = ",", header = T)

glimpse(Auto) 

summary(Auto)

plot(Auto$displacement, Auto$mpg)

# Convert factors to numeric

Auto$horsepower <- as.numeric(Auto$horsepower)

#Fit linear + polynomial regression with R
ggplot(Auto, aes(displacement,mpg)) +
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)+ 
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se = FALSE, linetype = 2, color = "red") +
  geom_smooth(method = 'lm', formula = y ~ poly(x,3), se = FALSE, linetype = 3, color = "green") +
  scale_color_manual(values = c("red", "green", "blue"))

set.seed(1) #itertion 1

train = sample(392,196) #splitting the training (50%) + test data

# Fit the model of linear regression with a fixed training data

lm.fit = lm(mpg~displacement,data = Auto, subset = train)

attach(Auto)

#Obtain the test error that is mean squared error 
mean((mpg~predict(lm.fit, Auto))[-train]^2)

# Fit the model of polynomial regression(degree 2) with a fixed training data

lm.fit2 = lm(mpg~poly(displacement,2),data = Auto, subset=train)

#Obtain the test error, that is mean squared error
mean((mpg~predict(lm.fit2, Auto))[-train]^2)

#If the mean square error drops that means the model accuracy has increased

# Fit the model of polynomial regression(degree 3) with a fixed training data

lm.fit3 = lm(mpg~poly(displacement,3),data = Auto, subset=train)

#Obtain the test error, that is mean squared error
mean((mpg~predict(lm.fit3, Auto))[-train]^2)

set.seed(2) #iteration 2

train = sample(392,196) # Splitting the training (50%) + test data 

lm.fit = lm(mpg~displacement, subset = train)

#Obtain test error, that is mean squared error

mean((mpg~predict(lm.fit, Auto))[-train]^2)

# Fit the model of polynomial regression(degree 2) with a fixed training data

lm.fit2 = lm(mpg~poly(displacement,2), subset=train)

#Obtain the test error, that is mean squared error
mean((mpg~predict(lm.fit2, Auto))[-train]^2)

#If the mean square error drops that means the model accuracy has increased

# Fit the model of polynomial regression(degree 3) with a fixed training data

lm.fit3 = lm(mpg~poly(displacement,3), subset=train)

#Obtain the test error, that is mean squared error
mean((mpg~predict(lm.fit3, Auto))[-train]^2)

#Iteration based on Probability

set.seed(1) # iteration 2
sample <- sample(c(TRUE, FALSE), nrow(Auto), replace =  T, prob = c(0.6, 0.4))

train <- Auto[sample, ]
test <- Auto[sample, ]

# Optional Code

# loop for first ten polynomial

mse.df <- tibble(degree = 1:10, mse = NA)

for(i in 1:10) {
  lm.fit <- lm(mpg ~ poly(displacement, i), data = train)
  mse.df[i,2] <- mean((test$mpg ~ predict(lm.fit, test))^2)
}

ggplot(mse.df, aes(degree, mse)) +
  geom_line()+
  geom_point()+
  ylim(c(10,30))

mse.df.2<- tibble(sample = vector("integer",100),
                  degree = vector("integer", 100),
                  mse = vector("double", 100))


# Cross Validation

#LOOCV : LEave one out cross validation

# step 1 : # Step 1 fit the linear model

glm.fit <- glm(mpg~displacement, data = Auto)

#Step 2 : perform LOOCV across entire data set

loocv.err <- cv.glm(Auto, glm.fit)

str(loocv.err)

loocv.err$delta[1]

#Create function that computes LOOCV specification based on specified poly
loocv_error <- function(x){
  glm.fit <- glm(mpg~poly(hoursepower,x), data = Auto)
  cv.glm(Auto, glm.fit)$delta[1]
}

# compute LOOCV MSE for polynomial degrees 1-5
library(purrr)
1:5 %>% map_dbl(loocv_error)

# K fold

# Step 1 : fir linear model

glm.fit <- glm(mpg ~ displacement, data = Auto)

# Step 2 : Perform LOOCV across entire data set

kfcv.err <- cv.glm(Auto, glm.fit, K= 10)

str(kfcv.err)

kfcv.err$delta[1]

# Bootstrap 

statistic <- function(Auto, index){
  lm.fit <- lm(mpg ~ displacement, data = Auto, subset = index)
  coef(lm.fit)
}

set.seed(123)

summary(lm(mpg~displacement, data = Auto))

statistic(Auto, 1:392)

library(boot)

set.seed(123)

boot(Auto, statistic = 1000) #Bootstrap with 1000 replicates

quad.statistic <- function(Auto, index) {
  lm.fit <- lm(mpg ~ poly(displacement,2), data = Auto, subset = index)+
  coef(lm.fit)
}

set.seed(1)

boot(Auto, quad.statistic, 1000)
