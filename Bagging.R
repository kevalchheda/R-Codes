# Bagging and Random Forests
#install.packages("ISLR")
library(ISLR)
library(MASS)
attach(Boston)
str(Boston)
library(randomForest)
train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test= Boston[-train, "medv"]
set.seed(1)
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston
yhat.bag = predict(bag.boston, newdata= Boston[-train,])
plot(yhat.bag, boston.test, xlab = "Predicted", tlab = "Actual")
albine(0,1)
mean((yhat.bag-boston.test)^2)
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, importance = TRUE)
#mtry is the number of predictors used in the randomForest function. 
#In Bagging all the variables are used as predictors. So randomForest with all variables as predictors acts as bagging
bag.boston
yhat.bag.Ran = predict(bag.boston, newdata= Boston[-train,])
plot(yhat.bag.Ran, boston.test, xlab = "Predicted", ylab = "Actual")
albine(0,1)
mean((yhat.bag.Ran-boston.test)^2)
set.seed(1)
rf.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, importance = TRUE)
yhat.rf = predict(rf.boston, newdata= Boston[-train])
plot(yhat.rf, boston, xlab = "Predicted", ylab = "Actual")

albine(0,1)

mean((yhat.rf-boston.test)^2)
importance(rf.boston)
#From importance we get Nodepurity
#We come to know that from which variable the node is splitting
#Here we can see that lowerstatus(lstat) has higher node purity so we can take that as first split
# The rm tells us about the number of rooms in the data set. It should be high as possible
# The distance is the third split 
varImpPlot(rf.boston)
# We can conclude that incremental Mean square error(%IncMSE)
# Incremental Node purity both depends on the rm and lstat values
  

