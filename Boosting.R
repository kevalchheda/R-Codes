#Boosting 

#install.packages("gbm")
library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data = Boston[train,], distribution = "gaussian", interaction.depth = 4)
                 
summary(boost.boston)
par(mfrow = c(1,2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")
yhat.boost = predict(boost.boston, newdata= Boston[-train], n.trees = 5000)
graphics.off()
plot(yhat.boost, boston.test, xlab = "Predicted", ylab = "Actual")
abline(0,1)
mean((yhat.boost-boston.test)^2)
boost.boston=gbm(medv~.,data = Boston[train,], distribution = "gaussian", interaction.depth = 4)
yhat.boost = predict(boost.boston, newdata = Boston[-train], n.trees = 5000, interaction.depth = 4, shrinkge= 0.2, verbose = F)
mean((yhat.boost-boston.test)^2)

