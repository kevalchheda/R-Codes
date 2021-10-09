data(mtcars)
data1 <- data.frame(mtcars)
View(mtcars)

data1$mpg <- as.factor(data1$mpg)
class(data1$mpg)
model.1 <- glm(mpg ~ . , data1, family = "binomial")
model.1
summary(model.1)

#Residual deviance is lower than the Null deviance. this is a good indiction.
#Balance variable has the high significant variable based on P value
#median value is close to ZERO. this is a good indiction for this model
#Number of Fisher Scoring iterations: 25 means, model has converzed to come to a solution by to doing 25 iterations