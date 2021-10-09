library(datasets)
data(mtcars)
data1 <- mtcars
View(data1)
data2 <- data.frame(data1)

model1 <- lm(mpg ~ disp + hp + wt, data2)
model1

summary(model1)