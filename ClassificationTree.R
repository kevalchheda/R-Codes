library(tree)
library(ISLR)
attach(Carseats)
head(Carseats)
High = ifelse(Sales<= 8.00, "No", "Yes")
Carseats = data.frame(Carseats, High)
Carseats
tree.carseats = tree(High~. -Sales, Carseats)
tree.carseats
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats
set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats = tree(High ~. -Sales, Carseats, subset = train)
tree.pred = predict(tree.carseats, Carseats.test, type= 'class')
table(tree.pred, High.test)
(86+57)/200
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow= c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
(94+60)/200
table(tree.pred, High.test)
(86+62)/ 200
graphics.off