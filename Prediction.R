#install.packages("MASS")

library(MASS)

Birthwt<-data.frame(birthwt)

View(Birthwt)

Birthwt1 <- subset(Birthwt, birthwt$bwt < 2500)
length(Birthwt1$bwt)

Birthwt2 <- subset(Birthwt, birthwt$bwt > 2500)
length(Birthwt2$bwt)

summary(Birthwt)
str(Birthwt)

set.seed(111)                      #split the data into training and test datasets in 70:30 ratio
index <- sample(1:nrow(Birthwt), size = 0.8 * nrow(Birthwt))

train.data22 <- Birthwt[index, ]
test.data22 <- Birthwt[-index, ]

summary(train.data22)
str(train.data22)
summary(test.data22)
str(test.data22)

table(birthwt$bwt)
prop.table(table(birthwt$bwt))

normalize <- function(x){
  z <- ((x-min(x)) / (max(x)- min(x)))
  return(z)
}

a <- lapply(birthwt, normalize)
birthwt_norm <- as.data.frame(a)

summary(birthwt_norm)

library(class)

train.data33 <- birthwt_norm[1:151, ]
test.data33 <- birthwt_norm[151 : 189, ]

birthwt_train_labels <- birthwt[1:151, 8]
birthwt_test_labels <- birthwt[151:189, 8]

birthwt_pred <- knn(train = train.data33, 
                     test = test.data33,
                     cl = birthwt_train_labels,
                     k = 36)

View(birthwt_pred)

