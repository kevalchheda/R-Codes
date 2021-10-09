bankloan <-  read.csv("bank_data.csv", header = T, sep = ",")

View(bankloan)
is.na(bankloan)
#There is no NA in the dataset
summary(bankloan)
str(bankloan)

set.seed(111)                      #split the data into training and test datasets in 70:30 ratio
index <- sample(1:nrow(bankloan), size = 0.8 * nrow(bankloan))

train.data22 <- bankloan[index, ]
test.data22 <- bankloan[-index, ]

summary(train.data22)
summary(test.data22)

table(bankloan$DEFAULTER)
prop.table(table(bankloan$DEFAULTER))
min(x)
max(x)
#Scale all th variable values in one range
normalize <- function(x){
  z <- ((x - min(x)) / (max(x)- min(x)))
  z(digits=2)
  return(z)
}


listdata <- lapply(bankloan, normalize)
bankloan_norm <- as.data.frame(listdata)

summary(bankloan_norm)



#install.packages("class")
library(class)

train.data33 <- bankloan_norm[1:560, ]
test.data33 <- bankloan_norm[560 : 700, ]

bankloan_train_labels <- bankloan[1:560, 8]
bankloan_test_labels <- bankloan[560:700, 8]

bankloan_pred <- knn(train = train.data33, 
                     test = test.data33,
                     cl = bankloan_train_labels,
                     k = 36)

bankloan1 <- as.data.frame(bankloan_pred)
View(bankloan_pred)

#install.packages("gmodels")
library(gmodels)

CrossTable(x= bankloan_test_labels,
           y = bankloan_pred, prop.chisq = FALSE,
           prop.r = FALSE, prop.c = FALSE)

#We are getting 100% of accuracy

bankloan_zscore <- as.data.frame(scale(bankloan[-8]))
summary(bankloan_zscore)

train.data33$DEFAULTER <- bankloan_train_labels
test.data33$DEFAULTER <- bankloan_test_labels

#install.packages("caret")
library(caret)

trctrl <- trainControl(method = "cv", number = 3)
set.seed(123)
knn_fit <- train(DEFAULTER~ . , data = train.data33, method= "knn", trControl = trctrl, preProcess = c("center", "scale"))

knn_fit
plot(knn_fit)

knn_pred <- predict(knn_fit, newdata = test.data33)
View(knn_pred)
CrossTable(x= bankloan_test_labels, y = knn_pred,
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE)

#install.packages("randomForest")
library(randomForest)
#install.packages("e1071")
library(e1071)
#install.packages("ROCR")
library(ROCR)

##Convert to factors
convert.to.factors <- function(df, v){
  for(variable in v){
    df[[variable]] <- as.factor(df[variable])
  }
  return(df)
}

#To convert all variables into data frame. imp step for decision trees

n <- names(bankloan)
n <- c('SN', 'AGE', 'EMPLOY', 'ADDRESS', 'DEBTINC',
       'CREDDEBT', 'OTHDEBT', 'DEFAULTER')

bankloan<- convert.to.factors(bankloan, n)
bankloan

caret::confusionMatrix(data = bankloan_pred, reference = bankloan_test_labels,
                       positive = "1")
str(bankloan_pred)
bankloan_test_labels <- as.factor(bankloan_test_labels)
