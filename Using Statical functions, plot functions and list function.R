#clear all existing variables from console
cat ("\014")

#clear all data variables
rm(list = ls())

#packages needed
#install.packages("Hmisc")
#install.packages("pastecs")
#install.packages("psych")

#################### GET DATA ##########################
# import the data set into the current workspace
data <- read.csv("bank_data.csv", header = T)
###########################################################
 
#Explore your data
View(data)

names(data)

#rename first col
colnames(data)[1] <- "age"

names(data)

dim(data) #dimension (number of rows and cols)

str(data) #structure of the dataset

class(data) #type of data

head(data, n = 5) #displays the first 5 rows

tail(data, n = 5) #displays the last 5 rows

#univariate stats

#load packages
library(Hmisc)
library(pastecs)
library(psych)
library(dplyr)

#summary 
summary(data)

#choose subset if data for EDA

var <-c ("age", "job", "marital", "education")
eda_data <- select(data, var)
names(eda_data)

#Let us use the describe function now from "Hmisc" package
#----------------------------------------------------------------
# Number of rows
# standard deviation
# Trimmed mean
# Mean absolute deviation
# Skewness
# Kurtosis
# Standard error
# -----------------------------------------------------------
 describe(eda_data$age)

# stat.desc() function which is part of the pastec package
# gives us additionaly

# variance
# coefficient of variation
# confidence interval for mean
#-------------------------------------------------------

stat.desc(eda_data$age)

#graphical views : 1 histogram

hist(eda_data$age,
     main = "histogram of age",
     xlab = " age in years")

# what can we say about age group >60
#--------------------------------------------------
# graphical views : 2 box plots
#---------------------------------------------------
boxplot(eda_data$age, 
        main = toupper("Boxplot of age"),
        ylab = "age in years",
        col ="blue")
#--------------------------------------------------
# kernel density plot
#--------------------------------------------------
d <- density(eda_data$age)
plot(d, main = "kernel density of age")
polygon(d, col = "red", border = "blue")

#----------------------------------------------------
#                   Descriptive stats
#--------------------------------------------------------

#read the data file
PCData <- read.csv(file= 'PC_SALES-6.csv', header = T, sep = ',')

# Explore your data

# provides basic descriptive statitics and frequencies
summary(PCData)

# open data editor
edit(PCData)

#Provides structure of the data set
str(PCData)

# LIsts variables in data set
names(PCData)

#First few rows of the dataset
head(PCData)

#First 2 rows of the dataset
head(PCData, n = 2)

# Last 2 rows
tail(PCData, n=2)

#Rows 2 to 4
PCData[2:4, ]

#Rows 2 to 4 with columns 1 to 3
PCData [2:4, 1:3]

#Rows 2 to 4 with columns 1 and 3
PCData[2:4,c(1,3)]

#-------------------------------------------------------
#            Descriptive Statistics
# ------------------------------------------------------
#Mean of a particular column
mean(PCData$Units_Shipped_Q1_2015)

#Another way of getting the mean
with(PCData, mean(PCData$Units_Shipped_Q1_2015))

#Median
median(PCData$Units_Shipped_Q1_2015)

#Variance
var(PCData$Units_Shipped_Q1_2015)

#standard Deviation
sd(PCData$Units_Shipped_Q1_2015)

# maximum 
max(PCData$Units_Shipped_Q1_2015)

#minimum
min(PCData$Units_Shipped_Q1_2015)

#range
range(PCData$Units_Shipped_Q1_2015)

#Quantile
quantile(PCData$Units_Shipped_Q1_2015)

#number of observations
length(PCData$Units_Shipped_Q1_2015)

#which is the Maximum
PCData$Company [[which.max(PCData$Units_Shipped_Q1_2016)]]

#----------------------------------------------------------
# apply()
#----------------------------------------------------------
#clear all existing variables from console
cat ("\014")

#clear all data variables
rm(list = ls())

#create a matrix
mat1 <- matrix(rep(seq(4), 4), ncol=4)
mat1

mat2 <- matrix(rep(seq(4), 4 ), nrow=4)
mat2

mat2[2][1] <- 6
mat2
#row sums of mat1
apply(mat1, 1, sum)

#column sums of mat1
apply(mat1, 2, sum)

# Applying a user defined function across rows
# Row Sum + 2
apply(mat1, 1, function(x) sum(x) + 2)

#----------------------------------------------------------
#              lapply()
#----------------------------------------------------------
# creating a data frame using mat1
mat1.df <- data.frame(mat1)
mat1.df

#obtaining the sum of each variable in mat.df
lapply(mat1.df, sum) #note a list is return

#storing the results of the lapply function in the list y
y <- lapply(mat1.df, sum)

#verifying  that y is a list
is.list(y)

# user defined function with multiple arguments
# function defined inside the lapply function
y1 <- lapply(mat1.df, function(x, y) sum(x) + y, y = 5)
y1

#displaying first 2 rows
y1[1:2]

#----------------------------------------------------------
#             sapply()
#-----------------------------------------------------------
y2 <- sapply(mat1.df, function(x, y) sum(x) + y, y = 5)
y2

# check if vector
is.vector(y2)

#----------------------------------------------------------
#             tapply()
#-----------------------------------------------------------
# consider the inbuilt data set iris

#a sneak peek into iris
head(iris)
View(iris)

#How many species is the Data available for?
uniquespeciesCnt <- length(unique(iris$Species))
uniquespeciesCnt

#what are the different species listed
uniquespecies <-unique(iris$Species)
uniquespecies

## FInd mean sepal length in dataset iris, split the results by species
tapply(iris$Sepal.Length, iris$Species, mean)

