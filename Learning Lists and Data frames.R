#clear all existing variables from console
cat ("\014")

#clear all data variables
rm(list = ls())

# Slicing vectors

vec1 <- c(12,4,7,6,6,9,2)

#extract 5th element
fifth_ele <- vec1[5]

fifth_ele

#define a matrix

A <- matrix(c(9,6,3,5,6,2), nrow = 2, ncol = 3, byrow = TRUE)

A

#extracting an element
A23 <- A[2,3]
A23

#extracting a sub matrix

A2B2 <- A[1:2, 1:2]
A2B2

#slicing data frames
n <- c(2,3,5)
s <- c("aa", "bb", "cc")
b <- c(TRUE, FALSE, TRUE)

df <- data.frame(n,s,b)

df

#extract element
df[1,2]

#extract 1st row
df[1,]


#using subset feature

#create building blocks
make <- c("toyota", "hyundai", "maruti", "tata", "hyundai")
model <- c("corolla", "i20", "alto", "manza", "i10")
sales <- c(333, 484, 126, 346, 891)

#create data frame
cardata <- data.frame(make, model, sales)

#Topsales <- subset(cardata, sales > 400, select = c(make, model, sales))
topsales <- subset (cardata, sales>300)

# sort
Sorted_topsales <- topsales[order(-topsales$sales),]

Sorted_topsales

#using the which function to subset

# Get index/row positions meeting any particular criteria
idx_pos <- which(cardata$sales>300)

#get the subset using index positions
carsale_gt_300 <- cardata [idx_pos, ]
carsale_gt_300

#get the subset for which sales are less than 300
carsale_lt_300 <- cardata [-idx_pos, ]
carsale_lt_300

#get the model name for the max sale value
max_sale_model <- cardata$model[which.max(cardata$sales)]
max_sale_model
which.max(cardata$sales)

#----------------IMPORTANT verbs in DPLYR Package---------------------------------
#install.packages("dplyr")
library(dplyr)

#the <select> verb

#selecting models column from cardata
car_models <- select(cardata, model)
car_models

#the <filter> verb

#select rows for which have sales400
sales_gt_400 <- filter(cardata, sales >400)

sales_gt_400

#the <arrange> verb

#sort cardata in descending order of highest sales
sorted_cardata <- arrange (cardata, desc(sales))
sorted_cardata

#the <mutate> verb

#create new column - perc_tot_sales
cardata <- mutate (cardata, perc_tot_sales = (100*sales)/(sum(sales)))
cardata

# the <summarise> verb

#get the summary statistics
sale_summary <- summarise(cardata, avg_sales = mean(sales),
          minsales = min(sales),
          maxsales = max(sales),
          totalsales = sum(sales))
          
sale_summary

# the <group_by> verb
# get summary statistics average sales by car manufacturer
mf_avg_sales <- cardata %>% group_by(make) %>% summarise(avg_sales = mean(sales))
mf_avg_sales

#----------------- Covert betwn data types--------------#

df4 <- as.data.frame(c("ashish", "john", "kumar"))
df5 <- as.data.frame(c("ashish2", "john2", "kumar2"))
df6 <- as.data.frame(c("ashish3", "john3", "kumar3"))

#covert data frames to list 

#define list with 3 elements
my.list <- vector("list", 3)

#populate list with data frames
my.list [[1]] <- list(df4)
my.list [[2]] <- list(df5)
my.list [[3]] <- list(df6)

my.list

#now convert this list to data frame
finalldf <- as.data.frame(my.list)
finalldf

#set coumn names
names(finalldf) <- c("col1", "col2", "col3")
finalldf
 

