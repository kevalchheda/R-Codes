#install.packages("readr")

library(readr)
library(dplyr)
library(ggplot2)
library(forecast)

#read sales data file
sales<- MonthlySales
View(sales)
str(sales)
head(sales, n=5)

#plotting sales versus time
options(repr.plot.width = 6, repr.plot.height = 3)
ggplot(sales, aes(x = month, y = sales)) + geom_line()+ geom_smooth(method = 'lm') +labs(x= "Time", y ="Monthly sales")

#convert our sales data to a time series ob;
salesTS <- ts(sales$sales, frequency = 12, sstart = c(2013,1))
class(salesTS)

#log transform time series data
saleslog <- log(salesTS)

saleslogHW <- HoltWinters(saleslog)
saleslogHW

options(repr.plot.width = 6, repr.plot.height = 4)
plot(saleslogHW)

#Forecast next years sales
nextYearSales <- forecast(saleslogHW, h=12)
nextYearSales

#Plot
plot(nextYearSales)
