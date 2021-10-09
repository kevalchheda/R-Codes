#clear all existing variables from console
cat ("\014")

#clear all data variables
rm(list = ls())

#install.packages("readxl")
library(readxl)
library(dplyr)

data <- read_excel("BloombergData.xlsx" )

View(data)

NA_row_count <- nrow(data) - sum(complete.cases(data))
NA_row_count

Completerows <- na.omit(data)
Completerows

class(data)

dim(data)

str(data)

head(data)

tail(data)

as.factor(data$INDUSTRY_SECTOR)
as.factor(data$INDUSTRY_GROUP)

data$CEO_START_DATE <- as.Date(data$CEO_START_DATE, format = "%d/%m/%Y")
class(data$CEO_START_DATE)
data$CEO_START_DATE

date <- grepl("[1-3000]-", data$CEO_START_DATE)
date

Newdate <- data$CEO_START_DATE[date]
Newdate

class(data$INDUSTRY_SECTOR)
class(data$CEO_START_DATE)

summary(data$CUR_MKT_CAP)
View(data)

quantile(data$CUR_MKT_CAP)

data1 <- na.omit(data) 
data1

#Summary 
 
mkt_cap_summary <- summary(data1$CUR_MKT_CAP)
mkt_cap_summary

quantile(data1$CUR_MKT_CAP)

data1$Com_Classification[data1$CUR_MKT_CAP<13064237737] <- "Lower Value"

View(data1$Com_Classification)

data1$Com_Classification[data1$CUR_MKT_CAP>1.306e+10 & data2$CUR_MKT_CAP <2.205e+10 ] <- "Middle Value"
View(data1$Com_Classification) 

data1$Com_Classification[data1$CUR_MKT_CAP >2.205e+10 ] <- "Higher Value"
View(data1$Com_Classification) 

data1 <- mutate(data1, Ous_Shares = data1$CUR_MKT_CAP/ data1$PX_LAST)
View(data)

C <- subset(data1, data1$CEO_START_DATE >= 2001)
C


aggregate(data1$CUR_MKT_CAP~data1$INDUSTRY_SECTOR+data1$Com_Classification, data1, mean )
aggregate(data1$Ous_Shares~data1$INDUSTRY_SECTOR+data1$Com_Classification, data1, mean)

class(data1$CUR_MKT_CAP)
class(data1$PX_LAST)

library(ggplot2) 

#histogram

ggplot(data1, aes(x=CUR_MKT_CAP, fill = Com_Classification)) + 
  geom_histogram(bins = 30) + labs(x = "Curreny Market Capital") + 
  facet_wrap(~ Com_Classification,  ncol = 2)

ggplot(data = data1,aes(x= PX_LAST, fill = Com_Classification))+
  geom_histogram() + labs(x="PX_Prices") + 
  facet_wrap(~ Com_Classification,  ncol = 2)

#Box plot 

z <- ggplot(data = data1,aes(CUR_MKT_CAP, Com_Classification))+
  geom_boxplot(aes(fill=Com_Classification),outlier.color = "red") 

z + labs(x = "Current Market Capital",y="Classification") + 
  facet_wrap(~ Com_Classification,  ncol = 2)


#Scatter plot 
ggplot(data = data1,aes(x=CUR_MKT_CAP, y = PX_LAST))+ 
  geom_point()+ labs(x = "Current Market Capital",y="PX_Price") + 
  facet_wrap(~ Com_Classification,  ncol = 2)

ggplot(data = data1)+ geom_point(aes(CUR_MKT_CAP,PX_LAST)) +  
  labs(x = "Current Market Capital",y="PX_Price") + facet_wrap(~ Com_Classification,  ncol = 2)


ggplot(data = data1,aes(x=CUR_MKT_CAP, y = PX_LAST, z = Com_Classification))+
  geom_point()+ labs(x = "Current Market Capital",y="PX_Price",z="Classification") + facet_wrap(~ Com_Classification,  ncol = 2)

#Bar chart 

ggplot(data = data1,aes(x=Com_Classification))+ 
  geom_bar()+ labs(x = "Classification",y="Number of Companies")
