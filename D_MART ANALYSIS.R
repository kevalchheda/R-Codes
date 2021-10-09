#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("dplyr")
library(tidyverse)
library(lubridate)
library(arules)
library(arulesViz)
library(dplyr)
library(ggplot2)
library(readxl)

retail <- read_excel('D_Mart_Mine.xlsx')
retail
retail <- retail[complete.cases(retail), ]

retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(Country = as.factor(City))
retail$Date <- as.Date(retail$InvoiceDate) 
retail$Time <- format(retail$InvoiceDate, "%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)

#------------------------------------------------------------
# Time Vs Sales

retail$Time <- as.factor(retail$Time)

ggplot (data = retail, aes(x= retail$Time))+ 
labs(x = "Time") + geom_bar()

# No. of items per Invoice 

retail$Quantity
retail %>%
group_by(InvoiceNo) %>%
summarize(n_items = (Quantity)) %>%
ggplot(aes(x = n_items)) + labs(x = "No. of Items")+ 
geom_histogram(fill = "indianred", bins = 10) + 
geom_rug()+ coord_cartesian(xlim = c(0,80))

# Best Seller

retail %>% group_by(StockCode, Description) %>%
arrange(desc(Quantity))

retail %>% group_by(StockCode, Description) %>%
summarize(count = n(Quantity)) %>%
arrange(desc(count))

retail %>%
ggplot(aes(x = reorder(Description, Quantity), y= count)) +
geom_bar(stat = "identity", fill = "indian red") + 
coord_flip()

# Association Rules
retail
retail_sorted <- retail[order(retail$CustomerID)]
retail_sorted
library(plyr)
itemList <- ddply(retail, c("CustomerID", "Date"),
                  function(df1)paste(df1$Description,
                                     collapse = ','))
itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c('items')

write.csv(itemList, "market_basket.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('market_basket.csv', format = 'basket', sep = ',')
tr
summary(tr)

itemFrequencyPlot(tr, topN = 20, type = 'absolute' )
rules <- apriori(tr, parameter = list(supp = 0.01, conf = 0.7))
rules <- sort(rules, by = 'confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1])