#----------------------------------------------------------#

# import the data

housing <- read.csv('housing.csv', sep = ",", header = T)

#----------------------------------------------------------#



# Data visualisation using Base R Graphics

#----------------------------------------------------------#

#----------------------------------------------------------#

#subset data based on strings
# select = c(1:13) - is the selection of rows

h_bronx <- subset(housing, State == "Bronx", select = c(1:13))

h_brooklyn <- subset(housing, State == "Brooklyn", select = c(1:13))

#----------------------------------------------------------#

# par(mfrow=c(2,1))

#histogram (probability) : projects across Bronx & Brooklyn.
#similarly subset data for other states and plot


hist(h_bronx$Year.Built, xlab = "Year"
     , main ="Housing Projects in Bronx")

hist(h_brooklyn$Year.Built, xlab = "Year"
     , main ="Housing Projects in Brooklyn")

#In base R graphics we first have to subset the data and then compare
#The main disadvantage is that it does not compare it with scale
#The scaling is different so it is hard to differentiate the difference

#----------------------------------------------------------#



#load the library dplyr & ggplot2

library(dplyr)

# Data visualisation using ggplot2

library(ggplot2)



# The main advatage of ggplot2 is that it keeps a standardise scale 
#So we can compare the data more easily
#----------------------------------------------------------#
#glimpse of data

glimpse(housing)

View(housing)
#----------------------------------------------------------#

#remove duplicates

housing <- distinct(housing)

#----------------------------------------------------------#

# for changing names

fix(housing)
#----------------------------------------------------------#

#select Variables: Year, Total Units, Expense, MarketValue, Building, State

housing %>% select(Building.Classification, Total.Units, Year.Built,
                   Expense.per.SqFt, Market.Value.per.SqFt,State)

#----------------------------------------------------------#

#histogram (probability) : projects across Various states

ggplot(housing, aes(Year.Built, fill = State))+
geom_histogram(binwidth = 2) + ggtitle("Total Units built") + xlab("Year")+
facet_wrap(~ State,  ncol = 2)
#----------------------------------------------------------#

#frequencypolygon (density) : number of Housing projects

ggplot(housing, aes(Year.Built, color = State))+
geom_freqpoly(binwidth = 5) + labs(title = "Housing Projects across various States", x ="Year")+
facet_wrap(~ State,  ncol = 2)

#----------------------------------------------------------#

#boxplot : total units built

#----------------------------------------------------------#

#Good practice during analytics is to create an object p 

#and then add layer

#----------------------------------------------------------#
# run this code first to create object p

p <- ggplot(data = housing, aes(Year.Built, Total.Units))+ 
  geom_boxplot(aes(fill =State),outlier.colour = "red")  
p
#----------------------------------------------------------#

p + ggtitle("Total Units built") + xlab("Year") + ylab("Units Built")+
facet_wrap(~ State,  ncol = 2)

#----------------------------------------------------------#

#barplot : Total Units Vs State + Building.Classification

ggplot(data = housing) + 
stat_count(mapping = aes(x = State, fill = Building.Classification)
, position = "dodge" ) 

#----------------------------------------------------------#


# boxplot : compare Expense per square feet (across various states)

# run this code first to create object p

p <- ggplot(data = housing)+ 
geom_boxplot(aes(State, Expense.per.SqFt),outlier.colour = "red")  
p
#----------------------------------------------------------#


p + ggtitle(xlab("State")) + (ylab("Expense Incurred per Sq.ft"))

#----------------------------------------------------------#

# boxplot : Expense per square feet (across various states + Building classification)
#----------------------------------------------------------#
# run this code first to create object p

p <- ggplot(data = housing, aes(housing$State, housing$Expense.per.SqFt)) 
p
#----------------------------------------------------------#
p + geom_boxplot(aes(fill = Building.Classification))+ 
  ggtitle(xlab("State")) + (ylab("Expense Incurred per Sq.ft"))

#----------------------------------------------------------#

# boxplot : Market Value per square feet (across various states + Building classification)
#----------------------------------------------------------#
# run this code first to create object p

p <- ggplot(data = housing, aes(State, Market.Value.per.SqFt))
p
#----------------------------------------------------------#

p + geom_boxplot(aes(fill = Building.Classification))+ 
  ggtitle(xlab("State")) + (ylab("Expense Incurred per Sq.ft"))

#----------------------------------------------------------#



#scatter plot: Expense per square feet (across various states + Building classification)

#----------------------------------------------------------#
# run this code first to create object p

p <- ggplot(housing, aes(Year.Built, Expense.per.SqFt))  

#----------------------------------------------------------#

p + geom_point(aes(colour = Building.Classification)) + 
  facet_wrap(~State, ncol = 2)+
  ggtitle(xlab("Year")) + (ylab("Expense Incurred per Sq.ft"))


#----------------------------------------------------------#

#scatter plot: Market BValue per square feet (across various states + Building classification)

#----------------------------------------------------------#
# run this code first to create object p

p <- ggplot(housing, aes(Year.Built, Market.Value.per.SqFt))  

#----------------------------------------------------------#


p + geom_point(aes(colour = Building.Classification)) + 
  facet_wrap(~State, ncol = 2)+
  ggtitle(xlab("Year")) + (ylab("Market Value per Sq.ft"))


#----------------------------------------------------------#


#scatter plot matrix: Messy one

# Powerful feature of  Base R Graphics
# Using the housing data (all states): corelation between Year.Built Vs
#Total.Units Vs Expense.Per.SqFt Vs Market.Value.Per.SqFt
#----------------------------------------------------------#


pairs(~Year.Built+Total.Units+Expense.per.SqFt+Market.Value.per.SqFt,data=housing, 
      main="Simple Scatterplot Matrix")

#----------------------------------------------------------#

#----------------------------------------------------------#


#scatter plot matrix


# Using the housing data (h_Bronx): corelation between Year.Built Vs
#Total.Units Vs Expense.Per.SqFt Vs Market.Value.Per.SqFt
#----------------------------------------------------------#


pairs(~Year.Built+Total.Units+Expense.per.SqFt+Market.Value.per.SqFt,data=h_bronx, 
      main="Simple Scatterplot Matrix")
 
#----------------------------------------------------------#
