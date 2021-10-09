library(dplyr)
paste
x <- 1:3

y <- 10:12
rbind(x, y)
rbind
A <- c(3,4,6,7,10)
mean(A)
var(A)
library(help = "datasets")
system.file("data",package = "datasets")
data()
data("state")

head(state.abb, 6)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center, state.division, state.name, state.region)
statedata
library(Hmisc)
library(pastecs)
library(psych)
library(dplyr)
length(statedata$state.name)
range(statedata$Population)
y = mean(statedata$HS.Grad)
min(statedata$Murder)
x= na.omit(statedata)
tapply(x$HS.Grad, x$state.region, mean)
View(statedata)
is.na(statedata)
