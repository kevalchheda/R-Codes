#Cluster Analysis
library(readxl)
mydata <- read_xlsx("D_mart.xlsx")
str(mydata)
head((mydata))

#Scatter Plot
plot(mydata$Fuel_Cost ~ mydata$Sales, data = mydata, xlab = 'Sales', ylab = 'Fuel Cost')
with(mydata, text(mydata$Fuel_Cost ~ mydata$Sales, labels = mydata$Outlets,pos=4))

#Normalize
z = mydata[ , -c(1,1)]
z
apply
means = apply(z,2,mean)
means
sds = apply(z,2,sd)
nor = scale(z, center = means, scale=sds)
nor

#Calculate distance matrix (Default is euclidean distance)
distance = dist(nor)
distance

#Hierarchical agglomerative clustering using default complete linkage
mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust, labels = mydata$Outlets , main = 'Default from hclust')
plot(mydata.hclust, hang = -1)

#hierarchical agglomerative clustering using average linkage
mydata.hclust <- hclust(distance, method = "average")
plot(mydata.hclust, hang = -1)

#Cluster Membership
member = cutree(mydata.hclust , 3)
table(member)

#Characterising Clusters
aggregate(nor, list(member), mean)
aggregate(mydata[, -c(1,1)], list(member), mean)

#Scree Plot
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for(i in 2:20) wss[i] <- sum(kmeans(nor, center= i)$withinss)
plot(1:20, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of sqaures")

#Kmeans clustering
kc <- kmeans(nor, 3)
kc

plot(mydata$Fuel_Cost ~ mydata$Sales, data = mydata, xlab = "Sales", ylab = "Fuel Cost", 
     col= kc$cluster, pch = 19)