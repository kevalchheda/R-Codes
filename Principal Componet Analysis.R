data("iris")
str(iris)
summary(iris)

#Portition Data : training and testing data

set.seed(111)
ind <- sample(2, nrow(iris), 
              replace = TRUE,
              prob = c(0.8,0.2)) #Split the data to 80 + 20%

training <- iris[ind == 1, ] #training data

testing <- iris[ind == 2, ] #testing data

# Scatter plot and correlation
library(psych)

pairs.panels(training[, -5],
             gap = 0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch = 21)

#Principal Component Analysis - It only works on numeric data

pc <- prcomp(training[,-5],
             center = TRUE,
             scale. = TRUE) #Use prcomp to principal component

#Only on datatype numeric

attributes(pc) #Names of the components associated with model

print(pc) #print the pc component which will explain the variations

plot(pc, type = 'lines') # Variance data

summary(pc)

# Print the individual components of the attributes pc

pc$dev #Standard deviation of the 4 principal components

pc$center #mean of the 4 variables

mean(training$Sepal.Length) #mean of the variable sepal length

pc$scale # standard deviation of the 4 variables

sd(training$Sepal.Length) # Standard deviation of the sepal length

pc$x # 4 principal component values 
# The value of pc degrades from PC1 to PC4 which means that the PC1 nd PC2 Carries much of the loading

pc$rotation # 4 prinipal axis values rotated

##################################################################
# Biplot information

biplot(pc, scale = 0) # messy approach

pca_val <- data.frame(PC1 = pc$x[,1], PC2 = pc$x[,2], PC3 = pc$x[,3],
                      PC4 = pc$x[,4], Species = training$Species)

# principal component to data frame with species
head(pca_val)

summary(pca_val)

# Biplot information using ggplot2
library(ggplot2) 
plot.data_pc <- ggplot(data = pca_val, aes(x= PC1, y= PC2)) +
geom_point(aes(color = Species)) + labs(x = "PC1(73.73%)") + labs(y = "PC2(22.11%)") +
theme_bw()

plot.data_pc

# Optional Information 

#Orthogonality of PCs
pairs.panels(pc$x,
             gap = 0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch = 21)

# Bi -plot 
#install.packages("devtools")
library(devtools)
install_github("ggbiplot", "vqv")

# Multinomial Logistic Regression with First two PCs
install.packages("nnet")
library(nnet)