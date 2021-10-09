data1 <- read.csv("bodyfat-reduced.csv")
str(data1)
summary(data1)

#Partition Data : training and testing data

set.seed(111)
ind <- sample(2, nrow(data1), 
              replace = TRUE,
              prob = c(0.8,0.2)) #Split the data to 80 + 20%

training <- data1[ind == 1, ] #training data

testing <- data1[ind == 2, ] #testing data

# Scatter plot and correlation
library(psych)

pairs.panels(training,
             gap = 0,
             bg = c("red", "yellow", "blue")[training$BodyFat],
             pch = 21)

#Principal Component Analysis - It only works on numeric data

pc <- prcomp(training,
             center = TRUE,
             scale. = TRUE) #Use prcomp to principal component

#Only on datatype numeric

attributes(pc) #Names of the components associated with model

print(pc) #print the pc component which will explain the variations

plot(pc, type = 'lines') # Variance data

summary(pc)

# Print the individual components of the attributes pc

pc$dev #Standard deviation of the  principal components

pc$center #mean of the 4 variables

mean(training$Abdomen) #mean of the variable abdomen

pc$scale # standard deviation of the 4 variables

sd(training$Abdomen) # Standard deviation of the sepal length

pc$x # 4 principal component values 
# The value of pc degrades from PC1 to PC7 which means that the PC1 nd PC2 Carries much of the loading

pc$rotation # 7 prinipal axis values rotated

##################################################################
# Biplot information

biplot(pc, scale = 0) # messy approach

pca_val <- data.frame(PC1 = pc$x[1], PC2 = pc$x[2], PC3 = pc$x[3],
                      PC4 = pc$x[4],PC5 = pc$x[5], PC6 = pc$x[6], BodyFat = data1$BodyFat)

pca_val
# principal component to data frame with species
head(pca_val)

summary(pca_val)

# Biplot information using ggplot2
library(ggplot2) 
plot.data_pc <- ggplot(data = pca_val, aes(x= PC1, y= PC2)) +
  geom_point(aes(color = pca_val$BodyFat)) + labs(x = "PC1") + labs(y = "PC2") +
  theme_bw()

plot.data_pc

# Optional Information 

#Orthogonality of PCs
pairs.panels(pc$x,
             gap = 0,
             bg = c("red", "yellow", "blue")[training$BodyFat],
             pch = 21)
