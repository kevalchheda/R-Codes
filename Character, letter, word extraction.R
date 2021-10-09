#clear all existing variables from console
cat ("\014")

#clear all data variables
rm(list = ls())

#----------------------------------------------------------
# Read Input
#----------------------------------------------------------
person <- read.csv("file1-1.txt")
person

#------------------------------------------------------------
#read input again but neglect headers and row names
#---------------------------------------------------------------
person <- read.csv(file= "file1-1.txt", header = FALSE, col.names = c("age","height"), row.names = NULL, sep = " ")
person

#Explore structure of the data
str(person)
# Note Hieght is treated as factor here

# lets try another read method-------------------
person <- read.csv(file = "file1-1.txt", header = FALSE, col.names = c("age", "height"), row.names = NULL, sep= " ", stringsAsFactors = FALSE)
person
str(person)


#Lets Convert
person$height <- as.numeric(person$height)
str(person)

#Now we have NA's
#---------------------------------------------------------------
#Data Prep Case 2: reading a file with readlines
#---------------------------------------------------------------
#Typical steps required for file data process
#---------------------------------------------------------------
# 1. Read the data with readlines character
# 2. Select lines containing data character
# 3. Split lines into separate fields list of character vectors
# 4. Standardize rows list equivalent vectors
# 5. Transpform to data.frame data.frame
# 6. Normalize and coerce to correct type data.frame
#------------------------------------------------------------------

# Step 1 Read

#-----------------------------------------------------------------
txt <- readLines("dalton.txt")
txt

# Step 2 : Getting rid of commented lines
#-----------------------------------------------------------

# detect lines starting with percent signs
I <- grepl("^%", txt)
I
# and throw them out 
dat <- txt[!I]
dat
# The issue here is that the characters and numbers(dates) got jumbled

#-----------------------------------------------------------
# Step 3 :Split lines
#-----------------------------------------------------------
fieldList <- strsplit(dat, split = ",")
fieldList

#-----------------------------------------------------------
# Step 4:Standardize Rows
# 1. Every row should have same number of fields
# 2. Fields should be in right order
#-----------------------------------------------------------

assignFields <- function(x){
  out <- character (3)
  #get names
  i <- grepl( "[[:alpha:]]" ,x)
  out[1] <- x[i]
  #get birthdates
  i <- which(as.numeric(x)< 1890)
  out[2] <- ifelse (length(i)>0 , x[i], NA)
  #get death date
  i <- which(as.numeric(x) > 1890)
  out[3] <- ifelse (length (i) >0 , x[i], NA)
  return(out)
}

standardfields <- lapply(fieldList, assignFields)  
standardfields  

#-----------------------------------------------------------
# Step 5 : Coerce to data frame
#-----------------------------------------------------------
# Create Matrix from list
(M<- matrix(
  unlist (standardfields),
  nrow = length(standardfields),
  byrow = TRUE)
 )
colnames(M) <- c("name", "birth", "death")
M

#Populate data frames
daltons <- as.data.frame(M,stringsAsFactors = FALSE)
daltons

#-----------------------------------------------------------
# Step 6 : Normalize to correct data types
#-----------------------------------------------------------
daltons$birth <- as.numeric(daltons$birth)
daltons$death <- as.numeric(daltons$death)
daltons

#-------------NOW DATAFRAME IS READY FOR EDA--------------------------------------

#Handling NA's while  doing EDA
Age_vec <- c(23,16,NA)
Mean_age <- mean(Age_vec)
Mean_age

#Oops we have issue here
# As the column has NA it could not process

# Bypass NA
Mean_age <- mean(Age_vec, na.rm = TRUE)
Mean_age

#Back to Daltons's example
# How many rows have NA's

NA_row_count <- nrow(daltons) - sum(complete.cases(daltons))
NA_row_count

#How can I remove rows which have NA's?

good_rows <- na.omit(daltons)
good_rows

#Imputattion : can I replace the NA's with the mean for the below example
Age_vec <- c(23,16,15,26,19,NA)
Mean_age <- mean(Age_vec, na.rm = TRUE)

#check mean age
Mean_age

#Impute with mean
Age_vec[ is.na(Age_vec)] <- Mean_age
Age_vec


