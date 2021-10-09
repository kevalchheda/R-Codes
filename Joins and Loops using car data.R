#file I/O operations
#reporting car data for models  having sale > 500

#read the car sales Csv file
Car_data <- read.csv(file= "CarSales.csv")

#Display car sales data
Car_data

#Subset - Choose car make and model sales for sales>300
Topsales<- subset(Car_data, Sales>300)
Topsales

#list in descending order
Sorted_Topsales <- Topsales[order(-Topsales$Sales),]

#Display Sortetd Data
Sorted_Topsales

#write output file
write.table(Sorted_Topsales, file= "Sorted_Top_Sales.csv", sep = "," , row.names = FALSE)
#sep is separator

# Misc. Control Functions

# Conditiional Functions
Vec1<- c(15,20,4,15,60)

#return 1 if value is 4 return 0 otherwise
ifelse(vec1== 4,1,0)

#check for odd and even
Testvec = c(5,9,8,5,6)
Resultvec <- ifelse(Testvec %% 2 == 0, "even", "odd")

Resultvec

#concatenation

paste("a","b")

paste("Hello" , "world", sep = "_")

string1 <- c("hello", "world")
paste(string1)

paste(string1, collapse = " ")

paste(string1, collapse = "")

#Create a vector with null
vec1<- c(1,6,3, NA, 7,8, NA)

#check for null
is.na(vec1)

#copy non null values too another vector
no_null_vec <- vec1[!is.na(vec1)]

no_null_vec

#replace null with 0
vec1[is.na(vec1)] <- 0

vec1

#merge data frames
df1 = data.frame(customerid = c(1:6), product = c(rep("Toaster", 3) , rep ("Radio", 3)))
df2 = data.frame(customerid = c(2,4,6), state = c(rep("Alabama", 2) , rep ("Ohio", 1)))
df1
df2

# example of inner join
inner_merge <- merge(x = df1, y = df2, by = "customerid")
inner_merge

#example of outer join
outer_merge <- merge(x = df1,  y= df2, by = "customerid", all= TRUE)
outer_merge
                                  
                                  
                                  #example of left outer join
                                  L_outer_merge <- merge (x = df1, y= df2, by = "customerid", all.x= TRUE)
                                  L_outer_merge
                                  
                                  #example of right outer join
                                  R_outer_merge <- merge (x = df1, y= df2, by = "customerid", all.y= TRUE)
                                  R_outer_merge
                                  
                                  #Load and save
                                  ls()
                                  rm(list = ls())                        #cleanup
                                  y.vector = runif(20)                   # a vector of 20 random numbers
                                  ls() 
                                  
                                  save(y.vector, file = "yvec.saved")    #save to the working directory
                                  rm(y.vector)                           #remove
                                  ls()
                                  
                                  load("yvec.saved")
                                  class (ls()) 
                                  
                                  y.vector
                                  class(y.vector)
                                  
                                  x <- "a"
                                  
                                  rm(list= c("x","y.vector"))
                                  
                                  ls()
                                  
                                  #One way and two way tables
                                  
                                  
                                  #create gender vector
                                  gender <- c("male", "female", "male", "male", "female")
                                  
                                  #create marital status vector
                                  mstatus <- c("married", "single", "single", "married", "married")
                                  
                                  #one way tabulation
                                  table (gender)
                                  
                                  #two way tabulation
                                  table(gender, mstatus)
                                  #one to one mapping will take place
                                  
                                  #loop
                                  #for loop
                                  
                                  for(i in 1:5) {
                                    j
                                  }
                                  
                                  #display 'j' outside loop
                                  j 
                                  
                                  i < 1:5
                                  
                                  for (j in i) {
                                    print (j)
                                  }
                                  
                                  
                                  #while loop
                                  
                                  x<- 0 
                                  while (x<5){
                                    print(x <- x+1)
                                  }
                                  
                                  #Repeat - similar to a do-while loop 
                                  y<- 0
                                  repeat{
                                    print (y<- y+1)
                                    if (y>5)
                                      break
                                  }

# function definition without arguements
HelloWorld <- function(){
  print("Hello world")
  return(0)
}

# Function call
Hello_world

Sum_two_nos <- function (a,b) {
  sum2 <- a + b
  print((paste ("Sum is ", sum2)))
  }

#Function call
C <- Sum_two_nos(5,10)
C
