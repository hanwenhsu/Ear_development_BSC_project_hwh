# write and read data
cats <- data.frame(coat = c("calico", "black", "tabby", "tabby2"),
                   weight = c(2.1, 5.0, 3.2, "2.3 or 2.4"),
                   likes_string = c(1, 0, 1,1))

write.csv(x = cats, file = "data/feline-data_v2.csv", row.names = FALSE)

cats <- read.csv(file = "data/feline-data_v2.csv")
cats

# Challenge 1

# Read data
cats <- read.csv("data/feline-data_v2.csv")

# 1. Print the data
print(cats)

# 2. Show an overview of the table with all data types
str(cats)

# 3. The "weight" column has the incorrect data type character.
#    The correct data type is: double/numeric.

# 4. Correct the 4th weight data point with the mean of the two given values
cats$weight[4] <- 2.35
#    print the data again to see the effect
cats
str(cats)
# 5. Convert the weight to the right data type
cats$weight <- as.numeric(cats$weight) # or <- as.double(cats$weight)
str(cats)
#    Calculate the mean to test yourself
mean(cats$weight)

# If you see the correct mean value (and not NA), you did the exercise
# correctly!

# vector functions

vec <- seq(20,26,0.5)
head(vec, n=3) # first 3 values of the vector
tail(vec, n=4) # last 4 values of the vector
length(vec) # length of vector

# Challenge 2: 
# Start by making a vector with the numbers 1 through 26. 
# Then, multiply the vector by 2.

vec2 <- 1:26
vec2 <- vec2*2
vec2

# lists
pizza_price <- c( pizzasubito = 5.64, pizzafresh = 6.60, callapizza = 4.50 )
names(pizza_price)
# Challenge 4
# 1. Create a vector that gives the number for each letter in the alphabet!
# 1. Generate a vector called letter_no with the sequence of numbers from 1 to 26!
# 2. R has a built-in object called LETTERS. It is a 26-character vector, from A to Z. 
# 2. Set the names of the number sequence to this 26 letters
# 3. Test yourself by calling letter_no["B"], which should give you the number 2!

letter_no <- 1:26 #1
names(letter_no) <- LETTERS #2
letter_no #3
letter_no["B"] #3

# Matrices
matrix_example <- matrix(0, ncol=6, nrow=3)
matrix_example

nrow(matrix_example)
ncol(matrix_example)

# Challenge 6 
length(matrix_example)

# Challenge 7
# Make another matrix, this time containing the numbers 1:50, 
# with 5 columns and 10 rows. Did the matrix function fill your matrix by column, 
# or by row, as its default behaviour? See if you can figure out how to change this. 
# (hint: read the documentation for matrix!)

matrix2 <- matrix(1:50, ncol=5, nrow=10, byrow= FALSE) # by column
matrix2 <- matrix(1:50, ncol=5, nrow=10, byrow= TRUE) # by row
