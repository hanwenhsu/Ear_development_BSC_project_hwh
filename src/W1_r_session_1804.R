# Practice 18.04.2023
library(dplyr)

# Challenge 1
##Using each-argument to replicate elements within a vector 
## instead of replicating the vector itself

c("a","b") %>% rep(.,each = 2) %>% paste0(.,c(1,2)) ## Outcome = "a1" "a2" "b1" "b2"
c(1,2) %>% rep(., each=2) %>% paste0(c("a","b"),.)  ## Outcome = "a1" "b1" "a2" "b2"

# Challenge 2: Writing a function

vec1 <- c(1,2,3)

func_vec <- function(vec){
  # input: datatype = vector, length = 3, meaning = a numeric vector with length 3
  # output : datatype = string, meaning = a string of average value of vec and its SD
  # action 1 - calculating the average value of vec : mean_vec <- mean(vec)
  # action 2 - calculating the SD of vec : sd_vec <- sd(vec)
  # action 3 - creating the output string : output_str <- paste0(mean_vec, "±", sd(vec))
  # action 4 - return output : return(output_str)
  mean_vec <- mean(vec)
  sd_vec <- sd(vec)
  output_str <- paste0(mean_vec, " ± ", sd(vec))
  return(output_str)
}

# Challenge 3: Dates

start_date <- as.Date("2023-04-17", format="%Y-%m-%d")
date_seq <- seq(start_date, by= 1, length.out = 5)
date_seq
mean(date_seq)

# Challenge 4: Pattern Matching

func_average <- function(x_num){
  # input: x_num, datatype = numeric input
  # output: datatype = numeric value, meaning = mean of input
  # or -- output: warning when non-numeric input
  # action 1 -- check input type
  # action 2 -- calculate mean if numeric
  # action 3 -- else error message "non numeric input"
  if(is.numeric(x_num)){
    mean_x <- mean(x_num)
    return(mean_x)
  }else{
    warning("non-numeric input")
  }
}

func_average(c(6,12,18))
func_average("non")
