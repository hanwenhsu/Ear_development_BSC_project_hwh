library(dplyr)

# 1. WORKING DIRECTORY (wd)

# abbreviation for wd and parent for wd
# working directory, abbreviated as "."
getwd()
# parent directory, abbreviated as ".."
dirname(getwd())
# assign current path to variable
current_path <- getwd()
# check the type 
current_path %>% str()


# check files in the directory

# are they different?
"." %>% list.files(path=.)
getwd() %>% list.files(path=.)

# are they different?
".." %>% list.files(path=.)
getwd() %>% dirname() %>% list.files(path=.)

# accessing files and folders in R-project
# absolute path, did you get error?
"C:/Users/marse/seadrive_root/Tien-Che/My Libraries/PhD_Tien/Project/Postdoc_teaching/BSC_project_IPFS2023/data" %>% list.files(path=.)
# relative path in R base
parent_path <- getwd() 
paste0(parent_path,"/data") %>% list.files(path=.)

# R project relative path, are they different? 
"./data" %>% list.files(path=.)
"data" %>% list.files(path=.)

# Challenge 2: Absolute paths
# read from relative path, which one is correct? 
# Assume working directory current_path is "C/users/BSC_project/src"
# Absolute_path = current_path +"/"+relative_path 
# Below are four relative paths. 
# Please rewrite them in absolute (full) path form. 
# What should . be replace? Which two are the same?

# "ear_summarized.csv"
## "C/users/BSC_project/src/ear_summarized.csv"

# "data/ear_summarized.csv"
## "C/users/BSC_project/src/data/ear_summarized.csv"

# "./data/ear_summarized.csv"
## "C/users/BSC_project/src/data/ear_summarized.csv"

# "../data/ear_summarized.csv"
## "C/users/BSC_projekct/data/ear_summarized.csv"

# 2. SUBSETTING ELEMENTS FROM VECTORS USING ACCESSORS []

## Vectors

empty_vec <- c()
length(empty_vec)
# what is the type of the empty vec?
empty_vec %>% str()

# NULL: empty 
empty_vec[1]
empty_vec[0]


vec <- c(1,3,5)
vec[1]
#reorder the vector 
vec[c(2,1,3)]
# removing the indexed elements
vec[-1]
vec[-2]

# indexing start from 1, not 0
# therefore you get, numeric(0)
vec[0]
# when access exceeding the range of a vector, what datatype do you get? 
vec[4:6]
vec %>% .[length(.)+1]
vec[1:4]
vec[4:1]

# find specific element or position
vec[c(F,T,F)]
vec[vec==5] ## equivalent to vec[c(F,F,T)]
# when codition not match at all, it will return? 
vec[vec==2] ## equivalent to vec[c(F,F,F)]
vec[c(F,F,F)] 
vec %>% .[c(F)]
vec[vec=="a"]


# default str vector
letters
LETTERS
# when the query does not match, guess what will be the datatype? 
letters %>% .[.==2]
letters %>% .[c(F)]
# vector over write
vec
vec <- c(2,1,3)
vec

# Challenge 4: Vector recycling --> the SHORTER vector will always be recycled
vec <- c(1, 2, 3, 4, 5)
logical_vec <- c(TRUE, FALSE)
subset_vec <- vec[logical_vec]
subset_vec

## Lists

# create a simple list
list(1)
# create a simple list with name "x" for first element
list(x=1)
list(x=1)["x"]  ## simple bracket : indicates the position of the list
# extract content
list(x=1)$"x"
list(x=1)[[1]]
list(x=1)[["x"]] ## double bracket : indicates the content of the list

# extract with pipe
list(x=1) %>% .[[1]]
list(x=1) %>% .$"x"

# long list
long_list_example <- list(1,c(1,2),
                          T,c(T,T),
                          "str",c("a","b"),
                          list(1),
                          mean,data.frame())
# check structure of this list 
#long_list_example %>% str()
#long_list_example %>% glimpse()
#long_list_example
# first list 
long_list_example[1]
# content of first list
long_list_example[[1]]
# first element of content of first list
long_list_example[[1]][1]

# Challenge 5 : guess the data types
long_list_example[[1]][2] #NA --> NA
long_list_example[1][1] #list --> list
long_list_example[1][2] #list --> NULL
long_list_example[2][2] #list --> NULL
long_list_example[[2]][2] #number --> number

# Lapply - return as a list --> lapply (vector, FUN=function)
# input is vector
c(1,4) %>% 
  lapply(.,FUN=function(x){x+3})
# input is list
list(2,4,c(1,4)) %>% 
  lapply(.,FUN=function(x){x+3})
# input has differnt type
list(2,4,c(1,4),"8") %>% 
  lapply(.,FUN=function(x){x+3})

# Challenge 6: Why is there an error at the last line --> String with numeric operator

# Dataframe
# create a dataframe 
df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                 temp=c(20,15,13), 
                 thermal_time=cumsum(c(20,15,13)))
# another way
df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1)) 
df$temp=c(20,15,13)
df$thermal_time=cumsum(df$temp)

# third method
library(dplyr)
df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1)) %>% 
  mutate(temp=c(20,15,13), 
         thermal_time=cumsum(temp))
df

# Challenge 7: vectors of different length cannot be used to create a dataframe
data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),temp=c(20,13))

# Challenge 8 
# use df, 
# 1. Access column thermal_time as vector 
df[,"thermal_time"]

# 2. Extract temp when time is 2023-04-17
df[df$time == "2023-04-17","temp"]

# Challenge 9: if you want to turn a data frame (df) by 90 degree (“transpose”), 
# which function can you use? Could you find the answer on google or chatGPT?

df_t <- t(df)
