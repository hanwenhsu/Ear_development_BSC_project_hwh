library(dplyr)
library(ggplot2)

# more mutate examples

df <- expand.grid(x=letters[1:4],
                  y=1:2)%>%
  # combine columns x and y 
  mutate(z=interaction(x,y))
rownames(df) <- LETTERS[1:nrow(df)]

# replace one column based on single condition

df %>% mutate(k=ifelse(x=="a","A","B"))
df %>% mutate(k=ifelse(y==1,"A","B"))
df %>% mutate(k=case_when(x=="a"~"A",
                          TRUE~"B"))
# practice 1 : Matching multiple conditions 
# add column k to df, when the condition x equals “a” and y equals 1.

df %>% mutate(k=ifelse((x=="a" & y==1),"A","B"))
df <- df %>% mutate(k=ifelse((x=="a" & y==1),"A","B"))

# replace one column based on multiple conditions

df %>% mutate(k=case_when(x=="a"~"A",
                          x=="b"~"B",
                          TRUE~"C"))

look_table <- data.frame(x=letters,
                         X=LETTERS)
df %>% merge(look_table)

# practice 2 : merge is not actually replace the original column.

# Write a function to replace letters with LETTERS. 
# The input is a vector of lower case vector vec <- c("c","a","b","d"), 
# output will be the matched upper case vector c("C","A","B","D").

vec <- c("c", "a", "b", "d")

vec_func <- function(vec){
  # 1: merge target vector to look table
  df <- vec %>% merge(look_table)
  # 2: Identify order of the vectors
  order_vec <- match(vec, df$x)
  # 3 output vector with correct order
  VEC <- df$X[order_vec]
  return(VEC)
}

# an easier way with letters/LETTERS
vec_func <- function(vec){
  # 1: merge target vector to look table
  # 2: Identify order of the vectors
  order_vec <- match(vec, letters)
  # 3 output vector with correct order
  VEC <- LETTERS[order_vec]
  return(VEC)
}


vec_func(vec)

# advanced mutate add-on :across
# apply function for multiple columns

climate <- read.csv("./data/climate.csv", sep=",")

climate %>% 
  select(ends_with("Radiation")) %>% 
  head(.,3) %>%
  glimpse()


climate %>% 
  mutate(across(where(is.numeric),~{round(.x, digits = 2)})) %>%
  # mutate(across(where(is.numeric),function(x){round(x, digits = 2)})) %>%
  select(ends_with("Temperature")) %>% 
  head(.,3) %>% 
  glimpse()

# reduce your code chunk by using function
display <- function(x){
  # subset dataframe and summarized for displaying purporse
  # x: input data frame
  x %>% 
    dplyr::select(ends_with("Temperature")) %>% 
    head(.,3) %>% 
    dplyr::glimpse()
}

climate %>% 
  mutate(across(where(is.numeric),~{round(.x, digits = 2)})) %>%
  display()


# Examples
by_cyl <- mtcars %>% group_by(cyl)

# grouping doesn't change how the data looks (apart from listing
# how it's grouped):
by_cyl

# It changes how it acts with the other dplyr verbs:
by_cyl %>% summarise(
  disp = mean(disp),
  hp = mean(hp)
)
by_cyl %>% filter(disp == max(disp))

by_cyl %>% summarize(across(c(disp,hp),mean))

# Each call to summarise() removes a layer of grouping
by_vs_am <- mtcars %>% group_by(vs, am)
by_vs <- by_vs_am %>% summarise(n = n())
by_vs
by_vs %>% summarise(n = sum(n))

# To removing grouping, use ungroup
by_vs %>%
  ungroup() %>%
  summarise(n = sum(n))

# By default, group_by() OVERRIDES existing grouping
by_cyl %>%
  group_by(vs, am) %>%
  group_vars()

# Use add = TRUE to instead append
by_cyl %>%
  group_by(vs, am, .add = TRUE) %>%
  group_vars()

# You can group by expressions: this is a short-hand
# for a mutate() followed by a group_by()
mtcars %>%
  group_by(vsam = vs + am)

# The implicit mutate() step is always performed on the
# ungrouped data. Here we get 3 groups:
mtcars %>%
  group_by(vs) %>%
  group_by(hp_cut = cut(hp, 3))

# If you want it to be performed by groups,
# you have to use an explicit mutate() call.
# Here we get 3 groups per value of vs
mtcars %>%
  group_by(vs) %>%
  mutate(hp_cut = cut(hp, 3)) %>% # separate by interval in numeric vectors
  group_by(hp_cut)

# when factors are involved and .drop = FALSE, groups can be empty
tbl <- tibble(
  x = 1:10,
  y = factor(rep(c("a", "c"), each  = 5), levels = c("a", "b", "c"))
)
tbl %>%
  group_by(y, .drop = FALSE) %>%
  group_rows()



# practice 3:read climate.csv from data folder using relative path and name it as climate.
# find the sowing date (i.e., the first date) for each year (DFG_year) 
# and each sowing treatment(sowing_date) from.
climate_sub <- climate %>% 
  dplyr::select(DayTime,DailyMean_Temperature,DFG_year,sowing_date)
# calculate thermal time based on DailyMean_Temperature from climate_sub

climate %>% 
  group_by(DFG_year,sowing_date) %>%
  mutate(DayTime = as.Date(DayTime,format = "%Y-%m-%d")) %>%
  filter(DayTime == min(DayTime)) %>%
  select(DayTime,sowing_date,DFG_year)

climate_sub %>%
  group_by(DFG_year,sowing_date) %>%
  mutate(DayTime = as.Date(DayTime,format = "%Y-%m-%d")) %>%
  arrange(DayTime) %>% #dont forget arrange in chronological order
  mutate(TT = cumsum(DailyMean_Temperature))

# check with ggplot
library(ggplot2)
merge_thermal_time <- thermal_time%>% 
  merge(.,climate %>% 
          select(DayTime,DFG_year,sowing_date,Acc_Temperature)) 

merge_thermal_time %>% 
  ggplot(.,aes(x=TT,y=Acc_Temperature))+
  geom_point(shape=0,size=.5,alpha=.5)+
  geom_abline(intercept=0)+
  facet_grid(sowing_date~DFG_year)


#climate %>%
#  group_by(DFG_year,sowing_date) %>% #group by year and treatment
#  mutate(TT = cumsum(DailyMean_Temperature)) %>% # calculate TT within groups
#  mutate(Tmean = mean(DailyMean_Temperature, na.rm=TRUE))%>% # show Tmean within groups
#  select(DayTime, DFG_year, sowing_date, TT, Tmean, Acc_Temperature) %>% # select column
#  filter(row_number()==1 | row_number()==n()) # select first and last row



