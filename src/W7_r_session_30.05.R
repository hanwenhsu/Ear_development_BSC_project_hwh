library(dplyr)
library(ggplot2)
climate <- read.csv("./data/climate.csv", sep=",")
glimpse(climate)

# Logic of coding
#1. Check: examine the datatype of each column in your dataframe.
#2. Drafting: draw the draft of your desired output.
#3. Target columns: identify which columns you would need to generate the output.
#4. Steps: write down the possible steps which required to generate the target columns.

# challenge 1:
# Compare the growing seasons across three years.

# check the datatype of DayTime, make sure it is Date.

climate$DayTime <- as.Date(climate$DayTime, format="%Y-%m-%d")
glimpse(climate)

# group_by year and sowing date, mutate a new column called DAS(Days after sowing).
climate_new <- climate %>%
  group_by(DFG_year, sowing_date) %>% #grouping by year and sowing
  arrange(DayTime) %>% #chronological order
  mutate(DAS = as.numeric(difftime(DayTime,min(DayTime), units="days"))) %>% #calculate days after sowing (DAS)
  ## OR : simple arithmetic with as.numeric ##
  mutate(Year_Sowing = paste(DFG_year, sowing_date, sep="_")) %>% #new group column for easier plotting
  select(DAS, DFG_year, sowing_date, Acc_Temperature, Year_Sowing)

# use ggplot to visualize this dataframe with points and lines:
# x is DAS, y is Acc_Temperature, color is DFG_year and 
# the points should be linked of same DFG_year and sowing_date.
climate_new %>%
  ggplot(.,aes(x = DAS, y = Acc_Temperature, color = DFG_year))+
  geom_line(aes(linetype=sowing_date, group = Year_Sowing))+
  xlab("Days after sowing")+
  ylab("Thermal time (°Cd)")+
  theme_light()+
  theme(legend.position = c(.2,.7))

# Is there additional columns required? 
## "Year_Sowing" for grouping of the lines

# Code answer: 
climate %>% 
  # dplyr::filter(DFG_year%in%c("DFG2019","DFG2020")) %>% 
  group_by(DFG_year,sowing_date) %>% 
  mutate(DayTime=as.Date(DayTime,format="%Y-%m-%d"),
         DAS=as.numeric(DayTime-min(DayTime))) %>% 
  ggplot(aes(x=DAS,y=Acc_Temperature,color=DFG_year,
             group=interaction(sowing_date,DFG_year)))+
  # geom_point()+
  geom_line(aes(linetype=sowing_date),linewidth=1)+
  theme_bw()+
  theme(legend.position = c(.1,.65))+
  labs(x="Days after sowing",y= "Thermal sum (°Cd)")+
  guides(color=guide_legend(title="Year"))

# how to get the minimum unique combination of dataframe?

climate %>% 
  dplyr::filter(DFG_year=="DFG2019") %>% 
  group_by(y,m) %>% 
  summarise()

climate %>% 
  dplyr::filter(DFG_year=="DFG2019") %>% 
  dplyr::select(y,m) %>% 
  dplyr::distinct()

# challenge 2: 
# read ear_summarized.csv and 
# extract the unique combinations of nitrogen, appl and timeid

ear_summarized <- read.csv("./data/ear_summarized.csv", sep = ",")

ear_summarized %>%
  select(nitrogen,appl,timeid) %>% 
  distinct()
  #group_by(nitrogen,appl,timeid) %>%
  #summarise()
 

# shapes of dataframe
# In general, we can describe the shape of dataframe as wide or long
# Wide refers to a dataframe which each column is one trait.
# Long refers to a dataframe which multiple trait names in one column 
# and multiple trait values in another one.

climate %>%glimpse()
climate_long <- climate %>% 
  tidyr::pivot_longer(names_to = "Daily_Terms",
                      values_to = "Daily_value",
                      cols = contains("Daily")) 
climate_long%>%   names()
climate_long%>% glimpse()

#select cols by position
# grep("(Daily|Acc)",names(climate))
climate_long <- climate %>% 
  tidyr::pivot_longer(names_to = "Terms",
                      values_to = "value",
                      # select both patterns
                      cols = grep("(Daily|Acc)",names(.)))

climate_long%>% names()

## data processing example
climate_long_subset<- climate_long %>% 
  filter(Terms%in%c('Acc_Temperature','Acc_Precipitation')) %>% 
  group_by(DFG_year,sowing_date,Terms) %>%
  summarise(Value=mean(value))

climate_long_subset

# Challenge 3: 
# Analyze the code that generating Fig2.
# Figure out how it works by blocking one line at a time. add documentation for each line.
# Which function need package scales?

library(scales) %>% suppressMessages()

climate_long %>% 
   #filtering wanted information
  filter(Terms%in%c('Acc_Temperature','Acc_Radiation'),
         sowing_date=='Early') %>%
   #group points with year and sowing date instead of unordered
  group_by(DFG_year,sowing_date) %>%
   # transforms Daytime (chr) into Dates for arithmetic operations
   # generate DAS through DayTime
  mutate(DayTime=as.Date(DayTime,format="%Y-%m-%d"),
         DAS=as.numeric(DayTime-min(DayTime))) %>% 
   # generate plot - introducing x and y value (and color grouping)
  ggplot(aes(DAS,value,color=DFG_year))+
   # generate line
  geom_line()+
   # separate plot into 2 plots based on "Terms" 
   # - here just 'Acc_Temperature'&'Acc_Radiation' (filtered out in first step)
  facet_grid(~Terms)+
   # theme
  theme_test()+
  theme(strip.background = element_blank(), # heading background
        strip.text = element_text(size=14), # heading size
        axis.text = element_text(size=14), # axis text
        axis.title = element_text(size=14), # axis title
        legend.position = c(.1,.1))+
  scale_y_log10(
    labels = label_number(scale_cut = cut_short_scale()) # log transformation for y axis
  )+
  xlab('Days after sowing')

# long
climate_long <- climate %>% # climate is wide
  tidyr::pivot_longer(names_to = "Daily_Terms",
                      values_to = "Daily_value",
                      cols = contains("Daily")) 
# wide again
climate_wide<- climate_long%>% 
  tidyr::pivot_wider(names_from = "Daily_Terms",
                     values_from = "Daily_value")

# check if they are the same 
setdiff(names(climate),names(climate_wide))
all.equal(climate,climate_wide)
# change the order of column
all.equal(climate,climate_wide[,names(climate)])
# change the type
all.equal(climate,climate_wide[,names(climate)]%>% as.data.frame())

# Looping with for(), lapply() and map()

filename <- c('grain_counting_practice_studentName1.xlsx',
              'grain_counting_practice_studentName2.xlsx')
file_list<- filename %>% strsplit("_")
# tradition way of for loop
res <- c()
for(i in 1:2){
  res <- c(res,file_list[[i]][4])
}

# alternative in r package purrr
# chr stands for the "character" output.
purrr::map_chr(1:length(file_list),  ~{
  file_list[[.x]][4]
})

# notice that the output of map_chr must be 1 element per iteration.
purrr::map_chr(filename,  ~{
  .x %>% strsplit("_") %>% unlist()
})

# equivalent
purrr::map(filename,  ~{
  .x %>% strsplit("_") %>% unlist()
})

lapply(filename,function(x){
  x %>% strsplit("_") %>% unlist()
})

# challenge 4: 
# using for loop, extract the student name from file name. 
# 1. list the files of the folder student using list.files() 
# 2. write your own for loop.

student_list <- list.files("./data/student")
str(student_list)

for(i in student_list){
   paste("./data/student/",i, sep="") %>%
    readxl::read_xlsx()%>%
    names() %>% print()
}

# Using map_dfr(), read the three files in the folder.
# 
# 1. list the files of the folder student using list.files()
# 2. find which function could read .xlsx
# 3. read each .xlsx as one list element
# 4. add a column containing student name information
# 5. use for loop to return the column names of three dataframes.
# 6. visualize it with ggplot:
#   6.1. do data summary for each file, compare the range of three files.
# 
# 6.2. Visualize it with ggplot, differentiate the data from each student by color.

purrr::map_dfr(student_list, ~{
  .x %>%  paste("./data/student/",., sep="") %>%
    readxl::read_xlsx()
})

