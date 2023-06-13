# Dataframe with real data

library(dplyr)

df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                 temp=c(20,15,13), 
                 thermal_time=cumsum(c(20,15,13)))
df %>% dplyr::glimpse() ## Avoid name conflict e.g. glimpse() in another package x
names(df)
# extract column from dataframe
df$thermal_time
df[,3]
df[,'thermal_time']

df[['thermal_time']]

# not work
df[thermal_time]
# different error message
#!! name space conflict
df[time]
time

# summarize dataframe
lapply(df, range)
# turn as data frame
lapply(df, range) %>% data.frame()

summary(df)

# Challenge 1

##Practice with large data set ear_summarized.csv in folder data.

## 1. read the file with relative path using function read.csv().
df2 <- read.csv("./data/ear_summarized.csv")

## 2. find the row and column number of data frame by nrow and ncol
nrow(df2) # 2112 rows
ncol(df2) # 9 columns

## 3. check the range of each column using lapply, how many unique days exist in column date
lapply(df2, range) %>% data.frame()
unique(df2$date) ## Unique days
df2$date %>% unique(.) %>% length(.) ## 11 unique days

## 4. compare the result of glimpse and str
dplyr::glimpse(df2)
str(df2)

## 5. extract column weight using [],[[]]and $ 1
df2[,"weight"]
df2[["weight"]]
df2$weight

## 6. what is the function of head() and tail()?
head(df2, n=2) ## return the first n values
tail(df2, n=3) ## return the last n values

## 7. how to extract the first three row using []?
df2[1:3,]


# dplyr

## Subset Rows -- dplyr::filter(): extract row where the condition matched

# df$time %>% str()
df %>% dplyr::filter(time=='2023-04-17') %>% .$temp
df %>% dplyr::filter(time==as.Date('2023-04-17')) %>% .$temp

## Add columns -- dplyr::mutate(): add one or multiple columns to dataframe

# result is not save
df %>% dplyr::mutate(Year="2023") 
df
# result is saved
df$Year <- "2023"
df[['Year']] <- "2023"
df

# Challenge 2 : How to save result using %>%? Check example with ?mutate.
## df <- df %>% dplyr::mutate(Year="2023") 

## Combine dataframes by column 
## -- at least one column in common and merge based on this column

df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                 temp=c(20,15,13), 
                 thermal_time=cumsum(c(20,15,13)))
# with same length DATAFRAME
ear_df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                     ear_weight=c(20,40,50))
merge(df,ear_df,by="time") ## merge based on "time"-column
dplyr::left_join(df,ear_df,by="time")

# combind with VECTOR of same length 
cbind(df, ear_weight=c(20,40,50))
df$ear_weight <- c(20,40)

# with differnt length 
short_ear_df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,2,1),
                           ear_weight=c(20,40))
merge(df,short_ear_df,by="time") ## the shortest length taken
dplyr::left_join(df,short_ear_df,by="time") ## refers to length of left, missing value as NA

# combind with vector of different length 
cbind(df, ear_weight=c(20,40))
df$ear_weight <- c(20,40)


# ggplot2 
phenology <- read.csv("./data/phenology_short.csv")
library("ggplot2")
#data <- read.csv("./data/ear_summarized.csv")

## Requirements of scientific plot.
## 1. axis title: specify with unit if there is any using xlab() or ylab().
## 2. legend title: full name instead of default abbreviation using guides().
## 3. other important rules

df2$group %>% unique(.) %>% length(.)

df2 %>% 
  ggplot(aes(x=date,y=weight, color=var))+
  geom_point()+
  geom_line(aes(group=group))+ # link the point by group.
  xlab("date of harvest")+ #x axis title
  ylab("ear weight(g)")+   #y axis title
  guides(color=guide_legend(title="Cultivar"))+ #change legend title 
  # Challenge 3 : use theme_xx() function series to change background of the plot
  theme_classic() 

# facet: organized subplot by column
phenology %>% 
  ggplot(.,aes(x=var,y=value,fill=var))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size=0.2, aes(color=var))+
  facet_grid(Year~stage)+
  #facet_wrap(Year~stage)
  xlab("Cultivars")+
  ylab("Thermal Time (°Cd)")+
  ggtitle("Phenological Phases")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90, size=7),plot.title=element_text(size=20))

        