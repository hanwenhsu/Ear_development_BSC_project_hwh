rm(list=ls())

library(dplyr)
library(ggplot2)


# list the file names
path <- "./data/student/"
filenames <- list.files(path,pattern=".xlsx")
#create empty list
df <- vector(mode="list",length=length(filenames))

for(i in 1:length(filenames)){
  # create path to read the file
  fullpath <- paste0(path,filenames[i])
  # read file %>% get column names
  # -> save column names to df (list) to the according list
  df[[i]] <- readxl::read_xlsx(fullpath) %>% 
    names()
}

df 

# How to name list elements based on student’s name ?
# 1. split the file names
file_list <- purrr::map(filenames,  ~{
  .x %>% strsplit("_") %>% unlist()
})
# 2. get student name -> strsplit() and gsub()
# 3. rename list element -> names()
for (i in length(file_list)){
  names(df)[i] <- gsub(".xlsx","", file_list[[i]][4])
}

#answer from TC
student_name <-   purrr::map_chr(filenames,  ~{
  .x %>% strsplit("_") %>% unlist() %>% 
    .[4] %>% sub(".xlsx","",.)
}) 
names(df) <-student_name
df
# difference between sub() & gsub() -> g = global

# How to combine all the dataframe by row?

df2<- purrr::map_dfr(list.files("./data/student"),~{
  file<- readxl::read_xlsx(paste0("./data/student/",.x))
  # replace "Kernal"
  names(file) <- names(file) %>% gsub("kernal","kernel",., ignore.case = TRUE)
  # replace "Spikes"
  names(file) <- names(file) %>% gsub("Spikes","spike",., ignore.case = TRUE)
  # replace "Plot_Id"
  names(file) <- names(file) %>% gsub("Plot_Id","plot_id",., ignore.case = TRUE)
  # replace "plot.id"
  names(file) <- names(file) %>% gsub("plot.id","plot_id",., ignore.case = TRUE)
  # replace "Var"
  names(file) <- names(file) %>% gsub("Var","var",., ignore.case = TRUE)
  # fill cultivar and plot_id
  file <- file %>% mutate(var = "capone") %>% mutate(plot_id = 159)
  # student name as column
  st_name <- strsplit(.x,"_")[[1]][4] %>% gsub(".xlsx","",.)
  file <- file %>% mutate(student_name = st_name)
  return(file)
})
df2 %>% 
  glimpse()

# Problem:
# 1. cases
# 2. typos: "kernal", "plot.id", "spikes"
# 3. Student names as column
# 4. fill missing cultivar and plot_id
# 5. remove "na." column


# How to visualize the result?
ggplot(data=df2, aes(x=flower, y=spike,color=student))+
  geom_path()+
  geom_point()+
  theme_classic()+
  theme(legend.position = "bottom")

