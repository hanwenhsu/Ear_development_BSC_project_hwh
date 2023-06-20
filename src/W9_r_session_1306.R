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
ggplot(data=df2, aes(x=flower, y=spike,color=student_name))+
  geom_path()+
  geom_point()+
  theme_classic()+
  theme(legend.position = "bottom")

# How to make it a bit more beautiful?

ggplot(data=df2, aes(x=flower, y=spike,color=student_name))+
  geom_path()+
  geom_point()+
  facet_grid(~student_name)+
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank(),
        panel.grid.major.x = element_line())

# How to place kernel-related traits in subplots?
# long format

df2_long <- df2 %>% tidyr::pivot_longer(names_to = "kernel_terms",
                                        values_to = "kernel_values",
                                        cols = contains("kernel"))
df2_long %>% 
  group_by(student_name,spike) %>%
  ggplot(aes(kernel_values, spike, color = student_name))+
  geom_point()+
  geom_path(alpha=.5)+
  facet_grid(vars(kernel_terms),vars(student_name))+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "none")

# challenge : add new column called type using mutate()
# 1. group the spikes into apical central basal using cut
# 2. convert result from #1 to numeric indices
# 3. create new column by case_when (1~basal, 2~central, T~apical)
# 3. (with groups based on student_name)

df2 <- df2 %>% 
  group_by(student_name) %>%
  mutate(type = as.numeric(cut(spike, breaks=3))) %>% 
  mutate(type = case_when(type == 1 ~ "basal", 
                          type == 2 ~ "central",
                          T ~ "apical")) %>%
  mutate(type = factor(type,levels = c("basal", "central", "apical")))

glimpse(df2)
levels(df2$type)

library(ggpol)
p <- df2%>% # p: save the result of the plot
  ggplot(aes(type,flower,fill=student_name))+
  geom_boxjitter(aes(color=student_name),alpha=.4,
                 jitter.shape = 21, jitter.color = NA, 
                 jitter.params = list(height = 0, width = 0.04),
                 outlier.color = NA, errorbar.draw = TRUE)+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom") 

print(p)
