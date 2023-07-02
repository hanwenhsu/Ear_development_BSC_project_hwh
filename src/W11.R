rm(list=ls())

library(dplyr)
library(magrittr)
library(purrr)
library(ggplot2)

df<- map_dfr(list.files("./data/student"),~{
  
  student_name <-  .x %>% strsplit("_") %>% unlist() %>% 
    .[4] %>% sub(".xlsx","",.)
  
  file<- readxl::read_xlsx(paste0("./data/student/",.x)) %>%  
    `colnames<-`(stringr::str_to_lower(names(.)))%>% 
    `colnames<-`(gsub("kernal","kernel",names(.))) %>% 
    `colnames<-`(gsub("spikes","spike",names(.)))%>%
    `colnames<-`(gsub("plot.id","plot_id",names(.))) %>% 
    mutate(student=student_name)
}) 

df %<>% mutate(var="Capone",plot_id=159) %>% 
  .[!grepl("na.",names(.))]
df %>% glimpse()

# how to make it more beautiful

ggplot(data=df, aes(x=flower, y=spike,color=student))+
  geom_path()+
  geom_point()+
  facet_grid(~student)+
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank(),
        panel.grid.major.x = element_line())

# How to place kernel-related traits in subplots?
# long format

df_long <- df %>% tidyr::pivot_longer(names_to = "kernel_terms",
                                        values_to = "kernel_values",
                                        cols = contains("kernel"))
df_long %>% 
  group_by(student,spike) %>%
  ggplot(aes(kernel_values, spike, color = student))+
  geom_point()+
  geom_path(alpha=.5)+
  facet_grid(vars(kernel_terms),vars(student))+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "none")

# classify spikelet based on position
# how to change the order of the boxplot

df <- df %>% 
  group_by(student) %>%
  mutate(type = as.numeric(cut(spike, breaks=3))) %>% 
  mutate(type = case_when(type == 1 ~ "basal", 
                          type == 2 ~ "central",
                          T ~ "apical")) %>%
  mutate(type = factor(type,levels = c("basal", "central", "apical")))

glimpse(df)
levels(df$type)

library(ggpol)
p <- df %>% # p: save the result of the plot
  ggplot(aes(type,flower,fill=student))+
  geom_boxjitter(aes(color=student),alpha=.4,
                 jitter.shape = 21, jitter.color = NA, 
                 jitter.params = list(height = 0, linewidth = 0.04),
                 outlier.color = NA, errorbar.draw = TRUE)+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom") 

print(p)

# summarize
# basic summary of kernel development sum for single spike
# 
# Sp: total spikelet
# Fl: maximum floret
# sfl: total floret
# kf: total full kernel
# kh: total half kernel
# ks: total small kernel
# kp: potential kernel number (half+pull)
# fr: filling rate (full kernel/ floret number)
# fc: potential filling rate (potential kernel number/ floret number)

df %>% 
  group_by(var, plot_id,student,type) %>%
  mutate(kernel.small = ifelse(is.na(kernel.small),0,0)) %>%
  summarise(
    Sp = max(spike), #total spikelet
    Fl = max(flower), #maximum floret
    sfl = sum(flower), #total floret
    kf = sum(kernel.full), #total full kernel
    kh = sum(kernel.half), #total half kernel
    ks = sum(kernel.small), #total small kernel
    kp = sum(kernel.full,kernel.half), #potential kernel number (half+full)
    fr = kf / sfl, #filling rate (full kernel/ floret number)
    fc = kp / sfl #potential filling rate (potential kernel number/ floret number)
  )

# how to visualize the maximum full kernels per spikelet?
# extra groupping with spike

df %>% 
  group_by(var, plot_id,student,type,spike) %>%
  summarize(
    fertil_flower = max(kernel.full)
  ) %>%
  ggplot(aes(fertil_flower,spike,color = type))+
  geom_path()+
  geom_point()+
  facet_grid(~student)+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom")

# practice on a larger data set

# read kernel_combine.csv in folder data using relative path.
# subset column tiller which match the pattern M.

df_kn <- read.csv("./data/kernel_combine.csv")

df_kn %>%
  filter(tiller == "M")

# how to get overview of unique combinations?
# How can you get the unique combination of car,var,nitrogen and time? 
# Which combinations of functions can you use?

df_kn %>%
  select(car,nitrogen,time,var) %>%
  unique() %>% glimpse()

df_kn %>%
  group_by(car,nitrogen,time,var) %>%
  summarise()

# classify spikelet based on position
colnames(df_kn)

df_kn <- df_kn %>%
  mutate(type = as.numeric(cut(spike, breaks = 3))) %>%
  mutate(type = case_when(type == 1 ~"basal",
                          type == 2 ~"central",
                          T ~"apical"))

# basic summary of kernel development summ for single spike  

df_kn %>% 
  group_by(car,nitrogen,time,var,rep,tiller) %>%
  summarise(
    Sp = max(spike), #total spikelet
    Fl = max(flower), #maximum floret
    sfl = sum(flower), #total floret
    kf = sum(kernel.full), #total full kernel
    kh = sum(kernel.half), #total half kernel
    ks = sum(kernel.small), #total small kernel
    kp = sum(kernel.full,kernel.half), #potential kernel number (half+full)
    fr = kf / sfl, #filling rate (full kernel/ floret number)
    fc = kp / sfl #potential filling rate (potential kernel number/ floret number)
  )

# Data wrangling and plot with facet

# step1 create long format
# create long format long_format, combine nitrogen and time, 
# names to “treatment” values to “levels”
# for each rep,levels,type,var,spike,treatment 
# calculate the maximum full kernel and name it fertile_flower.
# for each levels,type,var,spike,treatment calculate the mean fertile_flower.

df_kn_long <- df_kn %>%
  tidyr::pivot_longer(cols = c(nitrogen,time),
                      names_to = "treatment",
                      values_to = "levels"
                      )

df_kn_long <- df_kn_long %>%
  group_by(rep,levels,type,var,spike,treatment) %>%
  mutate(fertile_flower = max(kernel.full))

df_kn_long <- df_kn_long %>%
  group_by(levels,type,var,spike,treatment) %>%
  mutate(fertile_flower = mean(fertile_flower))

# step2 visualize
# subset the value of fertile_flower less than 10
# based on this graph, what is x, y, color and shape?
# what re the facet?

df_kn_long %>%
  filter(fertile_flower < 10) %>%
  ggplot(aes(fertile_flower,spike,color=type, shape=levels))+
  geom_point()+
  facet_grid(treatment~var)+
  theme_classic()+
  theme(strip.background = element_rect(),
        panel.grid.major.x = element_line(),
        )
