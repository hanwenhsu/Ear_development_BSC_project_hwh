library(dplyr)

# More mutate examples

df <- expand.grid(x=letters[1:4],
                  y=1:2)  # combine vectors as dataframe

df%>% mutate(paste(x,y))
df%>% mutate(z=paste(x,y))
df%>% mutate(z=paste(x,y,sep = "-")) # sep: Separator

# Unite multiple columns into one by pasting strings together
df %>% tidyr::unite(data = .,col = "z",c(x,y))#, remove=FALSE) 

df <- df %>% mutate(z=interaction(x,y))

# add identifier based on row numbers
df %>% mutate(id=1:n()) # n() : gives current group size
df %>% mutate(id=1:nrow(.))
# row names
rownames(df)
df %>% filter()
rownames(df) <- LETTERS[1:nrow(df)]
rownames(df)

# Challenge 1 subset the row where (x equals to “a”, y equals to 1) 
# or (x equals to c, y equals to 2) 1. How many ways to achieve this? 
# 2. Observe the row names, are they the same before and after subseting?

df %>% filter(x=="a" & y==1 | x=="c" & y == 2) 
# filter with logical operators
# & -> and / | -> or

df %>% filter(z%in%c("a.1", "c.2"))  # indexing using merged columns
