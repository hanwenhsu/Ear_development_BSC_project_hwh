rm(list = ls())
library(dplyr)
library(ggplot2)

p <- "./data/Grain_Counting/gc_57_1.xlsx"
df <- readxl::read_xlsx(p, sheet=1) %>% 
  mutate(across(starts_with("kernel"),function(x)as.character(x))) %>% 
  tidyr::pivot_longer(starts_with("kernel"),
                      names_to = "kernel.type",
                      values_to = "floret.pos") %>% 
  mutate(floret.pos=strsplit(floret.pos,",")) %>% 
  rowwise() %>%
  tidyr::unnest(floret.pos) %>% 
  mutate(floret.pos=as.numeric(floret.pos) %>%replace(., is.na(.), 0)) 

#mutate(floret.pos = factor(floret.pos, levels = c(3,2,1,0))) %>%
#mutate(kernel.type = factor(kernel.type, levels = c("kernel.S","kernel.M","kernel.L")))

df%>%
  ggplot(aes(x=flower, y=spike))+
  geom_path()+
  geom_point()+
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank(),
        panel.grid.major.x = element_line())

df %>%
  filter(floret.pos != 0) %>%
  ggplot( aes(x=floret.pos, y=spike, size = kernel.type, color = kernel.type)) +
  geom_point(alpha=0.7) +
  #scale_size(range = c(1.4, 19), name="Population (M)") +
  #scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_bw() +
  theme(legend.position="bottom")


df_multi <- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  file<- readxl::read_xlsx(p, sheet = .x)
  file <- file %>% mutate(across(starts_with("kernel"),function(x)as.character(x))) 
  file <- file %>% tidyr::pivot_longer(starts_with("kernel"),names_to = "kernel.type",values_to = "floret.pos") 
  file <- file %>% mutate(floret.pos=strsplit(floret.pos,","))  
  file <- file %>% tidyr::unnest(floret.pos) 
  file <- file %>% mutate(floret.pos=as.numeric(floret.pos) %>%replace(., is.na(.), 0))
  # file <- file %>% mutate(kernel.type = factor(kernel.type, 
  #                                              levels = c("kernel.S","kernel.M","kernel.L"),
  #                                              ordered = TRUE))
  return(file)
})

#library(viridis)

df_multi %>%
  ggplot(aes(x=flower, y=spike,color = rep)) +
  geom_path()+
  geom_point(alpha = 0.7) +
  facet_grid(~rep)+
  theme_bw() +
  scale_fill_viridis_c(guide="legend",breaks=c(1,2,5))+
  theme(legend.position="bottom",
        strip.background = element_blank(),
        panel.grid.major = element_line())

