library(dplyr)
library(ggplot2)

p <- "./data/Grain_Counting/gc_57_11.xlsx"
df <- readxl::read_xlsx(p, sheet=5) %>% 
  mutate(across(starts_with("kernel"),function(x)as.character(x))) %>% 
  tidyr::pivot_longer(starts_with("kernel"),names_to = "kernel.type",values_to = "floret.pos") %>% 
  mutate(floret.pos=strsplit(floret.pos,",")) %>% 
  tidyr::unnest(floret.pos) %>% 
  mutate(floret.pos=as.numeric(floret.pos) %>%replace(., is.na(.), 0)) #%>%
#mutate(floret.pos = factor(floret.pos, levels = c(3,2,1,0))) %>%
#mutate(kernel.type = factor(kernel.type, levels = c("kernel.S","kernel.M","kernel.L")))

df %>%
  ggplot(aes(x=floret.pos, y=spike, color = kernel.type))+
  geom_path()+
  geom_point()+
  facet_grid(~kernel.type)+
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank(),
        panel.grid.major.x = element_line())
