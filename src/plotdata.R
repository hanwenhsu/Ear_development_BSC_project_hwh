rm(list = ls())
library(dplyr)
library(ggplot2)
library(viridis)

readx<- function(p,sh){
  df <- readxl::read_xlsx(p,sheet = sh) %>% 
    mutate(across(starts_with("kernel"),function(x)as.character(x))) %>% 
    tidyr::pivot_longer(starts_with("kernel"),names_to = "kernel.type",values_to = "floret.pos") %>% 
    mutate(floret.pos=strsplit(floret.pos,",")) %>% 
    tidyr::unnest(floret.pos) %>% 
    mutate(floret.pos=as.numeric(floret.pos) %>%replace(., .==0, NA),
           kernel.size=factor(kernel.type,levels=paste0("kernel.",c("S","M","L"))) %>% as.numeric() %>% 
             # create contrast
             ifelse(.==3,5,.))
}

plot_fun <- function(df){
  # find floret where more than two types were recorded for same position  
  sdf <- df %>% group_by(rep,spike,floret.pos) %>% 
    summarise(n=n()) %>% 
    filter(n>1) %>% 
    mutate(lab=paste0("Check\n(Nsp,Nf)\n(",spike,',',floret.pos,')'))
  
  p <- df %>%
    ggplot(aes(floret.pos,spike))+
    geom_point(alpha=.4,aes(size=kernel.size,fill=kernel.size),
               shape=21)+
    facet_wrap(~rep)+
    theme_bw()+
    ggrepel::geom_label_repel(data = sdf, mapping=aes(floret.pos,spike, label = lab),color="red",
                              box.padding = 1,size=3)+
    scale_fill_viridis_c(guide = "legend",breaks = c(1, 2, 5),
                         labels = paste0("kernel.",c("S","M","L"))) +
    scale_size_continuous(breaks = c(1, 2, 3), range = c(1, 5),
                          labels = paste0("kernel.",c("S","M","L")))+
    theme(panel.grid.minor.x=element_blank(),strip.background = element_blank(),
          legend.position = "bottom")
  return(p)
  
}

hypo1 <- function(df){
  p <- df %>%
    group_by(plot_id,rep,spike) %>%
    summarise(kernel.num = sum(length(kernel.type))) %>%
    group_by(plot_id,spike) %>%
    summarise(kernel.num = mean(kernel.num)) %>%
    group_by(plot_id) %>%
    mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
    mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                  kernel.pos == 2 ~"central",
                                  T ~"apical")) %>%
    mutate(treatment = ifelse(plot_id == 57, "early","late")) %>%
    ungroup()%>%
    ggplot(aes(kernel.num,spike))+
    geom_path(alpha = 0.3,linewidth=0.3)+
    geom_point(size = 1.5,aes(col=kernel.pos))+
    facet_grid(~treatment)+
    theme_bw()+
    theme(strip.background = element_blank(),
          panel.grid.major.x = element_line(),
          legend.position = "bottom")
  return(p)
}
# -------------------------------------------------------------------------
# p <- "data/Grain_Counting/gc_57_11.xlsx"
# graindf <- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
#   readx(p,.x)
# }) %>% na.omit() 
# 
# graindf %>% plot_fun()

filelist <- list.files("./data/Grain_Counting") %>% paste0("./data/Grain_Counting/",.)

graindf <- purrr::map_dfr(1:10,~{
  x <- data.frame("var"=NA, "plot_id"=NA, "rep"=NA,
                  "spike"=NA,"flower"=NA,"kernel.type"=NA,
                  "floret.pos"=NA, "kernel.size"=NA)
  for (i in 1:length(filelist)){
    df <- readx(filelist[i],.x)
    x <- rbind(x,df)
  }
  return(x)
}) %>% .[-1,] %>% na.omit()


graindf %>%    
  group_by(plot_id,rep,spike) %>%
  summarise(kernel.num = sum(length(kernel.type))) %>%
  group_by(plot_id,spike) %>%
  summarise(kernel.num = mean(kernel.num)) %>%
  group_by(plot_id) %>%
  mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
  mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                kernel.pos == 2 ~"central",
                                T ~"apical")) %>%
  mutate(treatment = ifelse(plot_id == 57, "early","late")) %>%
  ungroup()%>%
  ggplot(aes(kernel.num,spike))+
  geom_path(aes(col=treatment))+
  geom_point(size = 1.5)+
  theme_bw()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom")

graindf %>% hypo1()
