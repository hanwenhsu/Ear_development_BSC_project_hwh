rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggpol)
library(ggpubr)

readx<- function(p,sh){
  df <- readxl::read_xlsx(p,sheet = sh) %>% 
    mutate(across(starts_with("kernel"),function(x)as.character(x))) %>% 
    # long format for kernel type and floret position
    tidyr::pivot_longer(starts_with("kernel"),names_to = "kernel.type",values_to = "floret.pos") %>% 
    mutate(floret.pos=strsplit(floret.pos,",")) %>% 
    tidyr::unnest(floret.pos) %>% 
    mutate(floret.pos=as.numeric(floret.pos)%>%replace(., is.na(.), 0),
           # sort data
           kernel.size=factor(kernel.type,levels=paste0("kernel.",c("S","M","L"))) %>% as.numeric() %>% 
             # create contrast for kernel size
             ifelse(.==3,5,.)) %>%
    #separate spike section
    mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
    mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                  kernel.pos == 2 ~"central",
                                  T ~"apical")) %>%
    mutate(var = case_when(var == "Capone" ~"capone",
                           T ~ var)) # adjust case difference
}

# hypothesis 1
hypo1 <- function(df){
  p <- df %>%
    # 1. document kernel number
    mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
    mutate(kernel.size=NULL,
           floret.pos = NULL,
           kernel.type = NULL) %>%
    # 2. calculate mean kernel number on each spike for each treatment 
    # --> across reps & across batch = group by plot id and spike
    # (here only difference is treatment) --> Choice: all batches?
    group_by(plot_id,spike) %>%
    summarise(kernel.num = mean(kernel.num, na.rm = FALSE)) %>%
    mutate(kernel.num = ifelse(kernel.num == 0, NA, kernel.num)) %>%
    # restore position information
    ungroup()%>%
    mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
    mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                  kernel.pos == 2 ~"central",
                                  T ~"apical")) %>%
    # create treatment column
    mutate(sowing_date = ifelse(plot_id == 57, "early","late")) %>%
    group_by(plot_id)%>%
    # 3. plot data
    ggplot(aes(kernel.num,spike))+
    geom_point(size = 5, alpha = 0.5, col = "black",aes(shape = kernel.pos))+
    #geom_boxplot(aes(fill = kernel.pos))+
    geom_path(aes(col=sowing_date))+
    theme_classic()+
    theme(strip.background = element_blank(),
          strip.text.x  = element_text(size=20),
          panel.grid.major.y = element_line(linetype = "dashed"),
          legend.position = "bottom")
  return(p)
}

# hypothesis 2
hypo2 <- function(df){
  p <- df %>%
    # 1. document kernel number
    mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
    mutate(kernel.size=NULL) %>%
    # 2. calculate total kernel number of each kernel type
    #    on each spike for each treatment in each rep for each batch
    group_by(plot_id,spike,kernel.type,batch,rep) %>%
    reframe(var, kernel.num = sum(kernel.num, na.rm = FALSE), kernel.type) %>%
    # 3. calculate mean kernel number of each kernel type 
    #    on each spike
    group_by( spike, kernel.type) %>%
    reframe(plot_id,kernel.num = mean(kernel.num, na.rm=FALSE)) %>%
    # 4. restore kernel position    
    ungroup()%>%
    mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
    mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                  kernel.pos == 2 ~"central",
                                  T ~"apical")) %>%
    mutate(kernel.type = factor(kernel.type, levels = paste0("kernel.",c("S","M","L")))) %>%
    mutate(kernel.size=factor(kernel.type,levels=paste0("kernel.",c("S","M","L"))) %>% as.numeric() %>% 
             # create contrast for kernel size
             ifelse(.==3,5,.)) %>%
    unique() %>% #remove repetitive row
    mutate(kernel.num = ifelse(kernel.num == 0, NA, kernel.num)) %>%
    # 5. plot data 
    ggplot(aes(kernel.num, spike))+
    geom_path(alpha = 0.3)+
    geom_point(alpha=.5,aes(size=kernel.size,fill=kernel.pos),
               shape=21)+
    scale_fill_viridis(discrete = TRUE, option = "E")+
    scale_size_continuous(breaks = c(1, 2, 3), range = c(1, 12),
                          labels = paste0("kernel.",c("S","M","L")))+
    facet_grid(~kernel.type)+
    theme_classic()+
    theme(strip.background = element_rect(linewidth = 0.5),
          panel.grid.major.y = element_line(linewidth = 0.5),
          legend.position = "bottom")
  return(p)
}

# hypothesis 3

hypo3 <- function(df){
  p <- df %>%
    # 1. document kernel number
    mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
    mutate(kernel.size=NULL) %>%
    # 2. calculate total kernel number of each kernel type
    #    on each spike for each treatment in each rep for each batch
    group_by(plot_id,spike,kernel.type,batch,rep) %>%
    reframe(var, kernel.num = sum(kernel.num, na.rm = FALSE), kernel.type) %>%
    # 3. calculate mean kernel number of each kernel type 
    #    on each spikefor each batch
    group_by(spike, kernel.type,batch) %>%
    reframe(plot_id,kernel.num = mean(kernel.num, na.rm=FALSE)) %>% 
    # 4. restore kernel position    
    ungroup()%>%
    mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
    mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                  kernel.pos == 2 ~"central",
                                  T ~"apical")) %>%
    mutate(kernel.size=factor(kernel.type,levels=paste0("kernel.",c("S","M","L"))) %>% as.numeric() %>% 
             # create contrast for kernel size
             ifelse(.==3,5,.)) %>%
    unique() %>% #remove repetitive row
    mutate(kernel.num = ifelse(kernel.num == 0, NA, kernel.num)) %>%
    # 5. plot data
    ggplot(aes(kernel.num, spike))+
    geom_point(alpha=.8,aes(size=kernel.size,fill=kernel.size),
               shape=21)+
    geom_path()+
    facet_grid(kernel.type~batch)+
    scale_fill_viridis_c(guide = "legend",breaks = c(1, 2, 5),
                         labels = paste0("kernel.",c("S","M","L"))) +
    scale_size_continuous(breaks = c(1, 2, 3), range = c(1, 12),
                          labels = paste0("kernel.",c("S","M","L")))+
    theme_classic2()+
    theme(strip.background = element_rect(),
          panel.grid.major.y = element_line(linewidth = 0.5),
          legend.position = "bottom")
  return(p)
}

# -------------------------------------------------------------------------

#file name list for reading file
filelist <- list.files("./data/Grain_Counting") %>% paste0("./data/Grain_Counting/",.)

#read data
graindf <- purrr::map_dfr(1:10,~{
  x <- data.frame("var"=NA, "plot_id"=NA, "rep"=NA,
                  "spike"=NA,"flower"=NA,"kernel.type"=NA,
                  "floret.pos"=NA, "kernel.size"=NA, "kernel.pos" =NA,
                  "batch" =NA)
  for (i in 1:length(filelist)){
    df <- readx(filelist[i],.x) %>%
      mutate(batch = substr(filelist[i],nchar(filelist[i]) - 6,nchar(filelist[i])-5))
    x <- rbind(x,df) 
  }
  return(x[-1,]) 
})  #%>% na.omit()


# hypothesis 1 
# Sowing date affects distribution of grains between spike sections
graindf %>% hypo1()

# hypothesis 2
# Spike section affects grain size
graindf %>% hypo2()

# hypothesis 3
# Grains increase in sizes as it develops (--> across batch)
# but not the distribution of grains (Grain filling)
graindf %>% hypo3()


# grain set
# graindf %>%
#   # 1. document kernel number
#   mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
#   mutate(kernel.size=NULL) %>%
#   # 2. calculate total kernel number on each spike 
#   # for each treatment in each rep for each batch
#   group_by(plot_id,spike,batch,rep) %>%
#   reframe(var, kernel.num = sum(kernel.num, na.rm = FALSE),flower) %>%
#   # 3. calculate mean kernel number on each spike
#   # for each treatment
#   group_by(plot_id, spike) %>%
#   reframe(var, kernel.num = mean(kernel.num),flower = mean(flower)) %>%
#   # 4. calculate grain set rate
#   mutate(grain.set = ifelse(flower != 0, kernel.num/flower, 0)) %>%
#   # 5. restore kernel position    
#   mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
#   mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
#                                 kernel.pos == 2 ~"central",
#                                 T ~"apical")) %>%
#   
#   unique() %>% #remove repetitive row
#   # 6. plot data 
#   ggplot(aes(grain.set,spike))+
#   geom_point()+
#   geom_path()+
#   facet_grid(~plot_id)
#     