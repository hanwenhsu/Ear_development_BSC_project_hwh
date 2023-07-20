rm(list = ls())
library(dplyr)
library(viridis)
library(ggplot2)
library(ggpol)
library(ggpubr)

# student project SoSe 2023 by Han-Wen Hsu
# Data arrangement and plotting of collected data from grain counting

# data overview: 
## variety : capone
## 2 treatments : early and late sowing date
## 3 harvest time point (batch) = 3 developmental stages of spikes
## numbers of spikelet per spike (spike), grain size (kernel.type) 
## and grain position on spikelet (floret.pos) documented
## 10 repetition per variante

# goal: using ggplot to create overview visualisation and boxplot to test 
#       3 different hypothesis

# create function to read excel table of multiple sheets
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


# plotting function for hypothesis 1 - visualisation
# Sowing date affects distribution of grains between spike sections

hypo1 <- function(df){
  p <- df %>%
    # 1. document kernel number floret position > 0 --> 1 grain exists
    mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
    ## clear ununsed data
    mutate(kernel.size=NULL,
           floret.pos = NULL,
           kernel.type = NULL) %>%
    # 2. calculate mean kernel number on each spike for each treatment 
    # --> across reps & across batch = group by plot id and spike
    group_by(plot_id,spike,batch,rep) %>%
    summarise(kernel.num = sum(kernel.num)) %>%
    ## calculate standard deviation and standard error for error bars
    group_by(plot_id,spike) %>%
    summarise(mean.kernel.num = mean(kernel.num, na.rm = FALSE),
              sd.kernel.num = sd(kernel.num)) %>%
    mutate(se.kernel.num = sd.kernel.num / sqrt(60)) %>%
    mutate(mean.kernel.num = ifelse(mean.kernel.num == 0, NA, mean.kernel.num)) %>%
    ## restore position information lost through summarise()
    ungroup()%>%
    mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
    mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                  kernel.pos == 2 ~"central",
                                  T ~"apical")) %>%
    # 3. create treatment column
    mutate(treatment = ifelse(plot_id == 57, "early","late")) %>%
    group_by(plot_id)%>%
    # 4. plot data
    ggplot(aes(mean.kernel.num,spike))+
    geom_point(size = 3, alpha = 0.5, col = "black",aes(shape = kernel.pos))+
    ## path for visualisation
    geom_path(aes(col=treatment), linewidth = 1.2)+
    ## create error bars
    geom_errorbar(aes(xmin = mean.kernel.num - sd.kernel.num, 
                      xmax = mean.kernel.num + sd.kernel.num),
                  width = 0.2, alpha=0.3)+
    labs(title = "Comparison between effect of two sowing dates on distribution of grains",
         caption = "Batch: 01, 06, 11")+
    xlab("Average Grain Number")+
    ylab("Spike Position")+
    theme_classic()+
    theme(strip.background = element_blank(),
          plot.title = element_text(size=12,hjust = 0.5,face="bold"),
          axis.title = element_text(face = "bold", size=15, vjust =1),
          axis.text = element_text(size = 12, vjust=0.5),
          panel.grid.major.y = element_line(linetype = "dashed"),
          legend.position = "bottom",
          legend.text = element_text(size = 15))
  return(p)
}

# plotting function for hypothesis 1 --> statistical plot
hypo1_stat <- function(df){
  p <- df %>%
    # 1. document kernel number floret position > 0 --> 1 grain exists
    mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
    ## clear unused data
    mutate(kernel.size=NULL,
           floret.pos = NULL,
           kernel.type = NULL) %>%
    # 2. calculate total kernel number on each spike for each treatment 
    # in each rep of each batch for boxplot and test
    group_by(plot_id,rep,spike,batch) %>%
    summarise(kernel.num = sum(kernel.num, na.rm = FALSE)) %>%
    # restore position information lost through summarise()
    ungroup()%>%
    mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
    mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                  kernel.pos == 2 ~"central",
                                  T ~"apical")) %>%
    mutate(kernel.pos = factor(kernel.pos, 
                               levels = c("basal","central","apical"))) %>%
    # 3. create treatment column
    mutate(treatment = ifelse(plot_id == 57, "early","late")) %>%
    group_by(plot_id)%>%
    # 4. boxplot and t-test
    ggplot(aes(treatment,kernel.num))+
    geom_boxplot(alpha = 0.7, aes(fill = treatment))+
    geom_jitter(alpha = 0.4, size = 0.7,position = position_jitter(0.1),shape=3)+
    facet_grid(~kernel.pos)+
    # add test result to boxplot
    stat_compare_means(method="t.test")+
    labs(title = "Comparison between two treatments in distribution of grains",
         subtitle = "Is there a difference in mean grain number between treatment",
         caption = "Batch: 01, 06, 11")+
    ylab("Grain Number")+
    theme_classic()+
    theme(strip.background = element_rect(),
          strip.text  = element_text(size=12),
          plot.title = element_text(size=12,hjust = 0.5,face="bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title = element_text(face = "bold",size=15),
          axis.text.x = element_text(angle = 45, vjust = 0.5,size = 12),
          axis.text.y = element_text(size =12),
          panel.grid.major.y = element_line(linetype = "dashed"),
          legend.position = "bottom",
          legend.text = element_text(size=15))
  return(p)
}

# plotting for hypothesis 2 --> visualisation
# Spike section affects distribution of different sized grains

hypo2 <- function(df){
  p <- df %>%
    # 1. document kernel number floret position > 0 --> 1 grain exists
    mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
    mutate(kernel.size=NULL) %>%
    # 2. calculate total kernel number of each kernel type
    #    on each spike for each treatment in each rep for each batch
    group_by(plot_id,spike,kernel.type,batch,rep) %>%
    summarise(kernel.num = sum(kernel.num, na.rm = FALSE)) %>%
    # 3. calculate mean kernel number of each kernel type 
    #    on each spike
    group_by(spike, kernel.type) %>%
    summarise(mean.kernel.num = mean(kernel.num, na.rm=FALSE),
              ## calculate standard deviation and standard error for errorbars
              sd.kernel.num = sd(kernel.num)) %>% 
    mutate(se.kernel.num = sd.kernel.num / sqrt(60)) %>%
    ungroup() %>%
    # 4. restore kernel position    
    mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
    mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                  kernel.pos == 2 ~"central",
                                  T ~"apical")) %>%
    #text order for grain size S>M>L
    mutate(kernel.type = factor(kernel.type, levels = paste0("kernel.",c("S","M","L")))) %>%
    mutate(kernel.size=factor(kernel.type,levels=paste0("kernel.",c("S","M","L"))) %>% as.numeric() %>% 
             # create contrast for kernel size
             ifelse(.==3,5,.)) %>%
    unique() %>% #remove repetitive rows
    # 5. plot data 
    ggplot(aes(mean.kernel.num, spike))+
    ## path for visualisation
    geom_path(alpha = 0.3)+
    geom_point(alpha=.5,aes(size=kernel.size,fill=kernel.pos),
               shape=21)+
    geom_errorbar(aes(xmin = mean.kernel.num - sd.kernel.num, 
                      xmax = mean.kernel.num + sd.kernel.num),
                  width = 0.2, alpha=0.3)+
    scale_fill_viridis(discrete = TRUE, option = "E")+
    scale_size_continuous(breaks = c(1, 2, 3), range = c(1, 12),
                          labels = paste0("kernel.",c("S","M","L")))+
    facet_grid(~kernel.type)+
    labs(title = "Comparison between distribution of different sized grains",
         caption = "Batch: 01, 06, 11")+
    xlab("average grain number")+
    ylab("Spike position")+
    theme_classic()+
    theme(strip.background = element_rect(linewidth = 0.5),
          strip.text.x = element_text(size = 15),
          panel.grid.major.y = element_line(linewidth = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size=12),
          axis.title = element_text(size=15, face = "bold"),
          axis.text = element_text(size = 15, vjust = 0.5),
          plot.title = element_text(size = 12, face = "bold",hjust = 0.5))
  return(p)
}

# plotting for hypothesis 2 --> statistical plot

hypo2_stat <- function(df){
  p <- df %>%
    # 1. document kernel number floret position > 0 --> 1 grain exists
    mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
    mutate(kernel.size=NULL) %>%
    # 2. calculate total kernel number of each kernel type
    #    on each spike for each treatment in each rep for each batch
    #    for boxplot and statistical tests
    group_by(plot_id,spike,kernel.type,batch,rep) %>%
    summarise(kernel.num = sum(kernel.num, na.rm = FALSE)) %>%
    ungroup() %>%
    # 3. restore kernel position    
    mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
    mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                  kernel.pos == 2 ~"central",
                                  T ~"apical")) %>%
    #text order for position basal>central>apical
    mutate(kernel.pos = factor(kernel.pos, levels = c("basal","central","apical"))) %>%
    mutate(kernel.type = factor(kernel.type, levels = paste0("kernel.",c("S","M","L")))) %>%
    unique() %>% #remove repetitive row 
    #filter(batch == "11") %>% # for plot only with batch 11
    # 5. plot data 
    ggplot(aes(kernel.pos, kernel.num))+
    geom_boxplot(alpha = 0.7, aes(fill = kernel.pos))+
    # add test result to boxplot
    stat_compare_means(method = "t.test", ref.group = "central", 
                       method.args = list(alternative="l"),
                       label = "p.signif",
                       label.y.npc = 0.9)+
    stat_compare_means(method = "anova",
                       label.x.npc = 0.4)+
    facet_grid(~kernel.type)+
    labs(title="Comparison between spike sections in distribution of different sized grains",
         caption = "Batch: 01, 06, 11")+
    # labs(title="Comparison between spike sections in distribution of different sized grains in batch 11",
    #      subtitle = "Alt: The basal or apical part has less grains than the central part",
    #      caption = "Batch: 11")+
    xlab("Spike Section")+
    ylab("Grain Number")+
    theme_classic()+
    theme(strip.background = element_rect(linewidth = 0.5),
          strip.text.x = element_text(size = 15),
          panel.grid.major.y = element_line(linewidth = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size=15),
          axis.title = element_text(size=15, face = "bold"),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          plot.title = element_text(size = 10, face = "bold",hjust = 0.5),
          plot.subtitle = element_text(hjust=0.5))
  return(p)
}

# plotting for hypothesis 3 --> visualisation
# Number of larger grains increases as the spike develops 
# but the distribution trend remains consistent

hypo3 <- function(df){
  p <- df %>%
    # 1. document kernel number floret position > 0 --> 1 grain exists
    mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
    mutate(kernel.size=NULL) %>%
    # 2. calculate total kernel number of each kernel type
    #    on each spike for each treatment in each rep for each batch
    group_by(plot_id,spike,kernel.type,batch,rep) %>%
    reframe(var, kernel.num = sum(kernel.num, na.rm = FALSE), kernel.type) %>%
    # 3. calculate mean kernel number, Standard Deviation, 
    #    Standard Error of each kernel type (error bars)
    #    on each spike for each batch
    group_by(spike, kernel.type,batch) %>%
    reframe(plot_id,mean.kernel.num = mean(kernel.num, na.rm=FALSE),
            sd.kernel.num = sd(kernel.num)) %>%
    mutate(se.kernel.num = sd.kernel.num / sqrt(20)) %>%
    # 4. restore kernel position    
    ungroup()%>%
    mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
    mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                  kernel.pos == 2 ~"central",
                                  T ~"apical")) %>%
    ## text order for kernel size S>M>L
    mutate(kernel.type = factor(kernel.type, levels = paste0("kernel.",c("S","M","L")))) %>%
    mutate(kernel.size=factor(kernel.type,levels=paste0("kernel.",c("S","M","L"))) %>% as.numeric() %>% 
             # create contrast for kernel size
             ifelse(.==3,5,.)) %>%
    unique() %>% #remove repetitive row
    mutate(mean.kernel.num = ifelse(mean.kernel.num == 0, NA, mean.kernel.num)) %>%
    #mutate(batch = factor(batch, levels = c("11","06","01"))) %>%
    # 5. plot data
    ggplot(aes(mean.kernel.num, spike))+
    geom_point(alpha=.5,aes(size=kernel.size,fill=kernel.size,shape=batch))+
    geom_errorbar(aes(xmin = mean.kernel.num - sd.kernel.num,
                      xmax = mean.kernel.num + sd.kernel.num),
                  width=0.2, alpha = 0.3)+
    geom_path(aes(group = batch))+
    facet_grid(~kernel.type)+
    scale_fill_viridis_c(guide = "legend",breaks = c(1, 2, 3),
                         labels = paste0("kernel.",c("S","M","L"))) +
    scale_size_continuous(breaks = c(1, 2, 3), range = c(1, 12),
                          labels = paste0("kernel.",c("S","M","L")))+
    scale_color_viridis_b(guide = "legend",breaks = c(1, 2, 3),
                          labels = paste0("kernel.",c("S","M","L")))+
    scale_shape_manual(values = c(20,21,23))+
    labs(title = "Distribution of different sized grains in 3 batches",
         caption = "Batch: 01, 06, 11")+
    xlab("average grain number")+
    ylab("Spike position")+
    theme_classic2()+
    theme(strip.background = element_rect(linewidth = 0.5),
          strip.text.x = element_text(size = 15),
          panel.grid.major.y = element_line(linewidth = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size=12),
          axis.title = element_text(size=15, face = "bold"),
          axis.text = element_text(size = 15, vjust = 0.5),
          plot.title = element_text(size = 12, face = "bold",hjust = 0.5))
  return(p)
}

# plotting for hypothesis 3 --> Statistical plot
## comparison amount of different sized grains between batches

hypo3_stat1 <- function(df){ 
  p <- df %>%
    # 1. document kernel number
    mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
    mutate(kernel.size=NULL) %>%
    # 2. calculate total kernel number of each kernel type
    #    on each spike for each treatment in each rep for each batch
    group_by(plot_id,spike,kernel.type,batch,rep) %>%
    summarise(kernel.num = sum(kernel.num, na.rm = FALSE)) %>%
    ungroup() %>%
    # 3. restore kernel position    
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
    #mutate(kernel.num = ifelse(kernel.num == 0, NA, kernel.num)) %>%
    filter(batch != "01") %>%
    # 4. plot data 
    ggplot(aes(batch, kernel.num))+
    geom_boxplot(alpha = 0.7, aes(fill = batch))+
    ## add test result to boxplot
    stat_compare_means(method = "t.test", ref.group = "06",
                       method.args = list(alternative="greater"),
                       label = "p.signif")+
    facet_grid(~kernel.type)+
    labs(title = "Comparison in distribution of grain size between batches",
         subtitle = "Alt: Spikes from batch 11 has more grains of size S/M/L than those of batch 6",
         caption = "Batch: 06, 11")+
    xlab("Batch Number")+
    ylab("Spike Number")+
    theme_classic()+
    theme(strip.background = element_rect(linewidth = 0.5),
          strip.text.x = element_text(size = 15),
          panel.grid.major.y = element_line(linewidth = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size=15),
          axis.title = element_text(size=15, face = "bold"),
          axis.text = element_text(size = 15, vjust = 0.5),
          plot.title = element_text(size = 12, face = "bold",hjust = 0.5),
          plot.subtitle = element_text(hjust=0.5))
  return(p)
}

hypo3_stat2 <- function(df){ # comparison grain distribution between batches
  p <- df %>%
    # 1. document kernel number floret position != 0 -> 1 grain exists
    mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
    mutate(kernel.size=NULL) %>%
    # 2. calculate total kernel number of each kernel type
    #    on each spike for each treatment in each rep for each batch
    group_by(plot_id,spike,kernel.type,batch,rep) %>%
    summarise(kernel.num = sum(kernel.num, na.rm = FALSE)) %>%
    ungroup() %>%
    # 3. restore kernel position    
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
    mutate(kernel.pos = factor(kernel.pos, levels = c("basal","central", "apical"))) %>%
    filter(batch != "01") %>%
    # 4. plot data 
    ggplot(aes(kernel.pos, kernel.num))+
    geom_boxplot(alpha = 0.7, aes(fill = batch))+
    ## add test result to boxplot
    stat_compare_means(method = "anova",
                       label.x.npc = 0.4)+
    stat_compare_means(method = "t.test", ref.group = "central",
                       method.args = list(alternative="less"),
                       label.y.npc = 0.9, label = "p.signif")+
    facet_grid(~batch)+
    labs(title = "Comparison in distribution of grains across spike sections between batches",
         subtitle = "Alt: The basal or apical part has less grains than the central part",
         caption = "Batch: 06, 11")+
    xlab("Spike Section")+
    ylab("Spike Number")+
    theme_classic()+
    theme(strip.background = element_rect(linewidth = 0.5),
          strip.text.x = element_text(size = 15),
          panel.grid.major.y = element_line(linewidth = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size=15),
          axis.title = element_text(size=15, face = "bold"),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          plot.title = element_text(size = 12, face = "bold",hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  return(p)
}

# -------------------------------------------------------------------------

#file name list for reading file
filelist <- list.files("./data/Grain_Counting") %>% paste0("./data/Grain_Counting/",.)

#read data with loop due to multiple data and multiple sheets
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
## plot visualisation
graindf %>% hypo1()
## plot boxplot
graindf %>% hypo1_stat()

# hypothesis 2
# Spike section affects grain size
## plot visualisation
graindf %>% hypo2()
## plot boxplot
graindf %>% hypo2_stat()

# hypothesis 3
# Grains increase in sizes as it develops 
# but not the distribution of grains (Grain filling)
## plot visualisation
graindf %>% hypo3()
## plot boxplot
graindf %>% hypo3_stat1()
graindf %>% hypo3_stat2()

