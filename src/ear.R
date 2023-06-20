library(dplyr)
library(ggplot2)
library(ggpol)


ear <- read.csv("data/ear_raw.csv") %>% 
  mutate(weight=Weight/Ear_numb,
         date=as.character(date) %>% 
           as.Date(.,format="%Y%m%d"))

ear %>% 
  filter(var%in%c('Potenzial',"Pionier",'Capone','Patras')) %>% 
  group_by(timeid,nitrogen,var,appl) %>% 
  summarise(plot_id=unique(plot_id))

ggplot(ear%>% 
         filter(var%in%c('Potenzial',"Pionier",'Capone','Patras'),
                # nitrogen==176&appl=='Split',
                # date%in%as.Date(c('2021-06-14','2021-06-21','2021-07-01','2021-07-08','2021-07-19')))
       ),
       aes(factor(date),Weight,color=timeid))+
  geom_boxjitter(aes(color=timeid),alpha=.4,
                 jitter.shape = 16, 
                 jitter.params = list(height = 0, width = 0.04),
                 outlier.color = NA, errorbar.draw = TRUE)+
  facet_grid(nitrogen+appl~var)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))


ggplot(ear%>% 
         filter(var%in%c('Potenzial',"Pionier",'Capone','Patras'),
                # nitrogen==176&appl=='Split',
                # date%in%as.Date(c('2021-06-14','2021-06-21','2021-07-01','2021-07-08','2021-07-19'))
                ),
       aes(factor(date),Weight,color=factor(nitrogen)))+
  geom_boxjitter(alpha=.4,
                 jitter.shape = 16, 
                 jitter.params = list(height = 0, width = 0.04),
                 outlier.color = NA, errorbar.draw = TRUE)+
  facet_grid(appl+timeid~var)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))


# -------------------------------------------------------------------------

sumdf <- ear  %>% 
  group_by(date,var,nitrogen,appl,timeid) %>% 
  summarise(weight=mean(weight,na.rm = T)) %>% 
  mutate(group=interaction(var,appl,timeid,nitrogen))
# write.csv(sumdf,"./data/summarized.csv",row.names = F)

ggplot(sumdf %>%
         filter(var%in%c("Capone",
                         "Patras","Pionier")),
       aes(date,weight,color=var))+
  geom_point(shape=1)+
  geom_line(alpha=.5,aes(group=interaction(var,group)))+
  facet_grid(~timeid)+
  theme_bw()+
  theme(panel.grid.minor.x = element_blank(),
        strip.background = element_blank())


ggplot(sumdf%>% 
         filter(var%in%c('Potenzial',"Pionier")),
       aes(date,weight))+
  geom_point(aes(shape=appl,color=factor(nitrogen)),alpha=.5)+
  geom_line(alpha=.5,aes(color=factor(nitrogen),group=interaction(appl,nitrogen)))+
  facet_grid(timeid~var)+
  scale_x_continuous(breaks=c(61,71,81,87))+
  theme_bw()+
  theme(panel.grid.minor.x = element_blank(),
        strip.background = element_blank())

ggplot(sumdf%>% 
         filter(var%in%c('Potenzial',"Pionier"),
                nitrogen==220&appl=='Split'&timeid=='Early'|
                  nitrogen==176&appl=='Combined'&timeid=='Late'),
       aes(date,weight))+
  geom_point(aes(shape=appl,color=timeid),alpha=.5)+
  geom_line(alpha=.5,aes(color=timeid,group=interaction(var,group)))+
  facet_grid(~var)+
  scale_x_continuous(breaks=c(61,71,81,87))+
  theme_bw()+
  theme(panel.grid.minor.x = element_blank(),
        strip.background = element_blank())

ggplot(ear%>% 
         filter(var%in%c('Potenzial',"Pionier"),
                nitrogen==176&appl=='Combined'),
       aes(factor(date),Weight,color=timeid))+
  geom_boxjitter(aes(color=timeid),alpha=.4,
                 jitter.shape = 16, 
                 jitter.params = list(height = 0, width = 0.04),
                 outlier.color = NA, errorbar.draw = TRUE)+
  facet_grid(~var)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))
ggplot(ear%>% 
         filter(var%in%c('Potenzial',"Pionier"),
                nitrogen==220&appl=='Split'&timeid=='Early'|
                  nitrogen==176&appl=='Combined'&timeid=='Late'),
       aes(factor(date),Weight,color=timeid))+
  geom_boxjitter(aes(color=timeid),alpha=.4,
                 jitter.shape = 16, 
                 jitter.params = list(height = 0, width = 0.04),
                 outlier.color = NA, errorbar.draw = TRUE)+
  facet_grid(~var)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))
