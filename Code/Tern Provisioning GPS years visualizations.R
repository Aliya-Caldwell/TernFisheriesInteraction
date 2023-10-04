
# packages etc
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(grid)
library(palettetown)
library(colorspace)
library(RColorBrewer)
library(pals)
library(wesanderson)

getwd()

setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern Provisioning Data")


df<-read.csv("T-Feeding up to 2022.csv",fileEncoding="UTF-8-BOM")

dfclean<-df %>%
  dplyr::rename(size='PreySizeMM') %>%
  dplyr::rename(prey='PreyFamily') %>%
  dplyr::rename(method='ObservationType') %>%
  dplyr::rename(year='Year') %>%
  dplyr::rename(day='DayOfYear') %>%
  dplyr::rename(week='WeekOfYear') %>%
  dplyr::rename(month='Month') %>%
  dplyr::rename(watchtime='WatchDurationMM') %>%
  dplyr::rename(nestswatched='NumberNestsObserved') %>%
  dplyr::rename(event='Event') %>% 
  filter(method==c("blind","Axis Cam")) %>% 
  filter(year==c("2019","2021","2022"))

#sampling effort

effort<-dfclean %>%
  filter(!IncludeForRegression %in% c("no")) %>% 
  dplyr::mutate(`sampling effort`=(as.numeric(watchtime)*as.numeric(nestswatched))) %>%
  group_by(year) %>%
  dplyr::summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T)) %>% 
  filter(!is.na(year))

#diet data
fish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("unknown","unknown fish","")) %>% 
  group_by(year,prey) %>%
  dplyr::summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T), mediansize=median(as.numeric(size),na.rm=T)) %>%
  dplyr::mutate(prop=n/sum(n))

#combine and generate relative abundance
fisheffort<-merge(effort,fish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

#FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
tprov<-fisheffort %>%
  dplyr::select(year, prey, n, meansize, mediansize, `Effort (nest mins)`, prop, `relative abundance (fish per nest min)`) %>%
  dplyr::rename(proportion=prop) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)


tprov<-tprov %>% 
  mutate(relabundancehr=abundance*60) 


#plot
fishies<-ggplot(tprov)+
  geom_col(aes(x=as.factor(year), y=proportion, fill=prey))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=20),
        legend.text = element_text(size=12),legend.title = element_text(size=20),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  labs(fill="")+
  scale_fill_poke(pokemon="surskit", spread=10)

fishies  

ggplot_build(fishies)$data

#2019 butterfish+invert
0.022727273+0.068181818

#2019 herring+sandlance
0.295454545+0.034090909

#2021
0.165354331+0.291338583

0.102362205+0.125984252

#2022
0.136842105+0.010526316

0.378947368+0.147368421

#herring
herring<-tprov %>% 
  filter(prey=="herring") %>% 
  filter(! year %in% c(2003, 2004, 2010, 2008,2012))
hake<-tprov %>% 
  filter(prey=="hake") %>% 
  filter(! year %in% c(2003, 2004, 2010,2008))

ggplot()+
  geom_point(data=herring, aes(x=year,y=relabundancehr), color="cadetblue",size=3)+
  geom_point(data=hake, aes(x=year,y=relabundancehr), color="indianred",size=3)+
  geom_smooth(method="lm",data=herring, aes(x=year,y=relabundancehr), alpha=0.3,color="cadetblue", fill="cadetblue", size=1.5)+
  geom_smooth(method="lm",data=hake, aes(x=year,y=relabundancehr), alpha=0.5,color="indianred", fill="indianred", size=1.5)+
  theme_classic()+
  xlab("")+ylab("relative abundance (fish/nest hr)")

