### BLiND VS VIDEO OBSERVATION EXPLORATION ###

rm(list=ls())
getwd()
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")

library(ggplot2)
library(plyr)
library(dplyr)
library(grid)
library(palettetown)

tprovallyrs<-read.csv("cleaned tern provisioning.csv")                          #read in data

tprov<-filter(tprovallyrs, !Year %in% c(2003, 2004, 2010, 2012))                #filter out low n yrs

blindfish<-subset(tprov, Species.lumped=="fish")      #filter out non-fish and video obs

blindfprop<-blindfish %>%                                                       #calculate proportion of each family from total for each year
  dplyr::group_by(Year, Species.2) %>%
  dplyr::summarise(n=n()) %>% 
  dplyr::mutate(prop=n/sum(n))

ggplot()+theme_bw()+
geom_col(data=blindfprop,aes(x=Year, y=prop, fill=Species.2))+
  theme_bw()+ scale_fill_poke(pokemon = 'Charizard', spread = 8)
  

gah<-blindfish %>% 
  dplyr::group_by(Species) %>% 
  dplyr:: summarise(n=n())
