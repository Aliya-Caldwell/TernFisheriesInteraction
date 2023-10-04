

## FISH SIZE vs ABUNDANCE in TERN DIET ##

rm(list=ls())
getwd()
setwd("C:/Users/aec1075/Box/SMLTernsProgram/Raw data/Feeding data")

library(ggplot2)
library(plyr)
library(dplyr)
library(grid)
library(palettetown)

# organize feeding data
tprovallyrs<-read.csv("T-feeding up to 2020.csv")                               #read in data
tprovallyrsallsp<-filter(tprovallyrs, !Year %in% c(2003, 2004, 2010, 2012),
                         !Prey.Family %in% c("invertebrate", "unknown"))        #filter out low n yrs and non-fish
tprov<-subset(tprovallyrsallsp, Prey.Family=="herring"|Prey.Family=="hake"|     #
                Prey.Family=="butterfish"|Prey.Family=="sandlance"|
                Prey.Family=="mackerel")
tprov1<-tprov %>%                                                               #calculate the mean size for each family for each year
  group_by(Year,Prey.Family) %>%                                                #also calculated proportion of each family here for whatever reason
  summarise(n=n(), meansize=mean(as.numeric(Prey.size..mm..based.on.36.5.mm.average.culmen.length.),na.rm=T)) %>% 
  mutate(prop=n/sum(n))

# combine feeding data with sampling effort
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
seffort<-read.csv("Tern provisioning sampling effort.csv")                      #read in sampling effort data
effort<-seffort %>%                                                             #create table with just nest hrs                            
  dplyr::select(Year,Tot.nest.hrs)

tprov2<-merge(tprov1,effort) %>%                                                #merge effort and tern feeding datasets
  dplyr::mutate(relabundance=(n/Tot.nest.hrs)) %>%                              #calcuate 
  dplyr::rename(effort=Tot.nest.hrs) %>% 
  dplyr::rename(Family=Prey.Family) %>% 
  dplyr::rename("relative abundance"=relabundance) %>% 
  dplyr::rename("mean size" = meansize)

ggplot(tprov2)+theme_bw()+
  geom_point(aes(x=`relative abundance`, y=`mean size`, color=Family),pch=19,size=4,alpha=.8)+
  geom_point(aes(x=`relative abundance`, y=`mean size`, color=Family),shape = 21,size = 4,colour = "black")+
  scale_color_poke(pokemon = 'magneton', spread = 8)

#look at each individual species for their distribution

tprovherr<-subset(tprov2, Family=="herring")

ggplot(tprovherr)+theme_bw()+
  geom_point(aes(x=`relative abundance`, y=`mean size`, color=Family),pch=19,size=4,alpha=.8)+
  geom_point(aes(x=`relative abundance`, y=`mean size`, color=Family),shape = 21,size = 4,colour = "black")+
  scale_color_poke(pokemon = 'magneton', spread = 8)

tprovhake<-subset(tprov2, Family=="hake")

ggplot(tprovhake)+theme_bw()+
  geom_point(aes(x=`relative abundance`, y=`mean size`, color=Family),pch=19,size=4,alpha=.8)+
  geom_point(aes(x=`relative abundance`, y=`mean size`, color=Family),shape = 21,size = 4,colour = "black")+
  scale_color_poke(pokemon = 'magneton', spread = 8)

#MIGHT NEED TO LOOK AT A DIFFERENT TEMPORAL SCALE TO UP THE SAMPLE SIZE, MAYBE BY WEEK

