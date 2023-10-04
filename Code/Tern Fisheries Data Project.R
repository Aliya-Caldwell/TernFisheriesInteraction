
#________________________#
# TERN FISHERIES PROJECT #
#________________________#

rm(list=ls())

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
library(stats)
library(tseries)
library(astsa)
library(forecast)
library(viridis)



# DIET DATA DF annual PROP and ABUND of each species --------------------------------------------------------

#-- this df will have a row for each family/year combinations
#-- families will include all fish species along with invertebrate, unknown, and unknown fish categories
#-- columns generated will include an intermediate sampling effort column and final relative abundance (in fish per nest minute)
#   and proportion (proportion of family from total) columns
#-- uses dfclean from above

getwd()
setwd("/Users/aliyae/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Official Most Recent T-Feeding Document")


df<-read.csv("T-Feeding Update 2023.csv",fileEncoding="UTF-8-BOM")

dfclean<-df %>%
  dplyr::rename(size='PreySizeCM') %>%
  dplyr::rename(prey='PreyFamily') %>%
  dplyr::rename(method='ObservationType') %>%
  dplyr::rename(year='Year') %>%
  dplyr::rename(day='DayOfYear') %>%
  dplyr::rename(week='WeekOfYear') %>%
  dplyr::rename(month='Month') %>%
  dplyr::rename(watchtime='WatchDurationMM') %>%
  dplyr::rename(nestswatched='NumberNestsObserved') %>%
  dplyr::rename(event='Event') %>% 
  dplyr::filter(!prey %in% c("unknown","unknown fish")) #remove unidentified items

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
  filter(! prey %in% c("")) %>% 
  group_by(year,prey) %>%
  dplyr::summarise(n=n(), minsize=min(as.numeric(size),na.rm=T), maxsize=max(as.numeric(size),na.rm=T), meansize=mean(as.numeric(size),na.rm=T), mediansize=median(as.numeric(size),na.rm=T)) %>%
  dplyr::mutate(prop=n/sum(n))

  #combine and generate relative abundance
fisheffort<-merge(effort,fish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

  #FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
tprov<-fisheffort %>%
  dplyr::select(year, prey, n, minsize, meansize, maxsize, `Effort (nest mins)`, prop, `relative abundance (fish per nest min)`) %>%
  dplyr::rename(proportion=prop) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)


    #abundance
ggplot(tprov)+
  geom_col(aes(x=year, y=abundance, fill=prey))+
  theme_classic()+
  ylab("Abundance")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="")

    #proportion
ggplot(tprov)+
  geom_col(aes(x=year, y=proportion, fill=prey))+
  theme_classic()+
  ylab("Proportion of Total")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="")#+scale_fill_viridis(discrete=TRUE)


  #write data to csv 
library(readr)
      #all species
write_csv(tprov, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Tern Provisioning Data/annual_tern_diet_summary.csv")
      #just herring
tprov_herr<-subset(tprov, prey == "herring")
write_csv(tprov_herr, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Tern Provisioning Data/annual_tern_herring_summary.csv")


# DIET DATA DF annual PROP and ABUND of top n prey items --------------------

getwd()
setwd("/Users/aliyae/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Official Most Recent T-Feeding Document")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Tern Provisioning Data")


df<-read.csv("T-Feeding Update 2023.csv",fileEncoding="UTF-8-BOM")

dfclean<-df %>%
  dplyr::rename(size='PreySizeCM') %>%
  dplyr::rename(prey='PreyFamily') %>%
  dplyr::rename(method='ObservationType') %>%
  dplyr::rename(year='Year') %>%
  dplyr::rename(day='DayOfYear') %>%
  dplyr::rename(week='WeekOfYear') %>%
  dplyr::rename(month='Month') %>%
  dplyr::rename(watchtime='WatchDurationMM') %>%
  dplyr::rename(nestswatched='NumberNestsObserved') %>%
  dplyr::rename(event='Event') 

dfcleansub<-dfclean %>%
  dplyr::mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","hake","butterfish","sandlance","mackerel","pollock"),
                                             collapse="|"), prey, ignore.case=T), prey, "other")) %>% 
  dplyr::filter(!preysub %in% c("other")) #remove all other


#sampling effort

effort<-dfclean %>%
  filter(!IncludeForRegression %in% c("no")) %>% 
  dplyr::mutate(`sampling effort`=(as.numeric(watchtime)*as.numeric(nestswatched))) %>%
  group_by(year) %>%
  dplyr::summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T)) %>% 
  filter(!is.na(year))

#diet data
fishsub<-dfcleansub %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  group_by(year,prey) %>%
  dplyr::summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T), mediansize=median(as.numeric(size),na.rm=T)) %>%
  dplyr::mutate(prop=n/sum(n))

#combine and generate relative abundance
fishsubeffort<-merge(effort,fishsub) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

#FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
tprovsub<-fishsubeffort %>%
  dplyr::select(year, prey, n, meansize, mediansize, `Effort (nest mins)`, prop, `relative abundance (fish per nest min)`) %>%
  dplyr::rename(proportion=prop) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)


#abundance
ggplot(tprovsub)+
  geom_col(aes(x=year, y=abundance, fill=prey))+
  theme_classic()+
  ylab("Abundance")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="")

#proportion
ggplot(tprovsub)+
  geom_col(aes(x=year, y=proportion, fill=prey))+
  theme_classic()+
  ylab("Proportion of Total")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="")#+scale_fill_viridis(discrete=TRUE)


# DIET DATA DF w PROP and ABUND for each DAY/WEEK/YEAR --------

#-- this df will have a row for each family/year combinations
#-- families will include all fish species along invertebrate, unknown, and unknown fish categories
#-- columns generated will include an intermediate sampling effort column and final relative abundance (in fish per nest minute)
#   and proportion (proportion of family from total) columns
#-- uses dfclean from above

  #sampling effort
dailyweeklyeffort<-dfclean %>%
  dplyr::mutate(`sampling effort`=(as.numeric(watchtime)*as.numeric(nestswatched)))%>%
  group_by(year, week, day) %>%
  dplyr::summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T))

  #diet data
dailyweeklyfish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  group_by(year, week, day, prey) %>%
  dplyr:: summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T)) %>%
  dplyr:: mutate(prop=n/sum(n))

  #combine and generate relative abundance
dailyweeklyfisheffort<-merge(dailyweeklyeffort,dailyweeklyfish) %>%
  dplyr::mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

  #FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
dailyweeklytprov<-dailyweeklyfisheffort %>%
  dplyr::select(year, week, day, prey, n, meansize, `Effort (nest mins)`,prop, `relative abundance (fish per nest min)`) %>%
  dplyr::rename(proportion=prop) %>%
  dplyr::rename(size=meansize) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)






# DIET DATA DF w PROP and ABUND for each WEEK/YEAR -- THIS IS FUCKED UP BTW -----------------

#-- this df will have a row for each family/year combinations
#-- families will include all fish species along invertebrate, unknown, and unknown fish categories
#-- columns generated will include an intermediate sampling effort column and final relative abundance (in fish per nest minute)
#   and proportion (proportion of family from total) columns
#-- uses dfclean from above

  #sampling effort
weeklyeffort<-dfclean %>%
  mutate(`sampling effort`=(as.numeric(watchtime)*as.numeric(nestswatched)))%>%
  group_by(year, week) %>%
  summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T))

  #diet data
weeklyfish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  group_by(year, week, PreyItem) %>%
  dplyr:: summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T)) %>%
  dplyr::mutate(prop=n/sum(n))

  #combine and generate relative abundance
weeklyfisheffort<-merge(weeklyeffort,weeklyfish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

  #FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
weeklytprov<-weeklyfisheffort %>%
  select(year, week, PreyItem, n, meansize, prop, `relative abundance (fish per nest min)`) %>%
  dplyr::rename(proportion=prop) %>%
  dplyr::rename(size=meansize) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)

#proportion
ggplot(weeklytprov)+
  geom_col(aes(x=week, y=proportion, fill=PreyItem))+
  theme_classic()+
  ylab("Proportion of Total")+xlab("Week")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="")+facet_wrap(vars(year))

#proportion just for one year
ggplot(subset(weeklytprov,year=="2023"))+
  geom_col(aes(x=week, y=proportion, fill=PreyItem))+
  theme_classic()+
  ylab("Proportion of Total")+xlab("Week")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="")


# DIET DATA DF w PROP and ABUND for each WEEK/MONTH/YEAR--------


  #sampling effort
weeklymonthlyeffort<-dfclean %>%
  mutate(`sampling effort`=(watchtime*as.numeric(nestswatched)))%>%
  group_by(year, week, month) %>%
  summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T))

  #diet data
weeklymonthlyfish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  dplyr:: group_by(year, week, month, prey) %>%
  dplyr:: summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T)) %>%
  dplyr::mutate(prop=n/sum(n))

  #combine and generate relative abundance
weeklymonthlyfisheffort<-merge(weeklymonthlyeffort,weeklymonthlyfish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

  #FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
weeklymonthlytprov<-weeklymonthlyfisheffort %>%
  select(year, week, month, prey, n, meansize, `Effort (nest mins)`,prop, `relative abundance (fish per nest min)`) %>%
  dplyr::rename(proportion=prop) %>%
  dplyr::rename(size=meansize) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)

#proportion
ggplot(weeklymonthlytprov)+
  geom_col(aes(x=week, y=proportion, fill=prey))+
  theme_classic()+
  ylab("Proportion of Total")+xlab("Week")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="")



# DIET DATA DF with PROP and ABUND for each DAY/WEEK/MONTH/YEAR --------

#-- uses dfclean from above

  #sampling effort
alltimeeffort<-dfclean %>%
  mutate(`sampling effort`=(watchtime*as.numeric(nestswatched)))%>%
  group_by(year, week, month, day) %>%
  summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T))

  #diet data
alltimefish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  group_by(year, week, month, day, prey) %>%
  dplyr::summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T)) %>%
  dplyr::mutate(prop=n/sum(n))

  #combine and generate relative abundance
alltimeeffort<-merge(alltimeeffort,alltimefish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

  #FINAL DATAFRAME (remove intermediate effort column, rename columns for easy use)
alltimetprov<-alltimeeffort %>%
  select(year, week, month, day, prey, n, meansize, `Effort (nest mins)`,prop, `relative abundance (fish per nest min)`) %>%
  dplyr:: rename(proportion=prop) %>%
  dplyr::rename(size=meansize) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)








# MEDMR DF annual PROP and ABUND of each species (tern catchable) ---------------------------------------------
    #see updated version of this in "Tern Catchable Fisheries CPUE Calculations" R script

library(dplyr)
library(tidyr)
library(ggplot2)

getwd()
setwd("/Users/aliyacaldwell/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl")

DMR_raw<-read.csv("InshoreTrawlCatchData.csv")
DMR_raw$Date <- as.POSIXct(DMR_raw$Date)

#how many tows per season per year
DMR_tows<-DMR_raw %>% 
  group_by(Season, Year) %>% 
  summarise(nTows=n_distinct(DMR_raw$Tow_Number)) #--> ok so they do 123 tows per survey

#remove any non-fish species
DMR_raw <- DMR_raw %>% 
  filter(!grepl("Astarte|Shrimp|Dollar|Star|Scallop|Crab|Krill|Jelly|Mussel|
                Snail|Clam|Lobster|Octopus|Pandalus|Cucumber|Urchin|Axius|
                Mysidacea|Cyclocardia|Lebbeid|Porbeagle|Anemone|Sponges|Skate", 
                Common_Name))

# RELATIVE ABUNDANCE and PROPORTION of each species across in local region (region 1)
DMR_catch_all<-DMR_raw %>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::group_by(Season, Year, Common_Name) %>% 
  dplyr::summarise(TotCatch=sum(Number_Caught),n=n()) %>% 
  na.omit() %>% 
  dplyr::mutate(Prop=n/sum(n))

  #to visualize, filter out only observations >50
      #abundance
ggplot(subset(DMR_catch_all,TotCatch > 50))+
  geom_col(aes(x=Year, y=TotCatch, fill=Common_Name))+
  theme_classic()+
  ylab("Abundance")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")
    
      #proportion
ggplot(DMR_catch_all)+
  geom_col(aes(x=Year, y=Prop, fill=Common_Name))+
  theme_classic()+
  ylab("Proportion of Total")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")

  #to visualize better, lets only look at species terns eat
DMR_terns <- DMR_raw%>% 
  filter(grepl("Alewife|Butterfish|Cunner|Herring|Hake|Mackerel|Monkfish|Moonfish|Pollock|Shad|Silverside|Smelt|Sand", 
                Common_Name)) %>% 
  filter(!grepl("Flounder|Scad|Spotted", 
               Common_Name))

DMR_catch_all_terns<-DMR_terns %>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::group_by(Season, Year, Common_Name) %>% 
  dplyr::summarise(TotCatch=sum(Number_Caught),n=n()) %>% 
  na.omit() %>% 
  dplyr::mutate(Prop=n/sum(n))

      #abundance
ggplot(DMR_catch_all_terns)+
  geom_col(aes(x=Year, y=TotCatch, fill=Common_Name))+
  theme_classic()+
  ylab("Abundance")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")

      #proportion
ggplot(DMR_catch_all_terns)+
  geom_col(aes(x=Year, y=Prop, fill=Common_Name))+
  theme_classic()+
  ylab("Proportion")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+facet_grid(vars(Season))


  #write data to csv 
library(readr)
      #all species that terns eat
write_csv(DMR_catch_all_terns, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Fisheries Data/MEDMR Inshore Trawl/annual_DMR_summary.csv")
      #just herring in the spring
DMR_catch_herr<-subset(DMR_catch_all_terns, Common_Name == "Herring Atlantic")
DMR_catch_herr<-subset(DMR_catch_herr, Season == "Spring")
write_csv(DMR_catch_herr, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Fisheries Data/MEDMR Inshore Trawl/annual_DMR_herring_summary.csv")

# ABUNDANCE CORRECTED BY LENGHTS
setwd("/Users/aliyacaldwell/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl")

          #summarise lenghts data#
DMR_met<-read.csv("InshoreTrawlLengthData.csv")
DMR_met_species<-as.data.frame(unique(DMR_met$Common_Name))
DMR_met <- DMR_met%>% 
  filter(!grepl("Astarte|Shrimp|Dollar|Star|Scallop|Crab|Krill|Jelly|Mussel|
                Snail|Clam|Lobster|Octopus|Pandalus|Cucumber|Urchin|Axius|
                Mysidacea|Cyclocardia|Lebbeid|Porbeagle|Anemone|Sponges|Skate", 
                Common_Name))
  #year 0 proportion and abundance all species
DMR_met_all<-DMR_met %>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::group_by(Season, Year, Common_Name) %>% 
  dplyr::summarise(TotCatch=n()) %>% 
  dplyr::mutate(Prop=TotCatch/sum(TotCatch))


  #THIS IS JUST FOR HERRING NOW
      #this work flow corrects the number of herring caught based on size ranges catchable by terns as determined by von bert curves
      #end goal is to make DF of all HERRING prop and abund for years 0-2 size corrected
      #the proportion of other species caught is NOT size corrected. so we have a df with the proportion of catchable herring in the total catch (of all sizes)
      #this workflow calculates one catchable proportion for each year and then applies that to every tow; alternatively, we could calculate a catch proportion for each individual tow and then apply those......... but this might not work well across other datasets?

    #for year 0 proportion and abundance#
DMR_herr_prop_catchable<-DMR_met%>% #determin proportion of herring that are catchable by terns
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name %in% c("Herring Atlantic","Herring Blueback","Alewife")) %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchableYr0=(sum(Length < 16)/n())) 
          #using fall data here, but should I use spring? or be correcting sizes at a finer scale (monthly)?
          #this is the proportion of catchable herring out of all fish caught (not proportion of catchable herring out of CATCHABLE other species)

DMR_raw<-read.csv("InshoreTrawlCatchData.csv") #bring in catch data
DMR_raw$Date <- as.POSIXct(DMR_raw$Date)
DMR_raw <- DMR_raw %>% 
  filter(!grepl("Astarte|Shrimp|Dollar|Star|Scallop|Crab|Krill|Jelly|Mussel|
                Snail|Clam|Lobster|Octopus|Pandalus|Cucumber|Urchin|Axius|
                Mysidacea|Cyclocardia|Lebbeid|Porbeagle|Anemone|Sponges|Skate", 
                Common_Name))
DMR_catch_all<-DMR_raw %>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Fall")

DMR_catch_all$Common_Name[DMR_catch_all$Common_Name == "Herring Atlantic"] <- 'Herring'
DMR_catch_all$Common_Name[DMR_catch_all$Common_Name == "Herring Blueback"] <- 'Herring'
DMR_catch_all$Common_Name[DMR_catch_all$Common_Name == "Alewife"] <- 'Herring'
          #combined all herring species; should I just do atlantic herring? 

DMR_catch_herr<-DMR_catch_all %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

DMR_catch_herr_prop<-full_join(DMR_catch_herr,DMR_herr_prop_catchable) #join catch data with calculation of catchable proportion

DMR_catch_herr_prop<-DMR_catch_herr_prop %>% #determine number that were catchable vs non-catchable from total catch
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchableYr0) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchableYr0)))

DMR_catchable_herr<-DMR_catch_herr_prop %>% #renamed number catchable to number caught
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)

DMR_non_catchable_herr<-DMR_catch_herr_prop %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
DMR_non_catchable_herr$Common_Name<-"Non-Catchable Herring"

DMR_catch_no_herr<-DMR_catch_all %>% 
  filter(Common_Name != "Herring")

DMR_catch_all<-rbind(DMR_catchable_herr,DMR_non_catchable_herr)
DMR_catch_all<-rbind(DMR_catch_all,DMR_catch_no_herr)

DMR_catch_all<-DMR_catch_all%>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(TotCatch=sum(Number_Caught),n=n()) %>% 
  na.omit() %>% 
  dplyr::mutate(Prop=n/sum(n))

DMR_herr_0<-DMR_catch_all %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  #dplyr::filter(Region == "1") %>% 
  #dplyr::filter(Season == "Fall") %>% 
  #::group_by(Year) %>% 
  #dplyr::summarise(TotCatchYr0=n()) %>% 
  #dplyr::mutate(PropYr0=TotCatchYr0/sum(TotCatchYr0)) %>% 
  dplyr::rename(PropYr0=Prop,TotCatchYr0=TotCatch) %>% 
  dplyr::select(Year,PropYr0,TotCatchYr0)



      #for year 1 proportion and abundance
DMR_herr_prop_catchable_1<-DMR_met%>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name %in% c("Herring Atlantic","Herring Blueback","Alewife")) %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchableYr1=(sum(Length %in% (16:20))/n()))

DMR_catch_all_1<-DMR_raw %>% #bring in catch data
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Fall")

DMR_catch_all_1$Common_Name[DMR_catch_all_1$Common_Name == "Herring Atlantic"] <- 'Herring'
DMR_catch_all_1$Common_Name[DMR_catch_all_1$Common_Name == "Herring Blueback"] <- 'Herring'
DMR_catch_all_1$Common_Name[DMR_catch_all_1$Common_Name == "Alewife"] <- 'Herring'

DMR_catch_herr_1<-DMR_catch_all_1 %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

DMR_catch_herr_prop_1<-full_join(DMR_catch_herr_1,DMR_herr_prop_catchable_1) 

DMR_catch_herr_prop_1<-DMR_catch_herr_prop_1 %>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchableYr1) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchableYr1)))

DMR_catchable_herr_1<-DMR_catch_herr_prop_1 %>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)

DMR_non_catchable_herr_1<-DMR_catch_herr_prop_1 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
DMR_non_catchable_herr_1$Common_Name<-"Non-Catchable Herring"

DMR_catch_no_herr_1<-DMR_catch_all_1 %>% 
  filter(Common_Name != "Herring")

DMR_catch_all_1<-rbind(DMR_catchable_herr_1,DMR_non_catchable_herr_1)
DMR_catch_all_1<-rbind(DMR_catch_all_1,DMR_catch_no_herr_1)

DMR_catch_all_1<-DMR_catch_all_1%>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(TotCatch=sum(Number_Caught),n=n()) %>% 
  na.omit() %>% 
  dplyr::mutate(Prop=n/sum(n))

DMR_herr_1<-DMR_catch_all_1 %>% 
  dplyr::filter(Common_Name == "Herring") 

DMR_herr_1<-as.data.frame(as.matrix(DMR_herr_1))

DMR_herr_1<-DMR_herr_1%>% 
  dplyr::mutate(PropYr1=lag(Prop,n=1),TotCatchYr1=lag(TotCatch,n=1))%>% 
  dplyr::select(Year,PropYr1,TotCatchYr1)




      #for year 2 proportion and abundance
DMR_herr_prop_catchable_2<-DMR_met%>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name %in% c("Herring Atlantic","Herring Blueback","Alewife")) %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchableYr2=(sum(Length %in% (20:25))/n()))

DMR_catch_all_2<-DMR_raw %>% #bring in catch data
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Fall")

DMR_catch_all_2$Common_Name[DMR_catch_all_2$Common_Name == "Herring Atlantic"] <- 'Herring'
DMR_catch_all_2$Common_Name[DMR_catch_all_2$Common_Name == "Herring Blueback"] <- 'Herring'
DMR_catch_all_2$Common_Name[DMR_catch_all_2$Common_Name == "Alewife"] <- 'Herring'

DMR_catch_herr_2<-DMR_catch_all_2 %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

DMR_catch_herr_prop_2<-full_join(DMR_catch_herr_2,DMR_herr_prop_catchable_2) 

DMR_catch_herr_prop_2<-DMR_catch_herr_prop_2 %>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchableYr2) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchableYr2)))

DMR_catchable_herr_2<-DMR_catch_herr_prop_2 %>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)

DMR_non_catchable_herr_2<-DMR_catch_herr_prop_2 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
DMR_non_catchable_herr_2$Common_Name<-"Non-Catchable Herring"

DMR_catch_no_herr_2<-DMR_catch_all_2 %>% 
  filter(Common_Name != "Herring")

DMR_catch_all_2<-rbind(DMR_catchable_herr_2,DMR_non_catchable_herr_2)
DMR_catch_all_2<-rbind(DMR_catch_all_2,DMR_catch_no_herr_2)

DMR_catch_all_2<-DMR_catch_all_2%>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(TotCatch=sum(Number_Caught),n=n()) %>% 
  na.omit() %>% 
  dplyr::mutate(Prop=n/sum(n))

DMR_herr_2<-DMR_catch_all_2 %>% 
  dplyr::filter(Common_Name == "Herring") 

DMR_herr_2<-as.data.frame(as.matrix(DMR_herr_2))

DMR_herr_2<-DMR_herr_2%>% 
  dplyr::mutate(PropYr2=lag(Prop,n=2),TotCatchYr2=lag(TotCatch,n=2)) %>% 
  dplyr::select(Year,PropYr2,TotCatchYr2)

DMR_herr_1$Year<-as.integer(DMR_herr_1$Year)
DMR_herr_2$Year<-as.integer(DMR_herr_2$Year)

DMR_herr_lags012<-full_join(DMR_herr_0, DMR_herr_1, by='Year')
DMR_herr_lags012<-left_join(DMR_herr_lags012, DMR_herr_2, by='Year')

library(readr)
#all species that terns eat
write_csv(DMR_herr_lags012, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Fisheries Data/MEDMR Inshore Trawl/DMR_herring_size_corrected.csv")
#just herring in the spring


### VISUALIZE MEDMR vs TERNS DATA

        #prey tern data
getwd()
setwd("/Users/aliyacaldwell/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Diet--Fisheries/Data/Tern Provisioning Data")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Tern Provisioning Data")
df<-read.csv("T-Feeding Update 2023.csv",fileEncoding="UTF-8-BOM")
dfclean<-df %>%
  dplyr::rename(size='PreySizeCM') %>%
  dplyr::rename(prey='PreyFamily') %>%
  dplyr::rename(method='ObservationType') %>%
  dplyr::rename(year='Year') %>%
  dplyr::rename(day='DayOfYear') %>%
  dplyr::rename(week='WeekOfYear') %>%
  dplyr::rename(month='Month') %>%
  dplyr::rename(watchtime='WatchDurationMM') %>%
  dplyr::rename(nestswatched='NumberNestsObserved') %>%
  dplyr::rename(event='Event')

effort<-dfclean %>%
  filter(!IncludeForRegression %in% c("no")) %>% 
  dplyr::mutate(`sampling effort`=(as.numeric(watchtime)*as.numeric(nestswatched))) %>%
  group_by(year) %>%
  dplyr::summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T)) %>% 
  filter(!is.na(year))

fish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  dplyr::mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","lumpfish","hake","butterfish","sandlance","mackerel","bluefish","cunner","pollock","mummichog","stickleback"),
                                                    collapse="|"), prey, ignore.case=T), prey, "other/unknown")) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012)) %>% 
  dplyr::filter(!preysub == "other/unknown") %>% 
  group_by(year,preysub) %>%
  dplyr::summarise(n=n(), minsize=min(as.numeric(size),na.rm=T), maxsize=max(as.numeric(size),na.rm=T), meansize=mean(as.numeric(size),na.rm=T), mediansize=median(as.numeric(size),na.rm=T)) %>%
  dplyr::mutate(prop=n/sum(n))

fisheffort<-merge(effort,fish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))
tprov<-fisheffort %>%
  dplyr::select(year, preysub, n, minsize, meansize, maxsize, `Effort (nest mins)`, prop, `relative abundance (fish per nest min)`) %>%
  dplyr::rename(proportion=prop) %>%
  dplyr::rename(abundance=`relative abundance (fish per nest min)`)
tprov[,2]=toupper(tprov[,2]) 
tprov<-tprov %>% 
  dplyr::select(year,preysub,proportion,abundance)

        #prep DMR data (not adjusted for anything in terms of size)
DMRprov <- DMR_raw %>% 
  filter(!grepl("Astarte|Shrimp|Dollar|Star|Scallop|Crab|Krill|Jelly|Mussel|
                Snail|Clam|Lobster|Octopus|Pandalus|Cucumber|Urchin|Axius|
                Mysidacea|Cyclocardia|Lebbeid|Porbeagle|Anemone|Sponges|Skate", 
                Common_Name))

DMRprov$Common_Name[DMRprov$Common_Name == "Herring Atlantic"] <- 'Herring'
DMRprov$Common_Name[DMRprov$Common_Name == "Herring Blueback"] <- 'Herring'
DMRprov$Common_Name[DMRprov$Common_Name == "Alewife"] <- 'Herring'
DMRprov$Common_Name[DMRprov$Common_Name == "Hake Silver (Whiting)"] <- 'Hake'
DMRprov$Common_Name[DMRprov$Common_Name == "Hake Atlantic Red"] <- 'Hake'
DMRprov$Common_Name[DMRprov$Common_Name == "Hake White"] <- 'Hake'
DMRprov$Common_Name[DMRprov$Common_Name == "Hake Spotted"] <- 'Hake'
DMRprov$Common_Name[DMRprov$Common_Name == "Sand Lance American"] <- 'Sandlance'
DMRprov$Common_Name[DMRprov$Common_Name == "Stickleback Fourspine"] <- 'Stickleback'
DMRprov$Common_Name[DMRprov$Common_Name == "Stickleback Threespine"] <- 'Stickleback'
DMRprov$Common_Name[DMRprov$Common_Name == "Mackerel Atlantic"] <- 'Mackerel'

DMRprov<-DMRprov %>% 
  dplyr::mutate(Common_Name=Common_Name<-ifelse(grepl(paste(c("Herring","Lumpfish","Hake","Butterfish","Sandlance","Mackerel","Bluefish","Cunner","Pollock","Mummichog","Stickleback"),
                                                                      collapse="|"), Common_Name, ignore.case=T), Common_Name, "other/unknown")) %>% 
  dplyr::filter(!Common_Name == "other/unknown") %>% 
  dplyr::filter(!Common_Name == "Scad Mackerel") %>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Season, Year, Common_Name) %>% 
  dplyr::summarise(TotCatch=sum(Number_Caught),n=n()) %>% 
  na.omit() %>% 
  dplyr::mutate(Prop=n/sum(n)) %>% 
  dplyr::rename(preysub=Common_Name, proportion=Prop, year=Year)

        #adbundance
ggplot(tprov)+
  geom_col(aes(x=year, y=abundance, fill=preysub))+theme_bw()+
  ylab("Relative Abundance (fish/nest min")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_viridis_d()

ggplot(DMRprov)+
  geom_col(aes(x=year, y=TotCatch, fill=preysub))+theme_bw()+
  ylab("Relative Abundance")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_viridis_d()

      #proportion
ggplot(tprov)+
  geom_col(aes(x=year, y=proportion, fill=preysub))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_viridis_d()

ggplot(DMRprov)+
  geom_col(aes(x=year, y=proportion, fill=preysub))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_viridis_d()
  
        #proportion on same plot

DMRprov_sub<-DMRprov %>% 
  select(year,proportion,preysub)
DMRprov_sub<-DMRprov_sub[,-1]
DMRprov_sub$DataSet<-"DMR"
DMRprov_sub$preysub = toupper(DMRprov_sub$preysub)

tprov_sub<-tprov %>% 
  select(year,proportion,preysub)
tprov_sub$DataSet<-"TERN"

df<-rbind(DMRprov_sub,tprov_sub) %>% 
  filter(! year %in% c("1999","2003","2004","2010","2012","2016"))

ggplot(df,aes(x = DataSet,y = proportion, fill = preysub)) + 
  geom_bar(stat = "identity",position = "stack") +
  facet_grid(~ year)+theme_classic()+
  ylab("Proportion of Total Observations")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  labs(fill="Prey")+
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# ENVIRONMENTAL DATA ------------------------------------------------------

#download SST from whole GOM extent 

library(dplyr) 
library(lubridate) 
library(ggplot2) 
library(tidync)
library(rerddap)
library(doParallel) 


rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

    # can only grab data comfortably in 7 year increments; function to loop through time periods
sst_grab <- function(time_df){
  sst_data <- rerddap::griddap(datasetx = "ncdcOisst21Agg_LonPM180",
                               url = "https://coastwatch.pfeg.noaa.gov/erddap/",
                               time = c(time_df$start, time_df$end),
                               zlev = c(0, 0),
                               latitude = c(41.5, 44),
                               longitude = c(-72, -69.5),
                               fields = "sst")$data %>%
    dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) %>%
    dplyr::rename(t = time, temp = sst, lon = longitude, lat = latitude) %>%
    dplyr::select(lon, lat, t, temp) %>%
    stats::na.omit()
}



    # Date download range by start and end dates per year
grab_years <- data.frame(date_index = 1:4,
                         start = as.Date(c("2000-01-01", "2006-01-01","2013-01-01", "2020-01-01")),
                         end = as.Date(c("2005-12-31", "2012-12-31", "2019-12-31", "2022-12-31")))

    # Download all of the data 
base::system.time(
  sst_data <- grab_years %>%
    dplyr::group_by(date_index) %>%
    dplyr::group_modify(~sst_grab(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::select(lon, lat, t, temp)
)

# write data to CSV
sst_data<-as.data.frame(sst_data)
library(readr)
write_csv(sst_data, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data/sst_erddap_GOM.csv")

#bring data back in
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data")
sst_dat<-read_csv("sst_erddap_GOM.csv")

    #visualize a single day to make sure it makes sense
sst_dat %>%
  dplyr::filter(t == "2019-3-01") %>%
  ggplot2::ggplot(aes(x = lon, y = lat)) +
  ggplot2::geom_tile(aes(fill = temp)) +
  ggplot2::scale_fill_viridis_c() +
  ggplot2::coord_quickmap(expand = F) +
  ggplot2::labs(x = NULL, y = NULL, fill = "SST (?C)") +
  ggplot2::theme(legend.position = "bottom")

  #mean for season for each raster cell to create map of annual mean temps
sst_data_avegrid<-sst_dat %>% 
  dplyr::mutate(year=year(t)) %>% 
  dplyr::group_by(year, lat, lon) %>% 
  dplyr::summarise(meanSST=mean(temp)) 

ggplot(sst_data_avegrid, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = meanSST)) +
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (?C)") +
  theme(legend.position = "bottom")+
  facet_wrap(vars(year))

    #create monthly average, min, and max temps across whole spatial scale
sst_data_summ_fullext<-sst_dat %>% 
  dplyr::group_by(month = lubridate::floor_date(t, "month"),
           year = lubridate::floor_date(t, "year")) %>% 
  dplyr::summarise(maxSST=max(temp), minSST=min(temp), meanSST=mean(temp))

    #create seasonal ave, min, max across whole spatial scale
sst_data_seasonal_fullext <- sst_dat %>% 
  mutate(month=as.character(lubridate::month(t)), Year=lubridate::year(t)) %>% 
  mutate(Season = ifelse(month == "3", "spring",
                         ifelse(month == "4", "spring",
                                ifelse(month == "5", "spring",
                                       ifelse(month == "6", "summer",
                                              ifelse(month == "7", "summer",
                                                     ifelse(month == "8", "summer",
                                                            ifelse(month == "9", "fall",
                                                                   ifelse(month == "10", "fall",
                                                                          ifelse(month == "11", "fall", "winter")))))))))) %>% 
  dplyr::group_by(Season,Year) %>% 
  dplyr::summarise(maxSST=max(temp), minSST=min(temp), meanSST=mean(temp))

    #visualize min,max,mean sst for each season of each year
ggplot(sst_data_seasonal_fullext)+
  theme_classic()+
  geom_point(aes(y=meanSST, x=Year, color=Season))+
  geom_line(aes(y=meanSST, x=Year, color=Season))+
  geom_line(aes(y=minSST, x=Year, color=Season), linetype="dashed", size=.5)+
  geom_line(aes(y=maxSST, x=Year, color=Season), linetype="dashed", size=.5)+
  scale_color_manual(values=c("orange","darkolivegreen3","indianred3","cadetblue4"))+
  theme(legend.position = "none")+
  facet_grid(vars(Season))
        #write fall spring and summer data to csv
sst_data_seasonal_fullext_sub<-subset(sst_data_seasonal_fullext, Season != "winter")
write_csv(sst_data_seasonal_fullext_sub, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data/sst_erddap_seasonal_summary.csv")


    #create monthly average, min, and max temps across small spatial scale
sst_data_summ_zoomed<-sst_dat %>% 
  dplyr::filter( lat>42.75 & lat<43.13) %>% 
  dplyr::filter(lon>-71.02 & lon< -70.3) %>% 
  dplyr::group_by(month = lubridate::floor_date(t, "month"),
           year = lubridate::floor_date(t, "year")) %>% 
  dplyr::summarise(maxSST=max(temp), minSST=min(temp), meanSST=mean(temp))

    #create seasonal ave, min, max across small spatial scale
sst_data_seasonal_zoomed <- sst_dat %>% 
  dplyr::filter( lat>42.75 & lat<43.13) %>% 
  dplyr::filter(lon>-71.02 & lon< -70.3) %>% 
  dplyr::mutate(month=as.character(lubridate::month(t)), Year=lubridate::year(t)) %>% 
  dplyr::mutate(Season = ifelse(month == "3", "spring",
                         ifelse(month == "4", "spring",
                                ifelse(month == "5", "spring",
                                       ifelse(month == "6", "summer",
                                              ifelse(month == "7", "summer",
                                                     ifelse(month == "8", "summer",
                                                            ifelse(month == "9", "fall",
                                                                   ifelse(month == "10", "fall",
                                                                          ifelse(month == "11", "fall", "winter")))))))))) %>% 
  dplyr::group_by(Season,Year) %>% 
  dplyr::summarise(maxSST=max(temp), minSST=min(temp), meanSST=mean(temp))

ggplot(sst_data_seasonal_zoomed)+
  theme_classic()+
  geom_point(aes(y=meanSST, x=Year, color=Season))+
  geom_line(aes(y=meanSST, x=Year, color=Season))+
  geom_line(aes(y=minSST, x=Year, color=Season), linetype="dashed", size=.5)+
  geom_line(aes(y=maxSST, x=Year, color=Season), linetype="dashed", size=.5)+
  scale_color_manual(values=c("orange","darkolivegreen3","indianred3","cadetblue4"))+
  theme(legend.position = "none")+
  facet_grid(vars(Season))

#download CHL A DATA from whole GOM extent 1997 to present

library(dplyr) 
library(lubridate) 
library(ggplot2) 
library(tidync)
library(rerddap)
library(doParallel) 


rerddap::info(datasetid = "esa-cci-chla-8d-v6-0", url = "https://oceanwatch.pifsc.noaa.gov/erddap/") #monthly

  #download data all in one big chunk
chla_data_1997_2022 <- rerddap::griddap(datasetx = "esa-cci-chla-8d-v6-0",
                                        url = "https://oceanwatch.pifsc.noaa.gov/erddap/",
                                        time = c("1997-09-04","2022-06-26"),
                                        latitude = c(41.5, 44),
                                        longitude = c(288.5, 292),
                                        fields = "chlor_a" )$data %>%
  dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) %>%
  dplyr::rename(t = time, chla = chlor_a, lon = longitude, lat = latitude) %>%
  dplyr::select(lon, lat, t, chla) %>%
  stats::na.omit()


  # write data to CSV
chla_data_1997_2022<-as.data.frame(as.matrix(chla_data_1997_2022)) #lol
library(readr)
write_csv(chla_data_1997_2022, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data/chla_erddap_GOM.csv")

  #bring data back in
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data")
chla_dat<-read_csv("chla_erddap_GOM.csv")
  
  #visualize a single day to make sure it makes sense
chla_dat %>%
  dplyr::filter(t == "2019-05-17") %>%
  ggplot2::ggplot(aes(x = lon, y = lat)) +
  ggplot2::geom_tile(aes(fill = chla)) +
  ggplot2::scale_fill_viridis_c() +
  ggplot2::coord_quickmap(expand = F) +
  ggplot2::labs(x = NULL, y = NULL, fill = "chla") +
  ggplot2::theme(legend.position = "bottom")

  #mean for season for each raster cell to create map of annual mean temps
chla_data_avegrid<-chla_dat %>% 
  dplyr::mutate(year=year(t)) %>% 
  dplyr::group_by(year, lat, lon) %>% 
  dplyr::summarise(meanchla=mean(chla)) 

ggplot(chla_data_avegrid, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = meanchla)) +
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "chla") +
  theme(legend.position = "bottom")+
  facet_wrap(vars(year))

  #create monthly average, min, and max temps across whole spatial scale
chla_data_summ_fullext<-chla_dat %>% 
  dplyr::group_by(month = lubridate::floor_date(t, "month"),
                  year = lubridate::floor_date(t, "year")) %>% 
  dplyr::summarise(maxchla=max(chla), minchla=min(chla), meanchla=mean(chla))

  #create seasonal ave, min, max across whole spatial scale
chla_data_seasonal_fullext <- chla_dat %>% 
  mutate(month=as.character(lubridate::month(t)), Year=lubridate::year(t)) %>% 
  mutate(Season = ifelse(month == "3", "spring",
                         ifelse(month == "4", "spring",
                                ifelse(month == "5", "spring",
                                       ifelse(month == "6", "summer",
                                              ifelse(month == "7", "summer",
                                                     ifelse(month == "8", "summer",
                                                            ifelse(month == "9", "fall",
                                                                   ifelse(month == "10", "fall",
                                                                          ifelse(month == "11", "fall", "winter")))))))))) %>% 
  dplyr::group_by(Season,Year) %>% 
  dplyr::summarise(maxchla=max(chla), minchla=min(chla), meanchla=mean(chla))
        #write out spring and summer data
library(readr)
chla_data_seasonal_fullext_sub<-subset(chla_data_seasonal_fullext, Season != "fall")
chla_data_seasonal_fullext_sub<-subset(chla_data_seasonal_fullext_sub, Season != "winter")
write_csv(chla_data_seasonal_fullext_sub, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data/chla_erddap_seasonal_summary_GOM.csv")


  #visualize min,max,mean chla for each season of each year
ggplot(chla_data_seasonal_fullext)+
  theme_classic()+
  geom_point(aes(y=meanchla, x=Year, color=Season))+
  geom_line(aes(y=meanchla, x=Year, color=Season))+
  geom_line(aes(y=minchla, x=Year, color=Season), linetype="dashed", size=.5)+
  geom_line(aes(y=maxchla, x=Year, color=Season), linetype="dashed", size=.5)+
  scale_color_manual(values=c("orange","darkolivegreen3","indianred3","cadetblue4"))+
  theme(legend.position = "none")+
  facet_grid(vars(Season))

  #create monthly average, min, and max temps across small spatial scale
chla_data_summ_zoomed<-chla_dat %>% 
  dplyr::filter( lat>42.75 & lat<43.13) %>% 
  dplyr::filter(lon>-71.02 & lon< -70.3) %>% 
  dplyr::group_by(month = lubridate::floor_date(t, "month"),
                  year = lubridate::floor_date(t, "year")) %>% 
  dplyr::summarise(maxchla=max(chla), minchla=min(chla), meanchla=mean(chla))

#   #create seasonal ave, min, max across small spatial scale
# chla_data_seasonal_zoomed <- chla_dat %>% 
#   dplyr::filter( lat>42.75 & lat<43.13) %>% 
#   dplyr::filter(lon>-71.02 & lon< -70.3) %>% 
#   dplyr::mutate(month=as.character(lubridate::month(t)), Year=lubridate::year(t)) %>% 
#   dplyr::mutate(Season = ifelse(month == "3", "spring",
#                                 ifelse(month == "4", "spring",
#                                        ifelse(month == "5", "spring",
#                                               ifelse(month == "6", "summer",
#                                                      ifelse(month == "7", "summer",
#                                                             ifelse(month == "8", "summer",
#                                                                    ifelse(month == "9", "fall",
#                                                                           ifelse(month == "10", "fall",
#                                                                                  ifelse(month == "11", "fall", "winter")))))))))) %>% 
#   dplyr::group_by(Season,Year) %>% 
#   dplyr::summarise(maxchla=max(chla), minchla=min(chla), meanchla=mean(chla))
#seems like there arent enough data within the small scale

# ggplot(chla_data_seasonal_zoomed)+
#   theme_classic()+
#   geom_point(aes(y=meanchla, x=Year, color=Season))+
#   geom_line(aes(y=meanchla, x=Year, color=Season))+
#   geom_line(aes(y=minchla, x=Year, color=Season), linetype="dashed", size=.5)+
#   geom_line(aes(y=maxchla, x=Year, color=Season), linetype="dashed", size=.5)+
#   scale_color_manual(values=c("orange","darkolivegreen3","indianred3","cadetblue4"))+
#   theme(legend.position = "none")+
#   facet_grid(vars(Season))



# FIRST WORKFLOW attempt: herring in TERN vs DMR -------------------------------

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
library(stats)
library(tseries)
library(astsa)
library(forecast)


#PREP DFs w/ ABUNDANCE and PROP
#______________________________

tern_her<-tprov %>% 
  filter(prey == "herring") %>% 
  rename(TERNproportion=proportion,TERNabundance=abundance) %>% 
  select(year, TERNproportion, TERNabundance)
  

        #df with all three herring species added together (abundance = number per trawl survey) and just fall
dmr_her<-DMR_catch_all %>% 
  filter(grepl("Alewife|Herring", 
               Common_Name)) %>% 
  select(Season,Year,Common_Name,n,Prop) %>% 
  group_by(Season,Year) %>% 
  summarise(n=sum(n),Prop=sum(Prop)) %>% 
  rename(DMRproportion=Prop,DMRabundance=n,year=Year) %>% 
  filter(Season == "Fall") %>% 
  ungroup() %>% 
  select(year, DMRproportion, DMRabundance)


ccf_her<-full_join(tern_her, dmr_her)
  

#IDENTIFY SIGNIFICANT TIME LAGS: ABUNDANCE
#_________________________________________

    #visualize both time series raw
ggplot(ccf_her)+theme_bw()+
  geom_point(aes(x=year, y=TERNabundance*3660), 
             color="cadetblue")+
  geom_line(aes(x=year, y=TERNabundance*3660),
            color="cadetblue")+
  scale_y_continuous("Relative abundance (indivs/survey)",
                     sec.axis = sec_axis( trans=~.*3600, name="Relative abundance (indivs/60hrs)"))+
  geom_point(aes(x=year, y=DMRabundance),
             color="indianred")+
  geom_line(aes(x=year, y=DMRabundance),
            color="indianred")+
  xlab("")

    #need to deal with NAs because they are not accepted in lagged tests )obviously)
        #could interpolate linearly, but there is SO much variability that it seems dumb to do that
        #instead, I am going to replace them with the average value across all the other years. not ideal, but fine for now.
ccf_her$TERNabundance[is.na(ccf_her$TERNabundance)]=mean(ccf_her$TERNabundance,na.rm=TRUE)
ccf_her$DMRabundance[is.na(ccf_her$DMRabundance)]=mean(ccf_her$DMRabundance,na.rm=TRUE)
ccf_her$TERNproportion[is.na(ccf_her$TERNproportion)]=mean(ccf_her$TERNproportion,na.rm=TRUE)
ccf_her$DMRproportion[is.na(ccf_her$DMRproportion)]=mean(ccf_her$DMRproportion,na.rm=TRUE)


    #test for stationarity
        #- Dickey-Fuller test: stationary when p<0.05
adf.test(ccf_her$TERNabundance) # not stationary
adf.test(ccf_her$DMRabundance) # not stationary

    #test how many differences is necessary to reach stationarity 
ndiffs(ccf_her$TERNabundance, test="adf") # 0
ndiffs(ccf_her$DMRabundance, test="adf") # 1


    #try to force stationarity by differencing 
ccf_her_TERN_1diff<-diff(ccf_her$TERNabundance, differences=1)
ccf_her_DMR_1diff<-diff(ccf_her$DMRabundance, differences=1)
adf.test(ccf_her_TERN_1diff) # not stationary but almost
adf.test(ccf_her_DMR_1diff) # not stationary

ccf_her_TERN_2diff<-diff(ccf_her$TERNabundance, differences=2)
ccf_her_DMR_2diff<-diff(ccf_her$DMRabundance, differences=2)
adf.test(ccf_her_TERN_2diff) # fine
adf.test(ccf_her_DMR_2diff) # fine

    #test autocorrelation on differenced data
acf(ccf_her_TERN_2diff) # this doesnt look great
acf(ccf_her_DMR_2diff) # this looks slightly better


#     #ccf on differenced data
# ccf_res_2diff<-ccf(ccf_her_TERN_2diff, ccf_her_DMR_2diff, 10, pl = TRUE)
# ccf_res_2diff
# lag2.plot(ccf_her_TERN_2diff, ccf_her_DMR_2diff, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this
# lag2.plot(ccf_her_DMR_2diff, ccf_her_TERN_2diff, 10) #switching the X and Y appears to show positive lags within the context of the previous which makes sense 

    #ccf on non-differenced data
ccf_res<-ccf(ccf_her$TERNabundance, ccf_her$DMRabundance, 10, pl = TRUE)
ccf_res
lag2.plot(ccf_her$TERNabundance, ccf_her$DMRabundance, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this
lag2.plot(ccf_her$DMRabundance, ccf_her$TERNabundance, 10) #switching the X and Y appears to show positive lags within the context of the previous which makes sense 


#IDENTIFY SIGNIFICANT TIME LAGS: PROPORTION
#_____________________________________________

    #visualize both time series raw
ggplot(ccf_her)+theme_bw()+
  geom_point(aes(x=year, y=TERNproportion), 
             color="cadetblue")+
  geom_line(aes(x=year, y=TERNproportion),
            color="cadetblue")+
  geom_point(aes(x=year, y=DMRproportion),
             color="indianred")+
  geom_line(aes(x=year, y=DMRproportion),
            color="indianred")+
  xlab("")+ylab("Proportion of Samples")

    #need to deal with NAs because they are not accepted in lagged tests )obviously)
      #could interpolate linearly, but there is SO much variability that it seems dumb to do that
      #instead, I am going to replace them with the average value across all the other years. not ideal, but fine for now.
ccf_her$TERNproportion[is.na(ccf_her$TERNproportion)]=mean(ccf_her$TERNproportion,na.rm=TRUE)
ccf_her$DMRproportion[is.na(ccf_her$DMRproportion)]=mean(ccf_her$DMRproportion,na.rm=TRUE)
ccf_her$TERNproportion[is.na(ccf_her$TERNproportion)]=mean(ccf_her$TERNproportion,na.rm=TRUE)
ccf_her$DMRproportion[is.na(ccf_her$DMRproportion)]=mean(ccf_her$DMRproportion,na.rm=TRUE)


    #test for stationarity
        #- Dickey-Fuller test: stationary when p<0.05
adf.test(ccf_her$TERNproportion) # not stationary
adf.test(ccf_her$DMRproportion) # almost stationary

    #test how many differences is necessary to reach stationarity 
ndiffs(ccf_her$TERNproportion, test="adf") # 1
ndiffs(ccf_her$DMRproportion, test="adf") # 0


    #try to force stationarity by differencing 
ccf_her_TERN_1diff<-diff(ccf_her$TERNproportion, differences=1)
ccf_her_DMR_1diff<-diff(ccf_her$DMRproportion, differences=1)
adf.test(ccf_her_TERN_1diff) # not quite
adf.test(ccf_her_DMR_1diff) # stationary

ccf_her_TERN_2diff<-diff(ccf_her$TERNproportion, differences=2)
ccf_her_DMR_2diff<-diff(ccf_her$DMRproportion, differences=2)
adf.test(ccf_her_TERN_2diff) # stationary
adf.test(ccf_her_DMR_2diff) # stationary

    #test autocorrelation on differenced data
acf(ccf_her_TERN_2diff) # this doesnt look great
acf(ccf_her_DMR_1diff) # this looks slightly better

# 
#     #ccf on differenced data
# ccf_res_2diff<-ccf(ccf_her_TERN_2diff, ccf_her_DMR_2diff, 10, pl = TRUE)
# ccf_res_2diff
# lag2.plot(ccf_her_TERN_2diff, ccf_her_DMR_2diff, 10)
# lag2.plot(ccf_her_DMR_2diff, ccf_her_TERN_2diff, 10)  

    #ccf on non-differenced data
ccf_res<-ccf(ccf_her$TERNproportion, ccf_her$DMRproportion, 10, pl = TRUE)
ccf_res
lag2.plot(ccf_her$TERNproportion, ccf_her$DMRproportion, 10) 
lag2.plot(ccf_her$DMRproportion, ccf_her$TERNproportion, 10) 

#GLM: abundance with 5 years of lag, temperature
#_____________________________________________

#add time lags to df
her_lags<-ccf_her %>% 
  dplyr::mutate(DMRproportionLag1=lag(DMRproportion),
                DMRabundanceLag1=lag(DMRabundance)) %>% 
  dplyr::mutate(DMRproportionLag2=lag(DMRproportionLag1),
                DMRabundanceLag2=lag(DMRabundanceLag1)) %>% 
  dplyr::mutate(DMRproportionLag3=lag(DMRproportionLag2),
                DMRabundanceLag3=lag(DMRabundanceLag2)) %>% 
  dplyr::mutate(DMRproportionLag4=lag(DMRproportionLag3),
                DMRabundanceLag4=lag(DMRabundanceLag3)) %>% 
  dplyr::mutate(DMRproportionLag5=lag(DMRproportionLag4),
                DMRabundanceLag5=lag(DMRabundanceLag4)) 



# NMFS DATA DF annual PROP and ABUND of each species -------------------------------------
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Fisheries Data/Prepped Fisheries Data/Newest")

      #this df is made via VERY STUPID AND INCONVENIENT METHODS
     #TO DO: go back and do this a not stupid way

  #FOR WHOLE GOM FALL+SPRING COMBINED#
  #----------------------------------#

NMFSrawGOM<-read.csv("GoM_catch.csv") %>% 
  mutate(Family=Family<-ifelse(grepl(paste(c("HERRING","HAKE","BUTTERFISH","MACKEREL","POLLOCK"),      #this is just to get rid of the space in SAND LANCE so it reads SANDLANCE to match t feeding data
                                       collapse="|"), Family, ignore.case=T), Family, "SANDLANCE"))   # it is a really shitty way to do this though, it only works if the df is arranged like it is now so be careful
  

NMFSdatGOM<-NMFSrawGOM %>% 
  filter(!Year %in% c(1963:1998)) %>% 
  rename(year=Year, prey=Family, catchable_rel_abundance=trawlCatch_catchablePerEffort,
         tot_n=trawlCatch_Num, catchable_n=trawlCatch_catchableNum)  %>% 
  mutate(tot_rel_abundance=(tot_n/Trawls_Efforts)) %>% 
  mutate(adjusted_rel_abundance=(tot_rel_abundance-catchable_rel_abundance))

NMFSdatGOMsub1<-NMFSdatGOM %>% 
  dplyr::select(year, Trawls, Trawls_Efforts, prey) %>% 
  group_by(year, prey) %>% 
  summarise_each(funs(max))

NMFSdatGOMsub2<-NMFSdatGOM %>% 
  dplyr::select(year, prey, tot_n, trawlMeasured_Num, propMeasured, propMeasured_catchable, catchable_n, trawlCatch_catchablePerTrawl,
         catchable_rel_abundance, tot_rel_abundance, adjusted_rel_abundance) %>% 
  group_by(year,prey) %>% 
  summarise_each(funs(sum)) 

NMFSdatGOMsamplesize<-NMFSdatGOMsub2 %>% 
  dplyr::select(year, prey, tot_n, catchable_n) %>% 
  group_by(year) %>% 
  mutate(totAnnualn=sum(tot_n),catchableAnnualn=sum(catchable_n))

NMFSdatGOMmerge<-merge(NMFSdatGOMsub1,NMFSdatGOMsub2)  

NMFSdfGOM<-merge(NMFSdatGOMmerge,NMFSdatGOMsamplesize) %>%
  mutate(tot_proportion=(tot_n/totAnnualn), catchable_proportion=(catchable_n/catchableAnnualn)) %>% 
  dplyr::select(year, prey, tot_n, catchable_n, tot_rel_abundance, catchable_rel_abundance, tot_proportion, catchable_proportion, adjusted_rel_abundance)

  #FOR WHOLE GOM FALL#
  #------------------#
NMFSrawGOMfall<-read.csv("GoM_catch.csv")%>% 
  mutate(Family=Family<-ifelse(grepl(paste(c("HERRING","HAKE","BUTTERFISH","MACKEREL","POLLOCK"),
                                           collapse="|"), Family, ignore.case=T), Family, "SANDLANCE")) 

NMFSdatGOMfall<-NMFSrawGOMfall %>% 
  filter(!Year %in% c(1963:1998)) %>% 
  filter(!season %in% "SPRING") %>% 
  rename(year=Year, prey=Family, catchable_rel_abundance=trawlCatch_catchablePerEffort,
         tot_n=trawlCatch_Num, catchable_n=trawlCatch_catchableNum) %>% 
  mutate(tot_rel_abundance=(tot_n/Trawls_Efforts)) 

NMFSdatGOMsub1fall<-NMFSdatGOMfall %>% 
  dplyr::select(year, Trawls, Trawls_Efforts, prey) %>% 
  group_by(year, prey) %>% 
  summarise_each(funs(max))

NMFSdatGOMsub2fall<-NMFSdatGOMfall %>% 
  dplyr::select(year, prey, tot_n, trawlMeasured_Num, propMeasured, propMeasured_catchable, catchable_n, trawlCatch_catchablePerTrawl,
         catchable_rel_abundance, tot_rel_abundance) %>% 
  group_by(year,prey) %>% 
  summarise_each(funs(sum)) 

NMFSdatGOMsamplesizefall<-NMFSdatGOMsub2fall %>% 
  dplyr:: select(year, prey, tot_n, catchable_n) %>% 
  group_by(year) %>% 
  mutate(totAnnualn=sum(tot_n),catchableAnnualn=sum(catchable_n))

NMFSdatGOMmergefall<-merge(NMFSdatGOMsub1fall,NMFSdatGOMsub2fall)  

NMFSdfGOMfall<-merge(NMFSdatGOMmergefall,NMFSdatGOMsamplesizefall) %>%
  mutate(tot_proportion=(tot_n/totAnnualn), catchable_proportion=(catchable_n/catchableAnnualn)) %>% 
  dplyr::select(year, prey, tot_n, catchable_n, tot_rel_abundance, catchable_rel_abundance, tot_proportion, catchable_proportion)

  #FOR WHOLE GOM SPRING#
  #------------------#

NMFSrawGOMspri<-read.csv("GoM_catch.csv")%>% 
  mutate(Family=Family<-ifelse(grepl(paste(c("HERRING","HAKE","BUTTERFISH","MACKEREL","POLLOCK"),
                                           collapse="|"), Family, ignore.case=T), Family, "SANDLANCE"))

NMFSdatGOMspri<-NMFSrawGOMspri %>% 
  filter(!Year %in% c(1963:1998)) %>% 
  filter(!season %in% "FALL") %>% 
  rename(year=Year, prey=Family, catchable_rel_abundance=trawlCatch_catchablePerEffort,
         tot_n=trawlCatch_Num, catchable_n=trawlCatch_catchableNum) %>% 
  mutate(tot_rel_abundance=(tot_n/Trawls_Efforts)) 

NMFSdatGOMsub1spri<-NMFSdatGOMspri %>% 
  dplyr::select(year, Trawls, Trawls_Efforts, prey) %>% 
  group_by(year, prey) %>% 
  summarise_each(funs(max))

NMFSdatGOMsub2spri<-NMFSdatGOMspri %>% 
  dplyr::  select(year, prey, tot_n, trawlMeasured_Num, propMeasured, propMeasured_catchable, catchable_n, trawlCatch_catchablePerTrawl,
         catchable_rel_abundance, tot_rel_abundance) %>% 
  group_by(year,prey) %>% 
  summarise_each(funs(sum)) 

NMFSdatGOMsamplesizespri<-NMFSdatGOMsub2spri %>% 
  dplyr::  select(year, prey, tot_n, catchable_n) %>% 
  group_by(year) %>% 
  mutate(totAnnualn=sum(tot_n),catchableAnnualn=sum(catchable_n))

NMFSdatGOMmergespri<-merge(NMFSdatGOMsub1spri,NMFSdatGOMsub2spri)  

NMFSdfGOMspri<-merge(NMFSdatGOMmergespri,NMFSdatGOMsamplesizespri) %>%
  mutate(tot_proportion=(tot_n/totAnnualn), catchable_proportion=(catchable_n/catchableAnnualn)) %>% 
  dplyr::select(year, prey, tot_n, catchable_n, tot_rel_abundance, catchable_rel_abundance, tot_proportion, catchable_proportion)

  #FOR 50km GOM FALL+SPRING COMBINED#
  #---------------------------------#

NMFSraw50km<-read.csv("50kmBuffer_catch.csv")%>% 
  mutate(Family=Family<-ifelse(grepl(paste(c("HERRING","HAKE","BUTTERFISH","MACKEREL","POLLOCK"),
                                           collapse="|"), Family, ignore.case=T), Family, "SANDLANCE"))

NMFSdat50km<-NMFSraw50km %>% 
  filter(!Year %in% c(1963:1998)) %>% 
  rename(year=Year, prey=Family, catchable_rel_abundance=trawlCatch_catchablePerEffort,
         tot_n=trawlCatch_Num, catchable_n=trawlCatch_catchableNum) %>% 
  mutate(tot_rel_abundance=(tot_n/Trawls_Efforts)) %>% 
  mutate(adjusted_rel_abundance=(tot_rel_abundance-catchable_rel_abundance))

NMFSdat50kmsub1<-NMFSdat50km %>% 
  select(year, Trawls, Trawls_Efforts, prey) %>% 
  group_by(year, prey) %>% 
  summarise_each(funs(max))

NMFSdat50kmsub2<-NMFSdat50km %>% 
  select(year, prey, tot_n, trawlMeasured_Num, propMeasured, propMeasured_catchable, catchable_n, trawlCatch_catchablePerTrawl,
         catchable_rel_abundance, tot_rel_abundance,adjusted_rel_abundance) %>% 
  group_by(year,prey) %>% 
  summarise_each(funs(sum)) 

NMFSdat50kmsamplesize<-NMFSdat50kmsub2 %>% 
  select(year, prey, tot_n, catchable_n) %>% 
  group_by(year) %>% 
  mutate(totAnnualn=sum(tot_n),catchableAnnualn=sum(catchable_n))

NMFSdat50kmmerge<-merge(NMFSdat50kmsub1,NMFSdat50kmsub2)  

NMFSdf50km<-merge(NMFSdat50kmmerge,NMFSdat50kmsamplesize) %>%
  mutate(tot_proportion=(tot_n/totAnnualn), catchable_proportion=(catchable_n/catchableAnnualn)) %>% 
  select(year, prey, tot_n, catchable_n, tot_rel_abundance, catchable_rel_abundance, tot_proportion, catchable_proportion,adjusted_rel_abundance)
    #this has some negative values for butterfish and blueback that seem to come from scenarios in which catchable abundance is pretty much equal to total abudance 
  

  #FOR 50km buffer FALL#
  #--------------------#

NMFSraw50kmfall<-read.csv("GoM_catch.csv")%>% 
  mutate(Family=Family<-ifelse(grepl(paste(c("HERRING","HAKE","BUTTERFISH","MACKEREL","POLLOCK"),
                                           collapse="|"), Family, ignore.case=T), Family, "SANDLANCE"))

NMFSdat50kmfall<-NMFSraw50kmfall %>% 
  filter(!Year %in% c(1963:1998)) %>% 
  filter(!season %in% "SPRING") %>% 
  rename(year=Year, prey=Family, catchable_rel_abundance=trawlCatch_catchablePerEffort,
         tot_n=trawlCatch_Num, catchable_n=trawlCatch_catchableNum) %>% 
  mutate(tot_rel_abundance=(tot_n/Trawls_Efforts)) 

NMFSdat50kmsub1fall<-NMFSdat50kmfall %>% 
  select(year, Trawls, Trawls_Efforts, prey) %>% 
  group_by(year, prey) %>% 
  summarise_each(funs(max))

NMFSdat50kmsub2fall<-NMFSdat50kmfall %>% 
  select(year, prey, tot_n, trawlMeasured_Num, propMeasured, propMeasured_catchable, catchable_n, trawlCatch_catchablePerTrawl,
         catchable_rel_abundance, tot_rel_abundance) %>% 
  group_by(year,prey) %>% 
  summarise_each(funs(sum)) 

NMFSdat50kmsamplesizefall<-NMFSdat50kmsub2fall %>% 
  select(year, prey, tot_n, catchable_n) %>% 
  group_by(year) %>% 
  mutate(totAnnualn=sum(tot_n),catchableAnnualn=sum(catchable_n))

NMFSdat50kmmergefall<-merge(NMFSdat50kmsub1fall,NMFSdat50kmsub2fall)  

NMFSdf50kmfall<-merge(NMFSdat50kmmergefall,NMFSdat50kmsamplesizefall) %>%
  mutate(tot_proportion=(tot_n/totAnnualn), catchable_proportion=(catchable_n/catchableAnnualn)) %>% 
  select(year, prey, tot_n, catchable_n, tot_rel_abundance, catchable_rel_abundance, tot_proportion, catchable_proportion)

  #FOR 50km buffer SPRING#
  #----------------------#

NMFSraw50kmspri<-read.csv("GoM_catch.csv")%>% 
  mutate(Family=Family<-ifelse(grepl(paste(c("HERRING","HAKE","BUTTERFISH","MACKEREL","POLLOCK"),
                                           collapse="|"), Family, ignore.case=T), Family, "SANDLANCE"))

NMFSdat50kmspri<-NMFSraw50kmspri %>% 
  filter(!Year %in% c(1963:1998)) %>% 
  filter(!season %in% "FALL") %>% 
  rename(year=Year, prey=Family, catchable_rel_abundance=trawlCatch_catchablePerEffort,
         tot_n=trawlCatch_Num, catchable_n=trawlCatch_catchableNum) %>% 
  mutate(tot_rel_abundance=(tot_n/Trawls_Efforts)) 

NMFSdat50kmsub1spri<-NMFSdat50kmspri %>% 
  select(year, Trawls, Trawls_Efforts, prey) %>% 
  group_by(year, prey) %>% 
  summarise_each(funs(max))

NMFSdat50kmsub2spri<-NMFSdat50kmspri %>% 
  select(year, prey, tot_n, trawlMeasured_Num, propMeasured, propMeasured_catchable, catchable_n, trawlCatch_catchablePerTrawl,
         catchable_rel_abundance, tot_rel_abundance) %>% 
  group_by(year,prey) %>% 
  summarise_each(funs(sum)) 

NMFSdat50kmsamplesizespri<-NMFSdat50kmsub2spri %>% 
  select(year, prey, tot_n, catchable_n) %>% 
  group_by(year) %>% 
  mutate(totAnnualn=sum(tot_n),catchableAnnualn=sum(catchable_n))

NMFSdat50kmmergespri<-merge(NMFSdat50kmsub1spri,NMFSdat50kmsub2spri)  

NMFSdf50kmspri<-merge(NMFSdat50kmmergespri,NMFSdat50kmsamplesizespri) %>%
  mutate(tot_proportion=(tot_n/totAnnualn), catchable_proportion=(catchable_n/catchableAnnualn)) %>% 
  select(year, prey, tot_n, catchable_n, tot_rel_abundance, catchable_rel_abundance, tot_proportion, catchable_proportion)






# LAGGED REGRESSION MuMin -------------------------------------------------------

### MAINE DMR DATA, SST, CHlA for HERRING ###

#   # Bring in data and clean
# setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Fisheries Data/MEDMR Inshore Trawl")
# DMR_herring <- read.csv("annual_DMR_herring_summary.csv", header=TRUE) %>% 
#   dplyr::select (Year, TotCatch, Prop) %>% 
#   dplyr::rename(DMRProp=Prop, DMRAbund=TotCatch)
# 
# setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Tern Provisioning Data")
# tern_herring <- read.csv("annual_tern_herring_summary.csv", header=TRUE) %>% 
#   dplyr::select (year, proportion, abundance) %>% 
#   dplyr::rename(Year=year, TernProp=proportion, TernAbund=abundance)
# 
# setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data")
# chl_a <- read.csv("chla_erddap_seasonal_summary_GOM.csv", header=TRUE) %>% 
#   dplyr::filter(Season == "summer") %>% 
#   dplyr::select(Year, minchla, meanchla, maxchla)
# 
# sst <- read.csv("sst_erddap_seasonal_summary_GOM.csv", header=TRUE)%>% 
#   dplyr::filter(Season == "summer") %>% 
#   dplyr::select(Year, minSST, meanSST, maxSST)
# 
# 
#   #merge data
# ex <- full_join(DMR_herring,tern_herring, by="Year")
# ex <- full_join(ex,chl_a, by="Year")
# ex <- full_join(ex, sst, by="Year")
# 
# 
#   # clean up data frame (no NAs please!) 
# table(is.na(ex)) 
# ex.clean <- ex[complete.cases(ex),]
# table(is.na(ex.clean))
# 
# 
# 
# 
#   #Make a global model with no time lags for ABUNDANCE 
# 
#       #run mode (remember, no NAs)
# glm.ex <- glm(TernAbund~DMRAbund + meanchla +meanSST, data=ex.clean)
# summary(glm.ex)
# AIC(glm.ex)
# 
# library(MuMIn)
# options(na.action = "na.fail")
# ex.dredge <- dredge(glm.ex, rank="AIC")
# ex.dredge
# 
#       #get summary of n model
# ex.get <- get.models(ex.dredge,subset = delta < 2)
# ex.get
# summary(ex.get[[1]]) 
#       
# #model averaging if no clear "winner"
# ex.avg <- model.avg(ex.get)
# ex.avg
# summary(ex.avg)
#       


  #Make a global model for ABUNDANCE with + 2 yrs of time lag and corrected for sizes that correspond with year 0 tern

          #this is size corrected DMR herring genereated in code above
# DMR_herring <- DMR_herr_lags012 %>% 
#   dplyr::rename(DMRProp=PropYr0, DMRAbund=TotCatchYr0,
#                 DMRProp1=PropYr1, DMRAbund1=TotCatchYr1,
#                 DMRProp2=PropYr2, DMRAbund2=TotCatchYr2)

setwd("/Users/aliyacaldwell/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Diet--Fisheries/Data/Fisheries Data/MEDMR Inshore Trawl")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Fisheries Data/MEDMR Inshore Trawl")
DMR_herring <- read.csv("DMR_herring_size_corrected.csv", header=TRUE) %>% 
     dplyr::rename(DMRProp=PropYr0, DMRAbund=TotCatchYr0,
                   DMRProp1=PropYr1, DMRAbund1=TotCatchYr1,
                   DMRProp2=PropYr2, DMRAbund2=TotCatchYr2)

setwd("/Users/aliyacaldwell/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Diet--Fisheries/Data/Tern Provisioning Data")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Tern Provisioning Data")
tern_herring <- read.csv("annual_tern_herring_summary.csv", header=TRUE) %>% 
  dplyr::select (year, proportion, abundance) %>% 
  dplyr::rename(Year=year, TernProp=proportion, TernAbund=abundance)

setwd("/Users/aliyacaldwell/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data")
chl_a <- read.csv("chla_erddap_seasonal_summary_GOM.csv", header=TRUE) %>% 
  dplyr::filter(Season == "summer") %>% 
  dplyr::select(Year, minchla, meanchla, maxchla)

sst <- read.csv("sst_erddap_seasonal_summary_GOM.csv", header=TRUE)%>% 
  dplyr::filter(Season == "summer") %>% 
  dplyr::select(Year, minSST, meanSST, maxSST)

ex.lagged <- full_join(DMR_herring,tern_herring, by="Year")
ex.lagged <- full_join(ex.lagged,chl_a, by="Year")
ex.lagged <- full_join(ex.lagged, sst, by="Year") %>% 
  mutate_at(c(4:7), as.numeric)

str(ex.lagged)

#     #add lags if not size correcting
# ex.lagged<-ex %>% 
#   mutate(DMRAbundLag1=lag(DMRAbund, n=1, default=NA),
#          DMRAbundLag2=lag(DMRAbund, n=2, default=NA),
#          DMRAbundLag3=lag(DMRAbund, n=3, default=NA),
#          DMRAbundLag4=lag(DMRAbund, n=4, default=NA))

    # clean up data frame (no NAs please!) 
table(is.na(ex.lagged)) 
ex.clean <- ex.lagged[complete.cases(ex.lagged),]
table(is.na(ex.clean))

    #run global mode (remember, no NAs)
glm.ex <- glm(TernAbund~DMRAbund + DMRAbund1+ DMRAbund2+ meanchla +meanSST, data=ex.clean)
summary(glm.ex)
AIC(glm.ex) #AIC of global model 

library(MuMIn)
options(na.action = "na.fail")
ex.dredge <- dredge(glm.ex, rank="AIC")
ex.dredge

    #get summary of models with delta>2
ex.get <- get.models(ex.dredge,subset = delta < 2)
ex.get
summary(ex.get[[1]]) 

    #model averaging if no clear "winner"
ex.avg <- model.avg(ex.get)
ex.avg
summary(ex.avg)

    #plot some of the relationships 
ggplot(data=ex.lagged, aes(TernAbund, DMRAbund))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(data=ex.lagged, aes(TernAbund, DMRAbund1))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(data=ex.lagged, aes(TernAbund, DMRAbund2))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(data=ex.lagged, aes(TernAbund, meanSST))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(data=ex.lagged, aes(TernAbund, meanchla))+
  geom_point()+
  geom_smooth(method='lm')



  #Make a global model for PROPORTION with + 2 yrs of time lag and corrected for sizes that correspond with year 0 tern
      #this is size corrected DMR herring genereated in code above
# DMR_herring <- DMR_herr_lags012 %>% 
#   dplyr::rename(DMRProp=PropYr0, DMRAbund=TotCatchYr0,
#                 DMRProp1=PropYr1, DMRAbund1=TotCatchYr1,
#                 DMRProp2=PropYr2, DMRAbund2=TotCatchYr2)
setwd("/Users/aliyacaldwell/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Diet--Fisheries/Data/Fisheries Data/MEDMR Inshore Trawl")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Fisheries Data/MEDMR Inshore Trawl")
DMR_herring <- read.csv("DMR_herring_size_corrected.csv", header=TRUE) %>% 
  dplyr::rename(DMRProp=PropYr0, DMRAbund=TotCatchYr0,
                DMRProp1=PropYr1, DMRAbund1=TotCatchYr1,
                DMRProp2=PropYr2, DMRAbund2=TotCatchYr2)

setwd("/Users/aliyacaldwell/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Diet--Fisheries/Data/Tern Provisioning Data")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Tern Provisioning Data")
tern_herring <- read.csv("annual_tern_herring_summary.csv", header=TRUE) %>% 
  dplyr::select (year, proportion, abundance) %>% 
  dplyr::rename(Year=year, TernProp=proportion, TernAbund=abundance)

setwd("/Users/aliyacaldwell/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data")
chl_a <- read.csv("chla_erddap_seasonal_summary_GOM.csv", header=TRUE) %>% 
  dplyr::filter(Season == "summer") %>% 
  dplyr::select(Year, minchla, meanchla, maxchla)

sst <- read.csv("sst_erddap_seasonal_summary_GOM.csv", header=TRUE)%>% 
  dplyr::filter(Season == "summer") %>% 
  dplyr::select(Year, minSST, meanSST, maxSST)

ex.lagged <- full_join(DMR_herring,tern_herring, by="Year")
ex.lagged <- full_join(ex.lagged,chl_a, by="Year")
ex.lagged <- full_join(ex.lagged, sst, by="Year")


#     #add lags if not size correcting
# ex.lagged<-ex %>% 
#   mutate(DMRAbundLag1=lag(DMRAbund, n=1, default=NA),
#          DMRAbundLag2=lag(DMRAbund, n=2, default=NA),
#          DMRAbundLag3=lag(DMRAbund, n=3, default=NA),
#          DMRAbundLag4=lag(DMRAbund, n=4, default=NA))

    # clean up data frame (no NAs please!) 
table(is.na(ex.lagged)) 
ex.clean <- ex.lagged[complete.cases(ex.lagged),]
table(is.na(ex.clean))

    #run model (remember, no NAs)
glm.ex <- glm(TernProp~DMRProp + DMRProp1+ DMRProp2+ meanchla +meanSST, data=ex.clean)
summary(glm.ex)
AIC(glm.ex)

options(na.action = "na.fail")
ex.dredge <- dredge(glm.ex, rank="AIC")
ex.dredge

    #get summary of n model
ex.get <- get.models(ex.dredge,subset = delta < 2)
ex.get
summary(ex.get[[1]]) 

    #model averaging if no clear "winner"
ex.avg <- model.avg(ex.get)
ex.avg
summary(ex.avg)

    #plot some of the relationships 

ggplot(data=ex.lagged, aes(TernProp, DMRProp))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(data=ex.lagged, aes(TernProp, DMRProp1))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(data=ex.lagged, aes(TernProp, DMRProp2))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(data=ex.lagged, aes(TernProp, meanSST))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(data=ex.lagged, aes(TernProp, meanchla))+
  geom_point()+
  geom_smooth(method='lm')




### NMFS FALL DATA, SST, CHlA for HERRING  ###

  #env data
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Environmental Data")
chl_a <- read.csv("chla_erddap_seasonal_summary_GOM.csv", header=TRUE) %>% 
  dplyr::filter(Season == "summer") %>% 
  dplyr::select(Year, minchla, meanchla, maxchla)

sst <- read.csv("sst_erddap_seasonal_summary_GOM.csv", header=TRUE)%>% 
  dplyr::filter(Season == "summer") %>% 
  dplyr::select(Year, minSST, meanSST, maxSST)


  #create combined df of NMFS and tern diet data

tprov_herr
          #or
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Tern Provisioning Data")
tern_herring <- read.csv("annual_tern_herring_summary.csv", header=TRUE) %>% 
  dplyr::select (year, proportion, abundance) %>% 
  dplyr::rename(Year=year, TernProp=proportion, TernAbund=abundance)

NMFS_herring<-subset(NMFSdfGOMfall, prey=="HERRING") %>% 
  rename(Year=year, NMFSAbund=tot_rel_abundance, NMFSProp=tot_proportion) %>% 
  select(Year,NMFSAbund, NMFSProp)

NMFS_herring[,2]=tolower(NMFS_herring[,2])  


  #merge data
ex <- full_join(NMFS_herring,tern_herring, by="Year")
ex <- full_join(ex,chl_a, by="Year")
ex <- full_join(ex, sst, by="Year")


  # clean up data frame (no NAs please!) 
table(is.na(ex)) 
ex.clean <- ex[complete.cases(ex),]
table(is.na(ex.clean))




  #Make a global model with no time lags

    #run mode (remember, no NAs)
glm.ex <- glm(TernAbund~NMFSAbund + meanchla +meanSST, data=ex.clean)
summary(glm.ex)
AIC(glm.ex)


library(MuMIn)
options(na.action = "na.fail")
ex.dredge <- dredge(glm.ex, rank="AIC")
ex.dredge

    #get summary of n model
ex.get <- get.models(ex.dredge,subset = delta < 2)
ex.get
summary(ex.get[[1]]) 

    #model averaging if no clear "winner"
ex.avg <- model.avg(ex.get)
ex.avg
summary(ex.avg)



  #Make a global model with + 4 yrs of time lag

    #add lags
ex.lagged<-ex %>% 
  mutate(NMFSAbundLag1=lag(NMFSAbund, n=1, default=NA),
         NMFSAbundLag2=lag(NMFSAbund, n=2, default=NA),
         NMFSAbundLag3=lag(NMFSAbund, n=3, default=NA),
         NMFSAbundLag4=lag(NMFSAbund, n=4, default=NA))

    # clean up data frame (no NAs please!) 
table(is.na(ex.lagged)) 
ex.clean <- ex.lagged[complete.cases(ex.lagged),]
table(is.na(ex.clean))

    #run mode (remember, no NAs)
glm.ex <- glm(TernAbund~NMFSAbund + NMFSAbundLag1+ NMFSAbundLag2+ NMFSAbundLag3+ NMFSAbundLag4+ meanchla +meanSST, data=ex.clean)
summary(glm.ex)
AIC(glm.ex)

options(na.action = "na.fail")
ex.dredge <- dredge(glm.ex, rank="AIC")
ex.dredge

    #get summary of n model
ex.get <- get.models(ex.dredge,subset = delta < 2)
ex.get
summary(ex.get[[1]]) 

    #model averaging if no clear "winner"
ex.avg <- model.avg(ex.get)
ex.avg
summary(ex.avg)

    #plot some of the relationships 

ggplot(data=ex.lagged)+
  geom_point(aes(TernAbund, NMFSAbund))
ggplot(data=ex.lagged)+
  geom_point(aes(TernAbund, NMFSAbundLag1))
ggplot(data=ex.lagged)+
  geom_point(aes(TernAbund, NMFSAbundLag2))
ggplot(data=ex.lagged)+
  geom_point(aes(TernAbund, NMFSAbundLag3))
ggplot(data=ex.lagged)+
  geom_point(aes(TernAbund, meanSST)) 
ggplot(data=ex.lagged)+
  geom_point(aes(TernAbund, meanchla)) 




#

# CROSS CORRELATION ANALYSES ----------------------------------------------


#==========================================================#
## CROSS CORRELATION BETWEEN TERN DIET AND NMFS DATA ##
#==========================================================#
library(stats)
library(tseries)
library(astsa)
library(forecast)

  #grab tprov sub from below
tprovsub<-tprov %>%
  mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","hake","butterfish","sandlance","mackerel","pollock","unknown fish"),
                                             collapse="|"), prey, ignore.case=T), prey, "other")) 


  #------------------------------------------#
  # NMFS from WHOLE GOM FALL+SPRING COMBINED #
  #------------------------------------------#

  #create combined df of NMFS and tern diet data
     #subset dfs first
ccfTERN<-tprovsub %>% 
  filter(!preysub %in% c("other","unknown fish"))

ccfTERN[,2]=toupper(ccfTERN[,2]) #make prey uppercase

ccfNMFS<-NMFSdfGOM 

ccfdf<-full_join(ccfNMFS, ccfTERN) #automatically sorted by prey and year but make sure this is always the case
  
ccfdf[is.na(ccfdf)]=0 #turn all NA into zeros (necessary for ccf function and also logical). NEED TO BE CAREFUL WITH 2016 in the tern data and 2020 in the NMFS data because this makes them read as 0 when those are actually NAs


#HERRING relative abundance#
#--------------------------#
ccfdfHERR<-ccfdf %>% 
  filter(prey %in% "HERRING")

#visualize both time series raw
ggplot(ccfdfHERR)+theme_bw()+
  geom_point(aes(x=year, y=abundance))+
  geom_line(aes(x=year, y=abundance))+xlab("Year")+ylab("rel abundance terns")

ggplot(ccfdfHERR)+theme_bw()+
  geom_point(aes(x=year, y=tot_rel_abundance))+
  geom_line(aes(x=year, y=tot_rel_abundance))+xlab("Year")+ylab("rel abundance nmfs")

 #test for stationarity
        #- KPSS test: stationary when p>0.05
        #- Duckey-Fuller test: stationary when p<0.05
adf.test(ccfdfHERR$tot_rel_abundance) #NMFS data  -->result: non stationary
adf.test(ccfdfHERR$abundance) #TERN data  -->result: non stationary
kpss.test(ccfdfHERR$tot_rel_abundance, null="Trend") # NMFS data  -->result: stationary
kpss.test(ccfdfHERR$abundance, null="Trend") # TERN data  -->result:  stationary

  #test how many differences is necessary to reach stationarity 
ndiffs(ccfdfHERR$tot_rel_abundance) #--> claims it only needs 1
ndiffs(ccfdfHERR$abundance) #--> claims it only needs 0


  #try to force stationarity by differencing 
diffHERRtern<-diff(ccfdfHERR$abundance, differences=1)
diffHERRnmfs<-diff(ccfdfHERR$tot_rel_abundance, differences=1)

  
  #test autocorrelation and re test for stationarity for differenced data
acf(diffHERRtern) #--> this makes it look good for stationarity because it quickly falls to or around zero after the first (0) lag (i.e. doesnt decrease to zero gradually)
acf(diffHERRnmfs) #---> this also looks good for stationarity 
    #- KPSS test: stationary when p>0.05
    #- Duckey-Fuller test: stationary when p<0.05
adf.test(diffHERRtern) #--> result: still not stationary
adf.test(diffHERRnmfs) # result: still not stationary
kpss.test(diffHERRtern, null="Trend") #--> result: stationary
kpss.test(diffHERRnmfs, null="Trend") # result: stationary
    #TO DO: understand differences between acf and kpss to understand why they disagree


  #perform ccf for relative abundance on differenced data
ccfvaluesHERRdiff<-ccf(diffHERRnmfs, diffHERRtern, 10, correlation = TRUE, pl = TRUE)
ccfvaluesHERRdiff
ccfvaluesHERRdiffcorrplot<-lag2.plot(diffHERRnmfs, diffHERRtern, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this
  lag2.plot(diffHERRtern, diffHERRnmfs, 10) #switching the X and Y appears to show positive lags within the context of the previous which makes sense 
  
  #perform ccf for relative abundance on non-differenced data
ccfvaluesHERR<-ccf(ccfdfHERR$tot_rel_abundance, ccfdfHERR$abundance, 10, correlation = TRUE, pl = TRUE)
ccfvaluesHERR

    #side note TO DO: test of how corrected herring tracks (hypothesis is for strong positive correlation at lag0)
    #another side note TO DO: make a column that is non-adjusted fish minus adjusted fish abundance and test if correlation is better

#HERRING proportion in diet#
#--------------------------#
ccfdfHERR<-ccfdf %>% 
  filter(prey %in% "HERRING")

  #visualize both time series raw
ggplot(ccfdfHERR)+theme_bw()+
  geom_point(aes(x=year, y=proportion))+
  geom_line(aes(x=year, y=proportion))+xlab("Year")+ylab("prop terns")
ggplot(ccfdfHERR)+theme_bw()+
  geom_point(aes(x=year, y=tot_proportion))+
  geom_line(aes(x=year, y=tot_proportion))+xlab("Year")+ylab("prop nmfs")

  #test for stationarity
   #- KPSS test: non-stationary when p<0.05
    #- Duckey-Fuller test: non-stationary when p>0.05
adf.test(ccfdfHERR$tot_proportion) #NMFS data  -->result: stationary
adf.test(ccfdfHERR$proportion) #TERN data  -->result: NON stationary
kpss.test(ccfdfHERR$tot_proportion, null="Trend") # NMFS data  -->result: stationary
kpss.test(ccfdfHERR$proportion, null="Trend") # TERN data  -->result: stationary

  #test how many differences is necessary to reach stationarity 
ndiffs(ccfdfHERR$tot_proportion) #--> claims it only needs 1
ndiffs(ccfdfHERR$proportion) #--> claims it only needs 0


  #try to force stationarity by differencing 
diffHERRtern<-diff(ccfdfHERR$proportion, differences=1)
diffHERRnmfs<-diff(ccfdfHERR$tot_proportion, differences=1)


  #test autocorrelation and re test for stationarity for differenced data
acf(diffHERRtern) #--> this makes it look good for stationarity because it quickly falls to or around zero after the first (0) lag
acf(diffHERRnmfs) #---> this also looks good for stationarity 
adf.test(diffHERRtern) #--> result: stationary
adf.test(diffHERRnmfs) # result: non stationary
kpss.test(diffHERRtern, null="Trend") #--> result: stationary ish but not really (0.06)
kpss.test(diffHERRnmfs, null="Trend") # result: stationary
    #TO DO: understand differences between acf and kpss to understand why they disagree


  #perform ccf for proportion on differenced data
ccfvaluesHERRdiffprop<-ccf(diffHERRnmfs, diffHERRtern, 10, correlation = TRUE, pl = TRUE)
ccfvaluesHERRdiffprop
ccfvaluesHERRdiffpropcorrplot<-lag2.plot(diffHERRnmfs, diffHERRtern, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this
lag2.plot(diffHERRtern, diffHERRnmfs, 10) #switching the X and Y appears to show positive lags within the context of the previous which makes sense 

  #perform ccf for proportion on NON differenced data
ccfvaluesHERRdiffprop<-ccf(ccfdfHERR$tot_proportion,ccfdfHERR$proportion, 10, correlation = TRUE, pl = TRUE)
ccfvaluesHERRdiffprop
#side note TO DO: test of how corrected herring tracks (hypothesis is for strong positive correlation at lag0)
#another side note TO DO: make a column that is non-adjusted fish minus adjusted fish abundance and test if correlation is better



# HAKE relative abundance #
#------------------------#
ccfdfHAKE<-ccfdf %>% 
  filter(prey %in% "HAKE")

  #visualize both time series raw
ggplot(ccfdfHAKE)+theme_bw()+
  geom_point(aes(x=year, y=abundance))+
  geom_line(aes(x=year, y=abundance))+xlab("Year")+ylab("rel abundance terns")
ggplot(ccfdfHAKE)+theme_bw()+
  geom_point(aes(x=year, y=tot_rel_abundance))+
  geom_line(aes(x=year, y=tot_rel_abundance))+xlab("Year")+ylab("rel abundance nmfs")

  #test for stationarity
    #- KPSS test: non-stationary when p<0.05
    #- Duckey-Fuller test: non-stationary when p>0.05
adf.test(ccfdfHAKE$tot_rel_abundance) #NMFS data  --> not stationary
adf.test(ccfdfHAKE$abundance) #TERN data --> not stationary but almost lol (0.07)
kpss.test(ccfdfHAKE$tot_rel_abundance, null="Trend") # NMFS data --> stationary
kpss.test(ccfdfHAKE$abundance, null="Trend") # TERN data --> not stationary
  
  #test how many differences is necessary to reach stationarity 
ndiffs(ccfdfHAKE$tot_re_abundance) #--> claims it only needs 0
ndiffs(ccfdfHAKE$abundance) #--> claims it only needs 1


  #try to force stationarity by differencing 
diffHAKEtern<-diff(ccfdfHAKE$abundance, differences=1)
diffHAKEnmfs<-diff(ccfdfHAKE$tot_rel_abundance, differences=1)


  #test autocorrelation and re test for stationarity for differenced data
acf(diffHAKEtern) #--> this makes it look good for stationarity because it quickly falls to or around zero after the first (0) lag
acf(diffHAKEnmfs) #---> this also looks good for stationarity 
adf.test(diffHAKEtern) #--> result: stationary
adf.test(diffHAKEnmfs) # result: non stationary
kpss.test(diffHAKEtern, null="Trend") #--> result: stationary
kpss.test(diffHAKEnmfs, null="Trend") # result: stationary

  #perform ccf for relative abundance
ccfvaluesHAKEdiff<-ccf(diffHAKEnmfs,diffHAKEtern, 20, correlation = TRUE, pl = TRUE)
ccfvaluesHAKEdiff
ccfvaluesHAKEdiffcorrplot<-lag2.plot(diffHAKEnmfs,diffHAKEtern, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this

  #perform ccf for relative abundance on non-differenced data
ccfvaluesHAKE<-ccf(ccfdfHERR$tot_rel_abundance, ccfdfHERR$abundance, 10, correlation = TRUE, pl = TRUE)
ccfvaluesHAKE

#BUTTERFISH relative abundance#
#-----------------------------#
ccfdfBUTT<-ccfdf %>% 
  filter(prey %in% "BUTTERFISH")

  #visualize both time series raw
ggplot(ccfdfBUTT)+theme_bw()+
  geom_point(aes(x=year, y=abundance))+
  geom_line(aes(x=year, y=abundance))+xlab("Year")+ylab("rel abundance terns")
ggplot(ccfdfBUTT)+theme_bw()+
  geom_point(aes(x=year, y=tot_rel_abundance))+
  geom_line(aes(x=year, y=tot_rel_abundance))+xlab("Year")+ylab("rel abundance nmfs")

  #test for stationarity
    #- KPSS test: non-stationary when p<0.05
    #- Duckey-Fuller test: non-stationary when p>0.05
adf.test(ccfdfBUTT$tot_rel_abundance) #NMFS data  --> not stationary
adf.test(ccfdfBUTT$abundance) #TERN data --> not stationary
kpss.test(ccfdfBUTT$tot_rel_abundance) # NMFS data --> stationary
kpss.test(ccfdfBUTT$abundance) # TERN data --> stationary

  #test how many differences is necessary to reach stationarity 
ndiffs(ccfdfBUTT$tot_re_abundance) #--> claims it only needs 0
ndiffs(ccfdfBUTT$abundance) #--> claims it only needs 0
    # so we are not gonna difference for this one

  #create vectors for ccf 
BUTTtern<-ccfdfBUTT$abundance
BUTTnmfs<-ccfdfBUTT$tot_rel_abundance

  #test autocorrelation 
acf(BUTTtern) #--> no evidence of autocorr
acf(BUTTnmfs) #---> no evidence of autocorr

  #perform ccf for relative abundance
ccfvaluesBUTTd<-ccf(BUTTnmfs,BUTTtern, 20, correlation = TRUE, pl = TRUE)
ccfvaluesBUTT
ccfvaluesBUTTcorrplot<-lag2.plot(BUTTnmfs,BUTTtern, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this

#MACKEREL relative abundance#
#---------------------------#
ccfdfMACK<-ccfdf %>% 
  filter(prey %in% "MACKEREL")

  #visualize both time series raw
ggplot(ccfdfMACK)+theme_bw()+
  geom_point(aes(x=year, y=abundance))+
  geom_line(aes(x=year, y=abundance))+xlab("Year")+ylab("rel abundance terns")
ggplot(ccfdfMACK)+theme_bw()+
  geom_point(aes(x=year, y=tot_rel_abundance))+
  geom_line(aes(x=year, y=tot_rel_abundance))+xlab("Year")+ylab("rel abundance nmfs")

  #test for stationarity
    #- KPSS test: non-stationary when p<0.05
    #- Dickey-Fuller test: non-stationary when p>0.05
adf.test(ccfdfMACK$tot_rel_abundance) #NMFS data --> non stationary 
adf.test(ccfdfMACK$abundance) #TERN data  -->non stationary
kpss.test(ccfdfMACK$tot_rel_abundance, null="Trend") # NMFS data --> stationary
kpss.test(ccfdfMACK$abundance, null="Trend") # TERN data --> stationary

  #create vectors for ccf
MACKtern<-ccfdfMACK$abundance
MACKnmfs<-ccfdfMACK$tot_rel_abundance

  #test autocorrelationand
acf(MACKtern) #--> not sure how to interpret this
acf(MACKnmfs) #---> honestly also not really sure how to interpret this right now

  #perform ccf for relative abundance
ccfvaluesMACK<-ccf(MACKnmfs,MACKtern, 20, correlation = TRUE, pl = TRUE)
ccfvaluesMACK #really unsure how to interpret this

ccfvaluesMACKcorrplot<-lag2.plot(MACKnmfs,MACKtern, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this


  #-------------------------------#
  # NMFS from WHOLE GOM FALL ONLY #
  #-------------------------------#

#create combined df of NMFS and tern diet data

ccfTERN<-tprovsub %>% 
  filter(!preysub %in% c("other","unknown fish")) %>% 
  filter(! year %in% c(2020)) #can get rid of 2020 because it is at the end of the TS and wont shift anything

ccfTERN[,2]=toupper(ccfTERN[,2]) #make prey uppercase
  
ccfNMFS<-NMFSdfGOMfall %>% 
  filter(! year %in% c(2020))

ccfdf<-full_join(ccfNMFS, ccfTERN) #automatically sorted by prey and year but make sure this is always the case

ccfdf[is.na(ccfdf)]=0 #turn all NA into zeros (necessary for ccf function and also logical )

  #HERRING relative abundance#
  #--------------------------#
ccfdfHERR<-ccfdf %>% 
  filter(prey %in% "HERRING")

    #visualize both time series raw
ggplot(ccfdfHERR)+theme_bw()+
  geom_point(aes(x=year, y=abundance))+
  geom_line(aes(x=year, y=abundance))+xlab("Year")+ylab("rel abundance terns")+
  scale_x_continuous(breaks=seq(1999,2020,1))
ggplot(ccfdfHERR)+theme_bw()+
  geom_point(aes(x=year, y=tot_rel_abundance))+
  geom_line(aes(x=year, y=tot_rel_abundance))+xlab("Year")+ylab("rel abundance nmfs")+
  scale_x_continuous(breaks=seq(1999,2020,1))

    #test for stationarity
    #- KPSS test: stationary when p>0.05
    #- Duckey-Fuller test: stationary when p<0.05
adf.test(ccfdfHERR$tot_rel_abundance) #NMFS data  -->result: non stationary
adf.test(ccfdfHERR$abundance) #TERN data  -->result: non stationary
kpss.test(ccfdfHERR$tot_rel_abundance, null="Trend") # NMFS data  -->result: stationary
kpss.test(ccfdfHERR$abundance, null="Trend") # TERN data  -->result:  stationary

    #test how many differences is necessary to reach stationarity 
ndiffs(ccfdfHERR$tot_rel_abundance) #--> claims it only needs 1
ndiffs(ccfdfHERR$abundance) #--> claims it only needs 0


    #try to force stationarity by differencing 
diffHERRtern<-diff(ccfdfHERR$abundance, differences=1)
diffHERRnmfs<-diff(ccfdfHERR$tot_rel_abundance, differences=1)


    #test autocorrelation and re test for stationarity for differenced data
acf(diffHERRtern) #--> this makes it look good for stationarity because it quickly falls to or around zero after the first (0) lag (i.e. doesnt decrease to zero gradually)
acf(diffHERRnmfs) #---> this also looks good for stationarity 
      #- KPSS test: stationary when p>0.05
      #- Duckey-Fuller test: stationary when p<0.05
adf.test(diffHERRtern) #--> result: still not stationary
adf.test(diffHERRnmfs) # result: still not stationary
kpss.test(diffHERRtern, null="Trend") #--> result: stationary
kpss.test(diffHERRnmfs, null="Trend") # result: stationary
    #TO DO: understand differences between acf and kpss to understand why they disagree


    #perform ccf for relative abundance on differenced data
ccfvaluesHERRdiff<-ccf(diffHERRnmfs, diffHERRtern, 10, correlation = TRUE, pl = TRUE)
ccfvaluesHERRdiff
ccfvaluesHERRdiffcorrplot<-lag2.plot(diffHERRnmfs, diffHERRtern, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this
lag2.plot(diffHERRtern, diffHERRnmfs, 10) #switching the X and Y appears to show positive lags within the context of the previous which makes sense 

    #perform ccf for relative abundance on non-differenced data
ccfvaluesHERR<-ccf(ccfdfHERR$tot_rel_abundance, ccfdfHERR$abundance, 10, correlation = TRUE, pl = TRUE)
ccfvaluesHERR

    #side note TO DO: test of how corrected herring tracks (hypothesis is for strong positive correlation at lag0)
    #another side note TO DO: make a column that is non-adjusted fish minus adjusted fish abundance and test if correlation is better

  #HERRING proportion in diet#
  #--------------------------#
ccfdfHERR<-ccfdf %>% 
  filter(prey %in% "HERRING")

    #visualize both time series raw
ggplot(ccfdfHERR)+theme_bw()+
  geom_point(aes(x=year, y=proportion))+
  geom_line(aes(x=year, y=proportion))+xlab("Year")+ylab("prop terns")+
  scale_x_continuous(breaks=seq(1999,2020,1))

ggplot(ccfdfHERR)+theme_bw()+
  geom_point(aes(x=year, y=tot_proportion))+
  geom_line(aes(x=year, y=tot_proportion))+xlab("Year")+ylab("prop nmfs")+
  scale_x_continuous(breaks=seq(1999,2020,1))


    #test for stationarity
    #- KPSS test: non-stationary when p<0.05
    #- Duckey-Fuller test: non-stationary when p>0.05
adf.test(ccfdfHERR$tot_proportion) #NMFS data  -->result: stationary
adf.test(ccfdfHERR$proportion) #TERN data  -->result: NOT stationary
kpss.test(ccfdfHERR$tot_proportion, null="Trend") # NMFS data  -->result: stationary
kpss.test(ccfdfHERR$proportion, null="Trend") # TERN data  -->result: stationary

    #test how many differences is necessary to reach stationarity 
ndiffs(ccfdfHERR$tot_proportion) #--> claims it only needs 0
ndiffs(ccfdfHERR$proportion) #--> claims it only needs 0


    #try to force stationarity by differencing 
diffHERRtern<-diff(ccfdfHERR$proportion, differences=1)
diffHERRnmfs<-diff(ccfdfHERR$tot_proportion, differences=1)


    #test autocorrelation and re test for stationarity for differenced data
acf(diffHERRtern) #--> this makes it look good for stationarity because it quickly falls to or around zero after the first (0) lag
acf(diffHERRnmfs) #---> this also looks good for stationarity 
adf.test(diffHERRtern) #--> result: non stationary
adf.test(diffHERRnmfs) # result: non stationary
kpss.test(diffHERRtern, null="Trend") #--> result: stationary 
kpss.test(diffHERRnmfs, null="Trend") # result: stationary
    #TO DO: understand differences between acf and kpss to understand why they disagree


    #perform ccf for proportion on differenced data
ccfvaluesHERRdiffprop<-ccf(diffHERRnmfs, diffHERRtern, 10, correlation = TRUE, pl = TRUE)
ccfvaluesHERRdiffprop
ccfvaluesHERRdiffpropcorrplot<-lag2.plot(diffHERRnmfs, diffHERRtern, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this
lag2.plot(diffHERRtern, diffHERRnmfs, 10) #switching the X and Y appears to show positive lags within the context of the previous which makes sense 

    #perform ccf for proportion on NON differenced data
ccfvaluesHERRdiffprop<-ccf(ccfdfHERR$tot_proportion,ccfdfHERR$proportion, 10, correlation = TRUE, pl = TRUE)
ccfvaluesHERRdiffprop
    #side note TO DO: test of how corrected herring tracks (hypothesis is for strong positive correlation at lag0)
    #another side note TO DO: make a column that is non-adjusted fish minus adjusted fish abundance and test if correlation is better



  # HAKE relative abundance #
  #------------------------#
ccfdfHAKE<-ccfdf %>% 
  filter(prey %in% "HAKE")

    #visualize both time series raw
ggplot(ccfdfHAKE)+theme_bw()+
  geom_point(aes(x=year, y=abundance))+
  geom_line(aes(x=year, y=abundance))+xlab("Year")+ylab("rel abundance terns")+
  scale_x_continuous(breaks=seq(1999,2020,1))

ggplot(ccfdfHAKE)+theme_bw()+
  geom_point(aes(x=year, y=tot_rel_abundance))+
  geom_line(aes(x=year, y=tot_rel_abundance))+xlab("Year")+ylab("rel abundance nmfs")+
  scale_x_continuous(breaks=seq(1999,2020,1))


    #test for stationarity
    #- KPSS test: non-stationary when p<0.05
    #- Duckey-Fuller test: non-stationary when p>0.05
adf.test(ccfdfHAKE$tot_rel_abundance) #NMFS data  --> not stationary
adf.test(ccfdfHAKE$abundance) #TERN data --> not stationary but almost lol (0.07)
kpss.test(ccfdfHAKE$tot_rel_abundance, null="Trend") # NMFS data --> stationary
kpss.test(ccfdfHAKE$abundance, null="Trend") # TERN data --> not stationary

    #test how many differences is necessary to reach stationarity 
ndiffs(ccfdfHAKE$tot_re_abundance) #--> claims it only needs 0
ndiffs(ccfdfHAKE$abundance) #--> claims it only needs 1


    #try to force stationarity by differencing 
diffHAKEtern<-diff(ccfdfHAKE$abundance, differences=1)
diffHAKEnmfs<-diff(ccfdfHAKE$tot_rel_abundance, differences=1)


    #test autocorrelation and re test for stationarity for differenced data
acf(diffHAKEtern) #--> this makes it look good for stationarity because it quickly falls to or around zero after the first (0) lag
acf(diffHAKEnmfs) #---> this also looks good for stationarity 
adf.test(diffHAKEtern) #--> result: stationary
adf.test(diffHAKEnmfs) # result: non stationary
kpss.test(diffHAKEtern, null="Trend") #--> result: stationary
kpss.test(diffHAKEnmfs, null="Trend") # result: stationary

    #perform ccf for relative abundance
ccfvaluesHAKEdiff<-ccf(diffHAKEnmfs,diffHAKEtern, 20, correlation = TRUE, pl = TRUE)
ccfvaluesHAKEdiff
ccfvaluesHAKEdiffcorrplot<-lag2.plot(diffHAKEnmfs,diffHAKEtern, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this

    #perform ccf for relative abundance on non-differenced data
ccfvaluesHAKE<-ccf(ccfdfHERR$tot_rel_abundance, ccfdfHERR$abundance, 10, correlation = TRUE, pl = TRUE)
ccfvaluesHAKE

  #BUTTERFISH relative abundance#
  #-----------------------------#
ccfdfBUTT<-ccfdf %>% 
  filter(prey %in% "BUTTERFISH")

    #visualize both time series raw
ggplot(ccfdfBUTT)+theme_bw()+
  geom_point(aes(x=year, y=abundance))+
  geom_line(aes(x=year, y=abundance))+xlab("Year")+ylab("rel abundance terns")
ggplot(ccfdfBUTT)+theme_bw()+
  geom_point(aes(x=year, y=tot_rel_abundance))+
  geom_line(aes(x=year, y=tot_rel_abundance))+xlab("Year")+ylab("rel abundance nmfs")

    #test for stationarity
    #- KPSS test: non-stationary when p<0.05
    #- Duckey-Fuller test: non-stationary when p>0.05
adf.test(ccfdfBUTT$tot_rel_abundance) #NMFS data  --> not stationary
adf.test(ccfdfBUTT$abundance) #TERN data --> not stationary
kpss.test(ccfdfBUTT$tot_rel_abundance) # NMFS data --> stationary
kpss.test(ccfdfBUTT$abundance) # TERN data --> stationary

    #test how many differences is necessary to reach stationarity 
ndiffs(ccfdfBUTT$tot_re_abundance) #--> claims it only needs 0
ndiffs(ccfdfBUTT$abundance) #--> claims it only needs 0
    # so we are not gonna difference for this one
    
    #create vectors for ccf 
BUTTtern<-ccfdfBUTT$abundance
BUTTnmfs<-ccfdfBUTT$tot_rel_abundance

    #test autocorrelation 
acf(BUTTtern) #--> no evidence of autocorr
acf(BUTTnmfs) #---> no evidence of autocorr

    #perform ccf for relative abundance
ccfvaluesBUTTd<-ccf(BUTTnmfs,BUTTtern, 20, correlation = TRUE, pl = TRUE)
ccfvaluesBUTT
ccfvaluesBUTTcorrplot<-lag2.plot(BUTTnmfs,BUTTtern, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this

  #MACKEREL relative abundance#
  #---------------------------#
ccfdfMACK<-ccfdf %>% 
  filter(prey %in% "MACKEREL")

   #visualize both time series raw
ggplot(ccfdfMACK)+theme_bw()+
  geom_point(aes(x=year, y=abundance))+
  geom_line(aes(x=year, y=abundance))+xlab("Year")+ylab("rel abundance terns")
ggplot(ccfdfMACK)+theme_bw()+
  geom_point(aes(x=year, y=tot_rel_abundance))+
  geom_line(aes(x=year, y=tot_rel_abundance))+xlab("Year")+ylab("rel abundance nmfs")

    #test for stationarity
    #- KPSS test: non-stationary when p<0.05
    #- Dickey-Fuller test: non-stationary when p>0.05
adf.test(ccfdfMACK$tot_rel_abundance) #NMFS data --> non stationary 
adf.test(ccfdfMACK$abundance) #TERN data  -->non stationary
kpss.test(ccfdfMACK$tot_rel_abundance, null="Trend") # NMFS data --> stationary
kpss.test(ccfdfMACK$abundance, null="Trend") # TERN data --> stationary

    #create vectors for ccf
MACKtern<-ccfdfMACK$abundance
MACKnmfs<-ccfdfMACK$tot_rel_abundance
    
    #test autocorrelationand
acf(MACKtern) #--> not sure how to interpret this
acf(MACKnmfs) #---> honestly also not really sure how to interpret this right now

    #perform ccf for relative abundance
ccfvaluesMACK<-ccf(MACKnmfs,MACKtern, 20, correlation = TRUE, pl = TRUE)
ccfvaluesMACK #really unsure how to interpret this

ccfvaluesMACKcorrplot<-lag2.plot(MACKnmfs,MACKtern, 10) #seems like this is only showing negative lags, TO DO: figure out how exactly to interpret this








# LINEAR REGRESSION for TERN DIET and NMFS DATA -------------
#-- regression analysis performed only for lags identified as significant in the above cross correlation analysis
    #-mackerel: +6, +7, and +8
    #-butterfish: +4
    #-hake: not technically any that are significant but the highest are at +3, +4, +7
    #-herring: +1

#-----------------------#
#1 YEAR LAGGED ABUNDANCE#
#-----------------------#

  #set up a million dfs because i am annoying as hell 

tprovsub<-tprov %>%
  mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","hake","butterfish","sandlance","mackerel","pollock","unknown fish"),
                                             collapse="|"), prey, ignore.case=T), prey, "other")) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

TERNdf<-tprovsub %>% 
  filter(!preysub %in% c("other","unknown fish")) %>% 
  filter(! year %in% c(2003, 2004, 2007, 2010,2012,2014,2016,2020))
TERNdf[,2]=toupper(TERNdf[,2]) #make prey uppercase

  #GOM fall and spring
NMFSgomFS<-NMFSdfGOM %>% 
  filter(! year %in% c(2003, 2004, 2007, 2010,2012,2014,2016,2020))
dfgomFS<-full_join(NMFSgomFS, TERNdf) #automatically sorted by prey and year but make sure this is always the case
#dfgomFS[is.na(dfgomFS)]=0 #turn all NA into zeros (necessary for ccf function and also logical )

  #GOM fall
NMFSgomF<-NMFSdfGOMfall %>% 
  filter(! year %in% c(2003, 2004, 2007, 2010,2012,2014,2016,2020)) 
dfgomF<-full_join(NMFSgomF, TERNdf) #automatically sorted by prey and year but make sure this is always the case
#dfgomF[is.na(dfgomF)]=0 #turn all NA into zeros (necessary for ccf function and also logical )

  #GOM spring
NMFSgomS<-NMFSdfGOMspri %>% 
  filter(! year %in% c(2003, 2004, 2007, 2010,2012,2014,2016,2020)) 
dfgomS<-full_join(NMFSgomS, TERNdf) #automatically sorted by prey and year but make sure this is always the case
#dfgomS[is.na(dfgomS)]=0 #turn all NA into zeros (necessary for ccf function and also logical )

  #50km fall and spring
NMFS50kmFS<-NMFSdf50km %>% 
  filter(! year %in% c(2003, 2004, 2007, 2010,2012,2014,2016,2020)) 
df50kmFS<-full_join(NMFS50kmFS, TERNdf) #automatically sorted by prey and year but make sure this is always the case
#df50kmFS[is.na(df50kmFS)]=0 #turn all NA into zeros (necessary for ccf function and also logical )

  #50km fall
NMFS50kmF<-NMFSdf50kmfall %>% 
  filter(! year %in% c(2003, 2004, 2007, 2010,2012,2014,2016,2020)) 
df50kmF<-full_join(NMFS50kmF, TERNdf) #automatically sorted by prey and year but make sure this is always the case
#df50kmF[is.na(df50kmF)]=0 #turn all NA into zeros (necessary for ccf function and also logical )

  #50km spring
NMFS50kmS<-NMFSdf50kmspri %>% 
  filter(! year %in% c(2003, 2004, 2007, 2010,2012,2014,2016,2020)) 
df50kmS<-full_join(NMFS50kmS, TERNdf) #automatically sorted by prey and year but make sure this is always the case
#df50kmS[is.na(df50kmS)]=0 #turn all NA into zeros (necessary for ccf function and also logical )


  #HERRING GOM Fall+SPRING#
  #----------------------#
dfgomFSHERR<-dfgomFS %>% 
  filter(prey %in% "HERRING")

HERRlag1gomFS<-dfgomFSHERR %>% 
  mutate(lagabundance=lag(dfgomFSHERR$abundance, k=1))

  

  #HERRING GOM Fall
  #----------------------#

dfgomFHERR<-dfgomF %>% 
  filter(prey %in% "HERRING")

HERRlag1gomF<-dfgomFHERR %>% 
  mutate(lagabundance=lag(dfgomFHERR$abundance, k=1))



  #HERRING GOM SPRING
  #-----------------#

dfgomSHERR<-dfgomS %>% 
  filter(prey %in% "HERRING")

HERRlag1gomS<-dfgomSHERR %>% 
  mutate(lagabundance=lag(dfgomSHERR$abundance, k=1))







# old code for NHSG 2020 symposium ----------------------------------------------------------------


#===========================================================#
## LINEAR LAGGED REGGRESSION FOR NHSG 2020 ANNUAL SYMPOSIUM ##
#===========================================================#

#merge terns and nmfs herring

ternherr<-tprov %>% 
  filter(prey %in% c("herring"))
ternherr[,2]=toupper(ternherr[,2]) #make prey uppercase

NMFSherr<-NMFSdfGOM %>% 
  filter(prey %in% c("HERRING"))
NMFSherr<-NMFSdf50km %>% 
  filter(prey%in%c("HERRING"))

dat<-full_join(NMFSherr,ternherr)

setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
setwd("/Users/aliyacaldwell/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")

df<-read.csv("HerrLaggedData.csv") 
df2<-dfGOM %>% 
  filter(!year %in% c(2011,2014)) #decide whether to leave out "small" ss yrs

# GULF OF MAINE #

  #2 year lag GOM adjusted abundance
ggplot(df)+theme_bw()+
  geom_point(aes(x=lag2abundance,y=adjusted_rel_abundance_GOM),size=2)+
  #geom_text(aes(label=year, x=lag2abundance, y=adjusted_rel_abundance_GOM), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("GOM rel. abundance (fish/trawl effort)")+xlab("Tern diet rel. abudance 2yr lag (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lag2abundance,y=adjusted_rel_abundance_GOM), alpha=0.0, linetype="dashed", color="black", size=0.5)+
  theme(axis.title.x = element_text(size = 16), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14), axis.text.y=element_text(size=12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(label="spearmans: rho= 0.588, p= 0.018", aes(x=0.0025, y=800), face="plain")+
  geom_text(label="R?=0.253, p=0.047", aes(x=0.00155, y=770), face="plain")

  
spearman<-cor.test(df$lag2abundance, df$adjusted_rel_abundance_GOM,method="spearman")
head(spearman)
mymod<-lm(df$lag2abundance~df$adjusted_rel_abundance_GOM)
summary(mymod)


  #2 year lag GOM non adjusted abundance
ggplot(df)+theme_bw()+
  geom_point(aes(x=lag2abundance,y=tot_rel_abundance_GOM),size=2)+
  #geom_text(aes(label=year, x=lag2abundance, y=tot_rel_abundance_GOM), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("GOM rel. abundance (fish/trawl effort)")+xlab("Tern diet rel. abudance 2yr lag (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lag2abundance,y=tot_rel_abundance_GOM), alpha=0.0, linetype="dashed", color="black", size=0.5)+
  theme(axis.title.x = element_text(size = 16), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14), axis.text.y=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(label="spearmans: rho= 0.579, p= 0.021", aes(x=0.0025, y=950), face="plain")+
  geom_text(label="R?=0.236, p=0.056", aes(x=0.00155, y=910), face="plain")


spearman<-cor.test(df$lag2abundance, df$tot_rel_abundance_GOM,method="spearman")
head(spearman)
mymod<-lm(df$lag2abundance~df$tot_rel_abundance_GOM)
summary(mymod)

  #same year regression

ggplot(df)+theme_bw()+
  geom_point(aes(x=abundance_tern,y=catchable_rel_abundance_GOM),size=2)+
  ylab("Catchable rel. abundance (fish/trawl effort)")+xlab("Tern diet rel. abudance (fish/nest min)")+
  geom_smooth(method="lm",aes(x=abundance_tern,y=catchable_rel_abundance_GOM), alpha=0.0, linetype="dashed", color="black", size=0.5)+
  theme(axis.title.x = element_text(size = 16), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14), axis.text.y=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(label="spearmans: rho= 0.127, p= 0.615", aes(x=0.0025, y=200), face="plain")+
  geom_text(label="R?=0.190, p=0.070", aes(x=0.00155, y=190), face="plain")

spearman<-cor.test(df$abundance_tern, df$catchable_rel_abundance_GOM,method="spearman")
head(spearman)
mymod<-lm(df$abundance_tern~df$catchable_rel_abundance_GOM)
summary(mymod)

# 50KM BUFFER #

  #2 year lag GOM adjusted abundance
ggplot(df)+theme_bw()+
  geom_point(aes(x=lag2abundance,y=adjusted_rel_abundance_buff),size=2)+
  #geom_text(aes(label=year, x=lag2abundance, y=adjusted_rel_abundance_buff), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("50km buffer rel. abundance (fish/trawl effort)")+xlab("Tern diet rel. abudance 2yr lag (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lag2abundance,y=adjusted_rel_abundance_buff), alpha=0.0, linetype="dashed", color="black", size=0.5)+
  theme(axis.title.x = element_text(size = 16), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14), axis.text.y=element_text(size=12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(label="spearmans: rho= 0.321, p= 0.226", aes(x=0.0025, y=3000), face="plain")+
  geom_text(label="R?=0.016, p=0.646", aes(x=0.00155, y=2840), face="plain")
 
spearman<-cor.test(df$lag2abundance, df$adjusted_rel_abundance_buff,method="spearman")
head(spearman)
mymod<-lm(df$lag2abundance~df$adjusted_rel_abundance_buff)
summary(mymod)

  #2 year lag GOM non adjusted abundance
ggplot(df)+theme_bw()+
  geom_point(aes(x=lag2abundance,y=tot_rel_abundance_buff),size=2)+
  #geom_text(aes(label=year, x=lag2abundance, y=tot_rel_abundance_buff), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("50km buffer rel. abundance (fish/trawl effort)")+xlab("Tern diet rel. abudance 2yr lag (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lag2abundance,y=tot_rel_abundance_buff), alpha=0.0, linetype="dashed", color="black", size=0.5)+
  theme(axis.title.x = element_text(size = 16), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14), axis.text.y=element_text(size=12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(label="spearmans: rho= 0.321, p= 0.226", aes(x=0.0025, y=3000), face="plain")+
  geom_text(label="R?=0.016, p=0.646", aes(x=0.00155, y=2840), face="plain")

spearman<-cor.test(df$lag2abundance, df$tot_rel_abundance_buff,method="spearman")
head(spearman)
mymod<-lm(df$lag2abundance~df$tot_rel_abundance_buff)
summary(mymod)







  #### ALL OF THIS WITH THE LAG FUNCTION IS MESSED UP

  #whole GOM fall+ spring on adjusted abundance for HERRING#
  #--------------------------------------------------------#

terndat<-tprov %>% 
  filter(prey %in% c("herring")) 
terndat[,2]=toupper(terndat[,2])
  
nmfsdatGOM<-NMFSdfGOM %>% 
  filter(prey %in% c("HERRING"))

GOMherring<-full_join(terndat,nmfsdatGOM)  


    #1 year lag
GOMherringlag1.1<-GOMherring %>% 
  mutate(lagabundance=lag(GOMherring$abundance, k=1)) 

GOMherringlag1<-GOMherringlag1.1 %>% 
  filter(!year %in% c(2003, 2004, 2007, 2010,2012,2014,2016,2020))


ggplot(GOMherringlag1)+theme_bw()+
  geom_point(aes(x=lagabundance,y=adjusted_rel_abundance))+
  geom_text(aes(label=year, x=lagabundance, y=adjusted_rel_abundance), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("NMFS relative abundance (fish/trawl effort)")+xlab("1yr lag relative abundance (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lagabundance,y=adjusted_rel_abundance), alpha=0.3, linetype="dashed", color="black", size=0.5)


  #2 year lag
GOMherringlag2.1<-GOMherringlag1.1 %>% 
  mutate(lag2abundance=lag(GOMherringlag1.1$lagabundance, k=1)) 

GOMherringlag2<-GOMherringlag2.1 %>% 
  filter(!year %in% c(2003, 2004, 2007, 2010,2012,2014,2016,2020))

kendall<-cor.test(GOMherringlag2$lag2abundance, GOMherringlag2$adjusted_rel_abundance,method="kendall")
head(kendall)
spearman<-cor.test(GOMherringlag2$lag2abundance, GOMherringlag2$adjusted_rel_abundance,method="spearman")
head(spearman)


ggplot(GOMherringlag2)+theme_bw()+
  geom_point(aes(x=lag2abundance,y=adjusted_rel_abundance))+
  geom_text(aes(label=year, x=lag2abundance, y=adjusted_rel_abundance), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("NMFS relative abundance (fish/trawl effort")+xlab("2yr lag relative abundance (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lag2abundance,y=adjusted_rel_abundance), alpha=0.3, linetype="dashed", color="black", size=0.5)

mymod<-lm(GOMherringlag2$lag2abundance~GOMherringlag2$adjusted_rel_abundance)
summary(mymod)

  #3 year lag
GOMherringlag3<-GOMherringlag2 %>% 
  mutate(lag3abundance=lag(GOMherringlag2$lag2abundance, k=1))

kendall<-cor.test(GOMherringlag3$lag3abundance, GOMherringlag3$adjusted_rel_abundance,method="kendall")
head(kendall)
spearman<-cor.test(GOMherringlag3$lag3abundance, GOMherringlag3$adjusted_rel_abundance,method="spearman")
head(spearman)

ggplot(GOMherringlag3)+theme_bw()+
  geom_point(aes(x=lag3abundance,y=adjusted_rel_abundance))+
  geom_text(aes(label=year, x=lag3abundance, y=adjusted_rel_abundance), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("NMFS relative abundance (fish/trawl effort")+xlab("3yr lag relative abundance (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lag3abundance,y=adjusted_rel_abundance), alpha=0.3, linetype="dashed", color="black", size=0.5)

  #50km fall+ spring on adjusted abundance for HERRING#
  #---------------------------------------------------#

terndat<-tprov %>% 
  filter(prey %in% c("herring")) 
terndat[,2]=toupper(terndat[,2])

nmfsdat50<-NMFSdf50km %>% 
  filter(prey %in% c("HERRING"))

herring50<-full_join(terndat,nmfsdat50)  


#1 year lag
km50herringlag1<-herring50 %>% 
  mutate(lagabundance=lag(herring50$abundance, k=1)) %>% 
  filter(!year %in% c(2003, 2004, 2010, 2012, 2016))


ggplot(km50herringlag1)+theme_bw()+
  geom_point(aes(x=lagabundance,y=adjusted_rel_abundance))+
  geom_text(aes(label=year, x=lagabundance, y=adjusted_rel_abundance), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("NMFS relative abundance (fish/trawl effort")+xlab("1yr lag relative abundance (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lagabundance,y=adjusted_rel_abundance), alpha=0.3, linetype="dashed", color="black", size=0.5)


#2 year lag
km50herringlag2<-km50herringlag1 %>% 
  mutate(lag2abundance=lag(km50herringlag1$lagabundance, k=1))

kendall<-cor.test(km50herringlag22$lag2abundance, km50herringlag2$adjusted_rel_abundance,method="kendall")
head(kendall)
spearman<-cor.test(km50herringlag2$lag2abundance, km50herringlag2$adjusted_rel_abundance,method="spearman")
head(spearman)


ggplot(km50herringlag2)+theme_bw()+
  geom_point(aes(x=lag2abundance,y=adjusted_rel_abundance))+
  geom_text(aes(label=year, x=lag2abundance, y=adjusted_rel_abundance), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("NMFS relative abundance (fish/trawl effort")+xlab("2yr lag relative abundance (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lag2abundance,y=adjusted_rel_abundance), alpha=0.3, linetype="dashed", color="black", size=0.5)


#3 year lag
km50herringlag3<-km50herringlag2 %>% 
  mutate(lag3abundance=lag(km50herringlag2$lag2abundance, k=1))

kendall<-cor.test(km50herringlag3$lag3abundance, km50herringlag3$adjusted_rel_abundance,method="kendall")
head(kendall)
spearman<-cor.test(km50herringlag3$lag3abundance, km50herringlag3$adjusted_rel_abundance,method="spearman")
head(spearman)

ggplot(km50herringlag3)+theme_bw()+
  geom_point(aes(x=lag3abundance,y=adjusted_rel_abundance))+
  geom_text(aes(label=year, x=lag3abundance, y=adjusted_rel_abundance), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("NMFS relative abundance (fish/trawl effort")+xlab("3yr lag relative abundance (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lag3abundance,y=adjusted_rel_abundance), alpha=0.3, linetype="dashed", color="black", size=0.5)


  #whole GOM fall+ spring on adjusted abundance for HAKE#
  #--------------------------------------------------------#

terndat<-tprov %>% 
  filter(prey %in% c("hake")) 
terndat[,2]=toupper(terndat[,2])

nmfsdatGOM<-NMFSdfGOM %>% 
  filter(prey %in% c("HAKE"))

GOMhake<-full_join(terndat,nmfsdatGOM)  

  
   #1 year lag
GOMhakelag1<-GOMhake %>% 
  mutate(lagabundance=lag(GOMhake$abundance, k=1))


ggplot(GOMhakelag1)+theme_bw()+
  geom_point(aes(x=lagabundance,y=adjusted_rel_abundance))+
  geom_text(aes(label=year, x=lagabundance, y=adjusted_rel_abundance), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("NMFS relative abundance (fish/trawl effort")+xlab("1yr lag relative abundance (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lagabundance,y=adjusted_rel_abundance), alpha=0.3, linetype="dashed", color="black", size=0.5)


    #2 year lag
GOMhakelag2<-GOMhakelag1 %>% 
  mutate(lag2abundance=lag(GOMhakelag1$lagabundance, k=1))

kendall<-cor.test(GOMhakelag2$lag2abundance, GOMhakelag2$adjusted_rel_abundance,method="kendall")
head(kendall)
spearman<-cor.test(GOMhakelag2$lag2abundance, GOMhakelag2$adjusted_rel_abundance,method="spearman")
head(spearman)


ggplot(GOMhakelag2)+theme_bw()+
  geom_point(aes(x=lag2abundance,y=adjusted_rel_abundance))+
  geom_text(aes(label=year, x=lag2abundance, y=adjusted_rel_abundance), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("NMFS relative abundance (fish/trawl effort")+xlab("2yr lag relative abundance (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lag2abundance,y=adjusted_rel_abundance), alpha=0.3, linetype="dashed", color="black", size=0.5)


  #50km fall+ spring on adjusted abundance for HAKE#
  #------------------------------------------------#

terndat<-tprov %>% 
  filter(prey %in% c("hake")) 
terndat[,2]=toupper(terndat[,2])

nmfsdat50<-NMFSdf50km %>% 
  filter(prey %in% c("HAKE"))

hake50<-full_join(terndat,nmfsdat50)  


#1 year lag
km50hakelag1<-hake50 %>% 
  mutate(lagabundance=lag(hake50$abundance, k=1)) %>% 
  filter(!year %in% c(2003, 2004, 2010, 2012, 2016))


ggplot(km50hakelag1)+theme_bw()+
  geom_point(aes(x=lagabundance,y=adjusted_rel_abundance))+
  geom_text(aes(label=year, x=lagabundance, y=adjusted_rel_abundance), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("NMFS relative abundance (fish/trawl effort")+xlab("1yr lag relative abundance (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lagabundance,y=adjusted_rel_abundance), alpha=0.3, linetype="dashed", color="black", size=0.5)


#2 year lag
km50hakelag2<-km50hakelag1 %>% 
  mutate(lag2abundance=lag(km50hakelag1$lagabundance, k=1))

kendall<-cor.test(km50hakelag22$lag2abundance, km50hakelag2$adjusted_rel_abundance,method="kendall")
head(kendall)
spearman<-cor.test(km50hakelag2$lag2abundance, km50hakelag2$adjusted_rel_abundance,method="spearman")
head(spearman)


ggplot(km50hakelag2)+theme_bw()+
  geom_point(aes(x=lag2abundance,y=adjusted_rel_abundance))+
  geom_text(aes(label=year, x=lag2abundance, y=adjusted_rel_abundance), nudge_x=0.0003, nudge_y=0, size=3)+
  ylab("NMFS relative abundance (fish/trawl effort")+xlab("2yr lag relative abundance (fish/nest min)")+
  geom_smooth(method="lm",aes(x=lag2abundance,y=adjusted_rel_abundance), alpha=0.3, linetype="dashed", color="black", size=0.5)




# fish species making up n% of diet ---------------------------------------

tprovprey<-tprov %>% 
  group_by(prey) %>% 
  summarise('tot_n'=sum(n)) 

tprovpercent<-tprovprey %>% 
  mutate('percent_tot_diet'=(100*(tot_n/sum(tot_n))))





# data vis: proportions of fish in TERN DATA vs NMFS DATA -----------------------------------

#all prey categories all years tern data
#--------------------------------------#

ggplot(tprov)+
  geom_col(aes(x=year, y=proportion, fill=prey))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))

#main five prey categories, good sample size years tern data
#----------------------------------------------------------#
tprovsub<-tprov %>%
  mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","lumpfish","hake","butterfish","sandlance","mackerel","bluefish","cunner","pollock","mummichog","stickleback"),
                                             collapse="|"), prey, ignore.case=T), prey, "other/unknown")) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

tprovsub[,9]=toupper(tprovsub[,9]) #make prey uppercase


speciesprop<-ggplot(tprovsub)+
  geom_col(aes(x=year, y=proportion, fill=preysub))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_poke(pokemon="surskit", spread=12)

# main five prey categories, good sample size years NMFS GOM fall and spring
# -------------------------------------------------------------------------#

ggplot(NMFSdfGOM)+
  geom_col(aes(x=year, y=tot_proportion, fill=prey))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("Year")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_poke(pokemon="surskit", spread=8)







# data vis: abundance of fish in TERN DATA vs NMFS DATA -----------------------------------

# all species all years TERNS
#---------------------------#
ggplot(tprov)+theme_bw()+
  geom_point(aes(x=year, y=abundance, color=prey))+ylim(0,0.015)+
  geom_line(aes(x=year, y=abundance, color=prey))+xlab("Year")+ylab("relative abundance (fish/nest min)")

# top 5 species, good sample size years TERNS
#-------------------------------------------#
tprovsub<-tprov %>%
  mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","hake","butterfish","sandlance","mackerel","unknown fish"),
                                             collapse="|"), prey, ignore.case=T), prey, "other")) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

ggplot(tprovsub)+theme_bw()+
  geom_point(aes(x=year, y=abundance, color=preysub))+ylim(0,0.015)+
  geom_line(aes(x=year, y=abundance, color=preysub))+xlab("Year")+ylab("relative abundance (fish/nest min)")+
  scale_color_poke(pokemon="surskit", spread=8)

#total relative abundance, good sample years (i.e. "total relative landings") TERNS
#----------------------------------------------------------------------------------#
tprovyrs<-tprov %>%
  group_by(year) %>%
  summarise(totalabundance=sum(abundance)) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

ggplot(tprovyrs)+theme_bw()+
  geom_point(aes(x=year, y=totalabundance))+ylim(0,0.05)+
  geom_line(aes(x=year, y=totalabundance))+xlab("Year")+ylab("total abundance (fish/nest min)")

#landings plotted with proportions TERNS
#---------------------------------------#
ggplot()+
  geom_col(data=tprovsub, aes(x=year, y=proportion, fill=preysub))+theme_bw()+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_poke(pokemon="surskit", spread=8)+
  geom_point(data=tprovyrs, aes(x=year, y=totalabundance*20),size=2)+
  geom_line(data=tprovyrs, aes(x=year, y=totalabundance*20), size=1)+
  scale_x_continuous(expand=c(0,0.01),name="Year")+
  scale_y_continuous(expand=c(0,0.001),
                     name="Proportion in diet", sec.axis=sec_axis(~./20, name="Total abundance (fish/nest min)"))
   #TO DO: make the line cut off for years without samples?

# relative abundance of certain species throughout the season for each year #
#---------------------------------------------------------------------------#
  #--using herring as an example
herringtprov<-dailyweeklytprov %>%
  group_by(year, week) %>%
  filter(prey %in% c("herring")) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

  #good sample years multiple panes
ggplot(herringtprov)+theme_bw()+
  geom_point(aes(x=day, y=abundance, color=as.character(year)))+
  geom_line(aes(x=day, y=abundance, color=as.character(year)))+
  facet_wrap(~year)

  #good sample years multiple panes
ggplot(herringtprov)+theme_bw()+
  geom_point(aes(x=day, y=abundance))


# relative abundance of certain species throughout the season across years #
#--------------------------------------------------------------------------#
ggplot(herringtprov)+theme_bw()+
  geom_point(aes(x=day, y=abundance))

#top 5 species, good sample years NMFS GOM Fall plus Spring
#----------------------------------------------------------------#

ggplot(NMFSdfGOM)+theme_bw()+
  geom_point(aes(x=year, y=tot_rel_abundance, color=prey))+
  geom_line(aes(x=year, y=tot_rel_abundance, color=prey))+xlab("Year")+ylab("relative abundance (fish/trawl effort)")


#total relative abundance, good sample years NMFS GOM Fall plus Spring
#--------------------------------------------------------------------------------#
  #using fall only
NMFSdfGOMtot<-NMFSdfGOM %>%
  group_by(year) %>%
  summarise(totalabundance=sum(tot_rel_abundance)) 

ggplot(NMFSdfGOMtot)+theme_bw()+
  geom_point(aes(x=year, y=totalabundance))+
  geom_line(aes(x=year, y=totalabundance))+xlab("Year")+ylab("total abundance (fish/trawl effort)")


#landings plotted with proportions NMFS GOM Fall+Spring
#-----------------------------------------------------#

ggplot()+
  geom_col(data=NMFSdfGOM, aes(x=year, y=tot_proportion, fill=prey))+theme_bw()+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_poke(pokemon="surskit", spread=8)+
  geom_point(data=NMFSdfGOMtot, aes(x=year, y=totalabundance/3500),size=2)+
  geom_line(data=NMFSdfGOMtot, aes(x=year, y=totalabundance/3500), size=1)+
  scale_x_continuous(expand=c(0,0.01),name="Year")+
  scale_y_continuous(expand=c(0,0.001),
                     name="Proportion in trawl", sec.axis=sec_axis(~.*3500, name="Total abundance (fish/trawl effort)"))



# VON BERT CURVES ------------------------------------------------------

#von Bertalanffy curves for the different species
vonB<-function(Li,k,t0,t) {
  l=Li*(1-exp(-1*k*(t-t0)))
}

#atlantic herring from Burbank et al. 2023
  #(https://www.mdpi.com/2410-3888/8/4/205)
  #this is using the values they found for fall spawners in the gulf of st lawrence
herrLi<-345.83 
herrk<-0.3
herrt0<- -1.1

herr_vonB<-vonB(herrLi,herrk,herrt0,seq(0,5))

herrDF<-data.frame(Length=herr_vonB,Age=seq(0,5))

par(mar=c(5,5,5,5))
plot(seq(0,5),herr_vonB,type="b")
abline(h=190,col="firebrick3") #Point of largest tern provision


ggplot(herrDF)+
  theme_classic()+
  geom_line(aes(Age,Length),lwd=1.5,color="grey30")+
  geom_point(aes(Age,Length),size=4)+
  geom_hline(yintercept=190, color = "red")+
  xlab("Age (yrs)")+ylab("Length (mm)")


#Silver Hake - from nate
hakeLi<-44.88
hakek<-0.354
haket0<-0.092

hake_vonB<-vonB(hakeLi,hakek,haket0,seq(0,20))

hakeDF<-data.frame(Length=hake_vonB,Age=seq(0,20))

par(mar=c(5,5,5,5))
plot(seq(0,20),hake_vonB,type="b")
abline(h=7.5,col="firebrick3") #Point of largest tern provision

ggplot(hakeDF)+
  geom_line(aes(Age,Length),lwd=1.5,color="grey30")+
  geom_point(aes(Age,Length),size=4)+
  xlab("Age (yrs)")+ylab("Length (cm)")

#Northern Sand Lance - from nate
NSLli<-249
NSLk<-0.26
NSLt0<--1.10

NSL_vonB<-vonB(NSLli,NSLk,NSLt0,seq(0,20))

NSLDF<-data.frame(Length=NSL_vonB,Age=seq(0,20),source="von Bertalanffy")

ggplot(NSLDF)+
  geom_line(aes(Age,Length),lwd=1.5,color="grey30")+
  geom_point(aes(Age,Length),size=4)+
  xlab("Age (yrs)")+ylab("Length (cm)")

    #Less than 1 year Sand Lance - from nate
NSL_y0<-data.frame(Age=numeric(length=11),Length=numeric(length=11),source="Logistic")
for (M in 0:10) {
  NSL_y0[M+1,1]<-M/12
  NSL_y0[M+1,2]<-(107.7/(1+exp((-0.71)*(M-5.17))))
}

ggplot(NSL_y0)+
  geom_point(aes(x=Age,y=Length))

    #All together now! - from nate
allNSL<-bind_rows(NSL_y0,NSLDF[2:20,])

ggplot(allNSL)+
  geom_point(aes(x=Age,y=Length,fill=source),shape=21,size=5,stroke=1.1)+
  scale_fill_brewer(palette="Set2",name="Source Equation")+
  theme(legend.position=c(0.135,0.875),legend.background=element_rect(fill=rgb(0,0,0,alpha=0)))

#What about the age data that I already have from the NMFS trawls?
# fallAges<-read_csv("NMFS Trawls/Fall/22560_UNION_FSCS_SVBIO.csv")
# springAges<-read_csv("NMFS Trawls/Spring/22561_UNION_FSCS_SVBIO.csv")
# 
# fallAges$season<-"Fall"
# springAges$season<-"Spring"
# springAges$Year<-as.numeric(substr(springAges$CRUISE6,1,4))
# fallAges$Year<-as.numeric(substr(fallAges$CRUISE6,1,4))
# 
# Tr<-rbind(fallT[,6:7],springT[,6:7])
# TrU<-Tr[!duplicated(Tr$SVSPP),]
# ages<-left_join(rbind(fallAges,springAges),TrU,by="SVSPP")
# 
# 
# 
# ages_seabirds<-filter(ages,grepl(paste(seabird_trawl,collapse="|"),
#                                  ages$LOGGED_SPECIES_NAME,ignore.case=T))%>%
#   dplyr::select(Year,CRUISE6,STRATUM,STATION,Species=LOGGED_SPECIES_NAME,SVSPP,season,LENGTH,AGE)%>%
#   arrange(Year,Species)
# 
# ages_seabirds$Family<-sapply(ages_seabirds$Species,spNMFSCats)
# 
# ages_seabirds<-ages_seabirds[!is.na(ages_seabirds$AGE),]
# ages_seabirds$season<-factor(ages_seabirds$season,levels=c("Spring","Fall"))
# 
# 
# ggplot(ages_seabirds)+
#   geom_point(aes(x=AGE,y=LENGTH,fill=season),shape=21,alpha=0.6)+
#   facet_wrap(~Family)+
#   scale_fill_brewer(palette = "Dark2",name="Trawl Season")+
#   ylab("Length (cm)")+xlab("Age (yrs)")+
#   guides(fill=guide_legend(override.aes = list(size=6)))
# 
# library(nlme)
# 
# nmfsVBs <- nlsList(LENGTH ~ Li*(1-exp(-1*k*(AGE-t0))) | Species,
#                    data=ages_seabirds,
#                    start=c(Li=249, k=0.26, t0=-1.10),
#                    na.action=na.omit)
# VBcoefs<-coef(nmfsVBs)
# VBcoefs$Species<-rownames(VBcoefs)
# 
# vonB_preds<-data.frame(Species=numeric(length=151),Age=numeric(length=151),Length=numeric(length=151))
# start=1
# 
# for (j in 1:9) {
#   end=start+max(subset(ages_seabirds,Species==VBcoefs[j,4])$AGE)
#   vonB_preds[start:end,1]<-as.character(VBcoefs[j,4])
#   vonB_preds[start:end,2]<-seq(0,max(subset(ages_seabirds,Species==VBcoefs[j,4])$AGE))
#   vonB_preds[start:end,3]<-vonB(VBcoefs[j,1],VBcoefs[j,2],VBcoefs[j,3],
#                                 seq(0,max(subset(ages_seabirds,Species==VBcoefs[j,4])$AGE)))
#   start=start+(end-start+1)
# }
# 
# ggplot()+
#   geom_hline(yintercept=7.5,lty=2)+
#   geom_point(data=ages_seabirds,aes(AGE,LENGTH,fill=Species),shape=21,size=2,alpha=0.2,show.legend=F)+
#   geom_line(data=vonB_preds,aes(Age,Length,color=Species),lwd=1.25,show.legend=F)+
#   #geom_point(data=vonB_preds,aes(Age,Length,fill=Species),shape=21,size=4,stroke=1.05,show.legend=F)+
#   facet_wrap(~Species)+
#   scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+
#   ylab("Length (cm)")+xlab("Age (yrs)")
# 
# ggplot()+
#   geom_hline(yintercept=7.5,lty=2)+
#   geom_jitter(data=ages_seabirds,aes(AGE,LENGTH,fill=Species),shape=21,size=2,alpha=0.3,width=0.2,show.legend=F)+
#   geom_line(data=vonB_preds,aes(Age,Length,color=Species),lwd=1.5,show.legend=F)+
#   #geom_point(data=vonB_preds,aes(Age,Length,fill=Species),shape=21,size=4,stroke=1.05,show.legend=F)+
#   scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+
#   ylab("Length (cm)")+xlab("Age (yrs)")
# 
# 
# 
# #Same but with Von B Grouped at the family level
# nmfsVB_fams <- nlsList(LENGTH ~ Li*(1-exp(-1*k*(AGE-t0))) | Family,
#                        data=ages_seabirds,
#                        start=c(Li=249, k=0.26, t0=-1.10),
#                        na.action=na.omit)
# VBcoefs_fams<-coef(nmfsVB_fams)
# VBcoefs_fams$Family<-rownames(VBcoefs_fams)
# 
# vonB_fam_preds<-data.frame(Family=numeric(length=102),Age=numeric(length=102),Length=numeric(length=102))
# start=1
# 
# for (j in 1:5) {
#   end=start+max(subset(ages_seabirds,Family==VBcoefs_fams[j,4])$AGE)
#   vonB_fam_preds[start:end,1]<-as.character(VBcoefs_fams[j,4])
#   vonB_fam_preds[start:end,2]<-seq(0,max(subset(ages_seabirds,Family==VBcoefs_fams[j,4])$AGE))
#   vonB_fam_preds[start:end,3]<-vonB(VBcoefs_fams[j,1],VBcoefs_fams[j,2],VBcoefs_fams[j,3],
#                                     seq(0,max(subset(ages_seabirds,Family==VBcoefs_fams[j,4])$AGE)))
#   start=start+(end-start+1)
# }
# 
# ggplot()+
#   geom_hline(yintercept=7.5,lty=2)+
#   geom_point(data=ages_seabirds,aes(AGE,LENGTH,fill=Family),shape=21,size=2,alpha=0.2,show.legend=F)+
#   geom_line(data=vonB_fam_preds,aes(Age,Length,color=Family),lwd=1.25,show.legend=F)+
#   #geom_point(data=vonB_fam_preds,aes(Age,Length,fill=Family),shape=21,size=4,stroke=1.05,show.legend=F)+
#   facet_wrap(~Family)+
#   scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+
#   ylab("Length (cm)")+xlab("Age (yrs)")
# 
# ggplot()+
#   geom_hline(yintercept=7.5,lty=2)+
#   geom_jitter(data=ages_seabirds,aes(AGE,LENGTH,fill=Family),shape=21,size=2,alpha=0.3,width=0.2,show.legend=F)+
#   geom_line(data=vonB_fam_preds,aes(Age,Length,color=Family),lwd=1.25,show.legend=F)+
#   #geom_point(data=vonB_fam_preds,aes(Age,Length,fill=Family),shape=21,size=4,stroke=1.05,show.legend=F)+
#   scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+
#   ylab("Length (cm)")+xlab("Age (yrs)")
# 
# 
# #Combination plot of them--facet family with species dots and lines
# vonB_preds$Family<-sapply(vonB_preds$Species,spNMFSCats)
# 
# ggplot()+
#   geom_hline(yintercept=7.5,lty=2)+
#   geom_jitter(data=ages_seabirds,aes(AGE,LENGTH,fill=Species),width=0.2,shape=21,size=2,alpha=0.2,show.legend=F)+
#   geom_line(data=vonB_preds,aes(Age,Length,color=Species),lwd=1.25,show.legend=F)+
#   #geom_point(data=vonB_preds,aes(Age,Length,fill=Species),shape=21,size=4,stroke=1.05,show.legend=F)+
#   facet_wrap(~Family)+
#   scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+
#   ylab("Length (cm)")+xlab("Age (yrs)")


# SIZE DISTRIBUTION: TERN vs NMFS -----------------------------------

# size distribution top 5 species from TERNS across all years
#------------------------------------------------------------#

dfcleansub<-dfclean %>%
  filter(prey %in% c("herring","hake","butterfish","sandlance","mackerel"))

  #multiple panes by species
ggplot(dfcleansub)+theme_bw()+
  geom_density(aes(x=size, color=prey, fill=prey),
               adjust=3,alpha=0.7,size=1)+
  facet_wrap(~prey)+
  scale_fill_poke(pokemon="surskit", spread=5)+
  scale_color_poke(pokemon="surskit", spread=5)+
  xlab("Length (mm)")

  #single pane
ggplot(dfcleansub)+theme_bw()+
  geom_density(aes(x=size, color=prey, fill=prey),
               adjust=3,alpha=0.3,size=1)+
  scale_fill_poke(pokemon="surskit", spread=5)+
  scale_color_poke(pokemon="surskit", spread=5)+
  xlab("Length (mm)")

# size distribution of species TERNS throughout years using HERRING as example
#----------------------------------------------------------------------------#
    #-just replace herring sub with whatever species sub you want
    #-need to use unsummarized dataset (dfclean)

herringsub<-dfclean %>%
  filter(! year %in% c(2003, 2004, 2010, 2012, 2006, 2017)) %>%  #2006 and 2017 have <15 herring samples
  filter(prey %in% "herring")
  #TO DO: decide how low a sample size needs to be to exclude the species/year combo from analysis

nb.cols<-16
mycolors <- colorRampPalette(brewer.pal(8, "BuGn"))(nb.cols)
   #TO DO: create a color palette for each family that matches with colors from other plots

  #multiple panes
ggplot(herringsub)+theme_bw()+
  geom_density(aes(x=as.numeric(size), color=as.character(year), fill=as.character(year)),
               adjust=3,alpha=0.7,size=1)+
  scale_fill_manual(values=mycolors,name="year")+
  scale_color_manual(values=mycolors,name="year")+
  xlab("Herring Length (mm)")+
  guides(color=guide_legend("year"))+
  facet_wrap(~year)

  #single pane
ggplot(herringsub)+theme_bw()+
  geom_density(aes(x=size, color=as.character(year), fill=as.character(year)),
               adjust=3,alpha=0.7,size=1.5)+
  scale_fill_manual(values=mycolors, name="year")+
  scale_color_manual(values=mycolors, name="year")+
  xlab("Herring Length (mm)")+
  guides(color=guide_legend("year"))


#mean and median size of the top 5 species across time
#-----------------------------------------------------#

tprovsubsize<-tprov %>% 
  filter(prey %in% c("herring","hake","butterfish","sandlance","mackerel"))
 
ggplot(tprovsubsize)+theme_bw()+
  geom_point(aes(x=year,y=meansize, color=prey))+
  geom_line(aes(x=year,y=meansize, color=prey))

  #herring
  #-------
tprovsubsizeherr<-tprov %>% 
  filter(prey %in% c("herring")) %>% 
  filter(minsize < 100) #get rid of infinity vals

ggplot(tprovsubsizeherr)+theme_bw()+
  geom_point(aes(x=year,y=meansize))+
  geom_line(aes(x=year,y=meansize))+
  geom_point(aes(x=year,y=minsize))+
  geom_line(aes(x=year,y=minsize), linetype="dashed")+
  geom_point(aes(x=year,y=maxsize))+
  geom_line(aes(x=year,y=maxsize), linetype="dashed")


  #hake
  #-------
tprovsubsizehake<-tprov %>% 
  filter(prey %in% c("hake"))

ggplot(tprovsubsizehake)+theme_bw()+
  geom_point(aes(x=year,y=meansize, color=prey))+
  geom_line(aes(x=year,y=meansize, color=prey))+
  geom_point(aes(x=year,y=mediansize, color=prey), pch=21)+
  geom_line(aes(x=year,y=mediansize, color=prey), linetype="dashed")
  

#TO DO:fit linear models to each


#IN PROCESS: top 5 species size, good sample years all years NMFS for catchable sizes 
#--------------------------------------------------------------------------------#

      #STOLE NATE's CODE HERE#
      # did not finish working through this --> ask Nate if he did this to as not reinvent wheel

library(readr)

fallTrawl_Sizes<-read_csv("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/NMFS Trawls/Fall/22561_UNION_FSCS_SVLEN.csv", 
                          col_types = cols(CATCHSEX = col_character(), ID = col_character(),
                                           CRUISE6 = col_character(), STATION = col_number(),
                                           STRATUM = col_number(), TOW = col_number()))

springTrawl_Sizes<-read_csv("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/NMFS Trawls/Spring/22561_UNION_FSCS_SVLEN.csv", 
                            col_types = cols(CATCHSEX = col_character(), ID = col_character(),
                                             CRUISE6 = col_character(), STATION = col_number(),
                                             STRATUM = col_number(), TOW = col_number()))

#Filtering it to the buffer trawl spots and the right species
bufferTrawl_Sizes<-merge(unique(dplyr::select(buffer_seabirds,CRUISE6,STRATUM,STATION,Year,season,LONG,LAT)),
                         rbind(fallTrawl_Sizes,springTrawl_Sizes),by=c("CRUISE6","STRATUM","STATION"))
bufferTrawl_Sizes<-bufferTrawl_Sizes%>%
  filter(grepl(paste(seabird_trawl,collapse="|"),LOGGED_SPECIES_NAME,ignore.case=T))%>%
  dplyr::select(Year,CRUISE6,STRATUM,TOW,STATION,Species=LOGGED_SPECIES_NAME,season,Sex=CATCHSEX,LENGTH,LONG,LAT)%>%
  arrange(Year,Species)


bufferTrawl_Sizes$Family<-sapply(bufferTrawl_Sizes$Species,spNMFSCats)

bufferTrawl_Sizes$season<-factor(bufferTrawl_Sizes$season,levels=c("SPRING","FALL"))

#Plotting the different sizes for the families
ggplot(bufferTrawl_Sizes)+
  geom_density(aes(x=LENGTH,fill=season),alpha=0.5)+
  #geom_errorbarh(aes(xmin=1,xmax=7.5,y=0.0025),lwd=1.25,height=0.005,color="firebrick3")+
  geom_point(data=data.frame(Family=c("BUTTERFISH","HAKE","HERRING","MACKEREL","POLLOCK","SAND LANCE"),
                             mean=c(5.67,4.65,5.82,5.57,7.73,7.21),
                             sd=2.12,1.92,2.13,1.93,2.29,2.76),aes(x=mean,y=0.1),size=2)+
  geom_errorbarh(data=data.frame(Family=c("BUTTERFISH","HAKE","HERRING","MACKEREL","POLLOCK","SAND LANCE"),
                                 mean=c(5.67,4.65,5.82,5.57,7.73,7.21),
                                 sd=2.12,1.92,2.13,1.93,2.29,2.76),aes(xmin=mean-sd,xmax=mean+sd,y=0.1),
                 height=0.005)+
  facet_wrap(~Family)+
  scale_y_continuous(expand=c(0,0.001))+
  scale_fill_brewer(palette = "Dark2",name="Trawl Season",labels=c("Spring","Fall"))+
  theme(legend.key.size=unit(1.25,"cm"),legend.text=element_text(size=25),legend.title=element_text(size=30))+
  ylab("Density")+xlab("Length (cm)")


#TO DO: top 5 species size, good sample years throughout years NMFS for catchable sizes
#------------------------------------------------------------------------------------#



# TEMPORAL DISTRIBUTION OF TERN DATA SAMPLING -----------------------------------

   #- uses unsummarized df (dfclean)

#distribution using number of observations#
#-----------------------------------------#

  #total observations across days across years
ggplot(dfclean)+theme_bw()+
  geom_density(aes(x=day),
               adjust=3,alpha=0.6)+xlab("day of year")

  #total observations (i.e. sampling effort) across days between years
ggplot(dfclean)+theme_bw()+
  geom_density(aes(x=day, color=as.character(year), fill=as.character(year)),
               adjust=3,alpha=0.6)+xlab("day of year")+
  facet_wrap(~year)

  #total observations of each species across all years
dfcleansub<-dfclean %>%
  filter(prey %in% c("herring","hake","butterfish","sandlance","mackerel"))

ggplot(dfcleansub)+theme_bw()+
  geom_density(aes(x=day, fill=prey, color=prey),
               adjust=3,alpha=0.4,size=1)+xlab("day of year")+
  scale_fill_poke(pokemon="surskit", spread=5)+
  scale_color_poke(pokemon="surskit", spread=5)

#distribution using weekly sampling effort
#-----------------------------------------#

  #total observations across days across all years single pane
ggplot(dailyweeklytprov)+theme_bw()+
  geom_point(aes(x=day, y=`Effort (nest mins)`, color=as.character(year)), na.rm=T)
  #total observations across days across all years multiple panes
ggplot(dailyweeklytprov)+theme_bw()+
  geom_point(aes(x=day, y=`Effort (nest mins)`, color=as.character(year)), na.rm=T)+
  geom_line(aes(x=day, y=`Effort (nest mins)`, color=as.character(year)), na.rm=T)+
  facet_wrap(~year)

  #plot number of samples and sampling effort together to compare
dailyweeklytprovsub<-dailyweeklytprov %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))
dfcleansub<-dfclean %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

ggplot()+theme_bw()+
  geom_density(data=dfcleansub,aes(x=day, color=as.character(year), fill=as.character(year)),
               adjust=3,alpha=0.3)+xlab("day of year")+
  geom_line(data=dailyweeklytprovsub, aes(x=day, y=(`Effort (nest mins)`/20000), color=as.character(year)), na.rm=T, size=1)+
  scale_y_continuous(expand=c(0,0.2),
                     name="sampling density", sec.axis=sec_axis(~.*20000, name="sampling effort (nest min)"))+
  facet_wrap(~year)





# PROPORTION of FISH SPECIES in BLIND vs VIDEO ----------------------------------

    #-- proportion of species in blind vs video

    #copied from sampling effort from earler:
effort<-dfclean %>%
  mutate(`sampling effort`=(watchtime*nestswatched))%>%
  group_by(method) %>%
  summarise(`Effort (nest mins)`=sum(`sampling effort`, na.rm=T))

methodfish<-dfclean %>%
  filter(! event %in% c("end","start")) %>% 
  filter(! prey %in% c("")) %>% 
  group_by(method, prey) %>%
  summarise(n=n(), meansize=mean(as.numeric(size),na.rm=T)) %>%
  mutate(prop=n/sum(n))

methodfisheffort<-merge(effort,methodfish) %>%
  mutate(`relative abundance (fish per nest min)`=(n/`Effort (nest mins)`))

methodtprov<-methodfisheffort %>%
  select(prey, method, meansize, prop, `relative abundance (fish per nest min)`) %>%
  rename(proportion=prop) %>%
  rename(size=meansize) %>%
  rename(abundance=`relative abundance (fish per nest min)`)

ggplot(methodtprov)+theme_bw()+
  geom_col(aes(x=method, y=proportion, fill=prey))+theme_bw()+
  ylab("Proportion of Total Observations")+xlab("Method")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_y_continuous(expand=c(0,0.001))

  #subset to top 5
methodtprovsub<-methodtprov %>%
  mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","hake","butterfish","sandlance","mackerel","unknown fish"),
                                             collapse="|"), prey, ignore.case=T), prey, "other"))
ggplot(methodtprovsub)+ theme_bw()+
  geom_col(aes(x=method, y=proportion, fill=preysub))+
  ylab("Proportion of Total Observations")+xlab("Method")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_y_continuous(expand=c(0,0.001))+
  labs(fill="Prey")+
  scale_fill_poke(pokemon="surskit", spread=8)




# old in process code: species correlations in tern diet data -----------------------------------

library(corrplot)

  #subset
dailyweeklytprovsub<-subset(dailyweeklytprov, prey=="hake"|prey=="herring"|prey=="butterfish"|prey=="sandlance"|prey=="mackerel")

dailyweeklytprovsub<-dailyweeklytprov %>%
  filter(!year %in% c(2003, 2004, 2010, 2012)) %>%
  filter(prey %in% c("herring","hake","butterfish","mackerel","sandlance"))
str(dailyweeklytprovsub)

dfcorr<-dailyweeklytprovsub %>%
  mutate(uniqueID=group_indices(., week, year), abundance=(abundance*10)) %>%
  spread(prey, abundance) %>%
  mutate_all(~replace(., is.na(.),0)) %>%
  select(uniqueID, herring, hake, sandlance, butterfish, mackerel) %>%
  group_by(uniqueID) %>%
  summarise_each(funs(sum)) %>%
  select(herring, hake, sandlance, butterfish, mackerel)

mycorr<-cor(dfcorr, method="kendall")

corrplot(mycorr, method="number", tl.col="black", tl.srt=45)
plot(dfcorr)
corrplot(mycorr, type="upper", order="hclust", tl.col="black", tl.srt=45)
     #the zeros are problematic.








# NMDS TERN DIET DATA -----------------------------------

library(vegan)
library(moments)
library(dplyr)

     #start with "dailyweeklytprov" df from line 109 ish

  #subset
tprovsub<-tprov %>%
  mutate(preysub=preysub<-ifelse(grepl(paste(c("herring","hake","butterfish","sandlance","mackerel","pollock","unknown fish"),
                                             collapse="|"), prey, ignore.case=T), prey, "other")) %>%
  filter(! year %in% c(2003, 2004, 2010, 2012))

dailyweeklytprovsub<-dailyweeklytprov %>%
  filter(!year %in% c(2003, 2004, 2010, 2012)) %>%
  filter(prey %in% c("herring","hake","butterfish","mackerel","sandlance", "bluefish","cunner","lumpfish",
                     "mummichog","pollock","silverside","stickleback")) %>%
  filter(!abundance>300000000) #im fucking annoying for fixing an issue this way but there were weird infinity values in the abundance column that needed to come out

dfNMDS0<-dailyweeklytprovsub %>%
  dplyr::mutate(uniqueID=group_indices(.,year,day)) %>%
  dplyr::select(uniqueID, prey, abundance, year, week) %>%
  spread(prey, abundance) %>%
  dplyr::mutate_all(~replace(., is.na(.),0))

dfNMDS1<-dfNMDS0 %>%
  dplyr::select(uniqueID, year, week) %>%
  group_by(uniqueID) %>%
  dplyr::summarise_all(funs(max))

dfNMDS2<-dfNMDS0 %>%
  dplyr::select(uniqueID,butterfish, hake, herring, mackerel, sandlance, bluefish, cunner, lumpfish,
         mummichog, pollock, silverside, stickleback) %>%
  group_by(uniqueID) %>%
  dplyr::summarise_all(funs(sum))

dfNMDS<-bind_cols(dfNMDS1, dfNMDS2) %>% 
  dplyr::select(-uniqueID...4)


NMDS_1ut<-dfNMDS[,4:12]
NMDS_2<-dfNMDS[,1:3]

  #For the all diets NMDS
original.dist<-vegdist(NMDS_1ut) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r2<-numeric(4)

for (n in 1:4) {
  nmds.resu <- metaMDS(NMDS_1ut, k=n, distance = "bray", try=250, autotransform=F) #Runs an NMDS at each dimension with 250 random starts
  stress_values[n]<-nmds.resu$stress*100 #Extract the ideal stress at that dimension and put it into the vector
  nmds.scores<-scores(nmds.resu)
  nmds.dist<-dist(nmds.scores)
  r2[n]<-summary(lm(original.dist~nmds.dist))[[8]] #Calculate the r-squared of that n-dim ordination
}  #THROWING ERROR HERE



plot(stress_values, xlab="Number of axes", ylab="Stress",type="b")
abline(h=20,col="red") #Rule of thumb, a good ordination will have stress below 20 at least

View(stress_values) #Second rule of thumb is stress should be <5 more than the stress at n+1-dimension

  #Go back and create the output for the 2 dimensions NMDS--chosen based on rules of thumb (1) below 20 and (2) drop <5 to next n-dimension
terndiet_NMDS<-metaMDS(NMDS_1ut, distance = "bray", k = 2, try=250, autotransform=F)
r2<-summary(lm(original.dist~dist(vegan::scores(terndiet_NMDS))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress<-terndiet_NMDS$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


  #Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns<-as.data.frame(terndiet_NMDS$species)
allNMDS_samples_terns<-as.data.frame(terndiet_NMDS$points)
allNMDS_env_terns<-as.data.frame(NMDS_2) %>% 
 dplyr:: select(year,week, uniqueID...1) %>% 
  mutate(binyear=cut(year, breaks=c(-Inf, 2003, 2007, 2011, 2015, Inf), labels=c("1999-2003","2004-2007","2008-2011","2011-2015","2016-2020"))) %>% 
  mutate(binyeardecs=cut(year, breaks=c(-Inf, 2006, 2013, Inf), labels=c("1999-2006","2007-2013","2014-2020"))) %>% 
  mutate(month=cut(week, breaks=c(-Inf, 22, 26, 30, Inf), labels=c("May","June","July","August")))
  


  #Plot filled by year
myord<-ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns,aes(MDS1,MDS2, fill=allNMDS_env_terns$binyeardecs),shape=21,size=2,alpha=0.5)+ #Each point is a sample (unique Id i.e. watch day)
  geom_segment(data=allNMDS_species_terns,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.2, y= 1.45, aes(-.65,1.27,label=paste("Stress =",round(actualStress,digits=4)*100)),size=6)+
  scale_fill_manual(values=c("red","green","yellow"))+
  guides(fill=guide_legend(title=""))

  #plot filled by week
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns,aes(MDS1,MDS2, fill=allNMDS_env_terns$week),shape=21,size=2,alpha=0.5)+ #Each point is a sample (unique Id i.e. watch day)
  geom_segment(data=allNMDS_species_terns,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.5, y= 1.4, aes(-.65,1.27,label=paste("Stress =",round(actualStress,digits=4)*100)),size=6)

  #plot filled by month different method
      #went back and used the alltimetprov df that has months in it
      #TO DO: go back and use this df for the other NMDS above so that I dont have to run multiple models to look at different timescales
alltimetprovsub<-subset(alltimetprov, prey=="hake"|prey=="herring"|prey=="butterfish"|prey=="sandlance"|prey=="mackerel")

alltimetprovsub<-alltimetprov %>%
  filter(!year %in% c(2003, 2004, 2010, 2012)) %>%
  filter(prey %in% c("herring","hake","butterfish","mackerel","sandlance")) %>%
  filter(!abundance>300000000) #im fucking annoying for fixing an issue this way but there were weird infinity values in the abundance column that needed to come out

dfNMDS01<-alltimetprovsub %>%
  mutate(uniqueID=group_indices(.,year,day)) %>%
  select(uniqueID, prey, abundance, n, year, week, month) %>%
  spread(prey, abundance) %>%
  mutate_all(~replace(., is.na(.),0))

dfNMDS11<-dfNMDS01 %>%
  select(uniqueID, year, week, month) %>%
  group_by(uniqueID) %>%
  summarise_all(funs(max))

dfNMDS21<-dfNMDS01 %>%
  select(uniqueID, n, butterfish, hake, herring, mackerel, sandlance) %>%
  group_by(uniqueID) %>%
  summarise_all(funs(sum))

dfNMDS1<-bind_cols(dfNMDS11, dfNMDS21) %>%
  select(!uniqueID...5)

NMDS_1ut1<-dfNMDS1[,6:10]
NMDS_21<-dfNMDS1[,1:5]

  #For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

  #run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions

  #Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

  #plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.5, y= 1.7, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)


#IN PROCESS: NMDS between decades
#-------------------------------#

  # so to start I am just going to run and visualize multiple NMDSs
  # gonna do this in 5 year increments (1999-2004, 2005-2009, 2010-2014, 2015-2019)
  
alltimetprovsub<-subset(alltimetprov, prey=="hake"|prey=="herring"|prey=="butterfish"|prey=="sandlance"|prey=="mackerel")

alltimetprovsub<-alltimetprov %>%
  filter(!year %in% c(2003, 2004, 2010, 2012)) %>%
  filter(prey %in% c("herring","hake","butterfish","mackerel","sandlance")) %>%
  filter(!abundance>300000000) #im fucking annoying for fixing an issue this way but there were weird infinity values in the abundance column that needed to come out

dfNMDS01<-alltimetprovsub %>%
  mutate(uniqueID=group_indices(.,year,day)) %>%
  select(uniqueID, prey, abundance, n, year, week, month) %>%
  spread(prey, abundance) %>%
  mutate_all(~replace(., is.na(.),0))

dfNMDS11<-dfNMDS01 %>%
  select(uniqueID, year, week, month) %>%
  group_by(uniqueID) %>%
  summarise_all(funs(max))

dfNMDS21<-dfNMDS01 %>%
  select(uniqueID, n, butterfish, hake, herring, mackerel, sandlance) %>%
  group_by(uniqueID) %>%
  summarise_all(funs(sum))

dfNMDS1<-bind_cols(dfNMDS11, dfNMDS21) %>%
  select(!uniqueID1)

# FOR 1999-2004 #

dfNMDS1999<-dfNMDS1 %>%
  filter(year %in% c(1999:2004)) #sample size is 115

NMDS_1ut1<-dfNMDS1999[,6:10]
NMDS_21<-dfNMDS1999[,1:5]

    #For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

    #run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


    #Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

    #plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.8, y= 3, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)+
  scale_x_continuous(limits=c(-2.5,3.5))+
  scale_y_continuous(limits=c(-2.5,3))

# FOR 2005-2009 #

dfNMDS2005<-dfNMDS1 %>%
  filter(year %in% c(2005:2009)) #sample size is 70

NMDS_1ut1<-dfNMDS2005[,6:10]
NMDS_21<-dfNMDS2005[,1:5]

    #For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

    #run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


    #Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

    #plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.8, y= 3, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)+
  scale_x_continuous(limits=c(-2.5,3.5))+
  scale_y_continuous(limits=c(-2.5,3))

# FOR 2010-2020 #

dfNMDS2019<-dfNMDS1 %>%
  filter(year %in% c(2019:2020)) #sample size is 74

NMDS_1ut1<-dfNMDS2019[,6:10]
NMDS_21<-dfNMDS2019[,1:5]

    #For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

    #run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


    #Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

    #plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.8, y= 3, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)+
  scale_x_continuous(limits=c(-2.5,3.5))+
  scale_y_continuous(limits=c(-2.5,3))

# FOR FIRST DECADE ISH (i.e. 1999-2009) #

dfNMDSdec1<-dfNMDS1 %>%
  filter(year %in% c(1999:2009)) #sample size is 185

NMDS_1ut1<-dfNMDSdec1[,6:10]
NMDS_21<-dfNMDSdec1[,1:5]

  #For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

  #run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


  #Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

  #plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.8, y= 3, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)+
  scale_x_continuous(limits=c(-2.5,3.5))+
  scale_y_continuous(limits=c(-2.5,3))

# FOR SECOND DECADE ISH (i.e. 2010-2020) #

dfNMDSdec2<-dfNMDS1 %>%
  filter(year %in% c(2010:2020)) #sample size is 208

NMDS_1ut1<-dfNMDSdec2[,6:10]
NMDS_21<-dfNMDSdec2[,1:5]

#For the all diets NMDS
original.dist1<-vegdist(NMDS_1ut1) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values1<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r21<-numeric(4)

#run NMDS
terndiet_NMDS1<-metaMDS(NMDS_1ut1, distance = "bray", k = 2, try=250, autotransform=F)
r21<-summary(lm(original.dist1~dist(vegan::scores(terndiet_NMDS1))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress1<-terndiet_NMDS1$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS1) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


#Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns1<-as.data.frame(terndiet_NMDS1$species)
allNMDS_samples_terns1<-as.data.frame(terndiet_NMDS1$points)
allNMDS_env_terns1<-as.data.frame(NMDS_21)

#plot filled by month
ggplot()+theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_point(data=allNMDS_samples_terns1,aes(MDS1,MDS2, fill=allNMDS_env_terns1$month),shape=21,size=2,alpha=0.5)+
  stat_ellipse(data=allNMDS_samples_terns1,aes(MDS1,MDS2,color=allNMDS_env_terns1$month),lwd=1.0)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  geom_segment(data=allNMDS_species_terns1,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns1,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns1))),fill="grey90",size=6)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  geom_text(x=-1.8, y= 3, aes(-.65,1.27,label=paste("Stress =",round(actualStress1,digits=4)*100)),size=6)+
  scale_x_continuous(limits=c(-2.5,3.5))+
  scale_y_continuous(limits=c(-2.5,3))


#IN PROCESS: NMDS using watch ID as sample as opposed to day
#----------------------------------------------------------#








# old in process: NMDS TERN DIET vs NMFS -----------------------------------
library(vegan)

#NMDS with pres abs on only tern diet data #
#------------------------------------------#

dailyweeklytprovsub<-subset(dailyweeklytprov, prey=="hake"|prey=="herring"|prey=="butterfish"|prey=="sandlance"|prey=="mackerel")

dailyweeklytprovsub<-dailyweeklytprov %>%
  filter(!year %in% c(2003, 2004, 2010, 2012)) %>%
  filter(prey %in% c("herring","hake","butterfish","mackerel","sandlance"))

dfNMDS<-dailyweeklytprovsub %>%
  mutate(uniqueID=group_indices(.,year,day)) %>%
  select(uniqueID, prey, abundance, n) %>%
  spread(prey, abundance) %>%
  mutate_all(~replace(., is.na(.),0)) %>%
  group_by(uniqueID) %>%
  summarise_each(funs(sum)) %>%
  select(n, uniqueID, butterfish, herring, hake, sandlance, mackerel) #%>%
  #filter(n>20)

NMDS_1ut<-dfNMDS[,3:7]
NMDS_2<-dfNMDS[,1:2]

NMDS_1utpa<-decostand(x=NMDS_1ut, method="pa")

  #For the all diets NMDS
original.dist<-vegdist(NMDS_1utpa) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r2<-numeric(4)

for (n in 1:4) {
  nmds.resu <- metaMDS(NMDS_1utpa, k=n, distance = "bray", try=250, autotransform=F) #Runs an NMDS at each dimension with 250 random starts
  stress_values[n]<-nmds.resu$stress*100 #Extract the ideal stress at that dimension and put it into the vector
  nmds.scores<-scores(nmds.resu)
  nmds.dist<-dist(nmds.scores)
  r2[n]<-summary(lm(original.dist~nmds.dist))[[8]] #Calculate the r-squared of that n-dim ordination
}
plot(stress_values, xlab="Number of axes", ylab="Stress",type="b")
abline(h=20,col="red") #Rule of thumb, a good ordination will have stress below 20 at least

View(stress_values) #Second rule of thumb is stress should be <5 more than the stress at n+1-dimension

  #Go back and create the output for the 2 dimensions NMDS--chosen based on rules of thumb (1) below 20 and (2) drop <5 to next n-dimension
terndiet_NMDS<-metaMDS(NMDS_1utpa, distance = "bray", k = 2, try=250, autotransform=F)
r2<-summary(lm(original.dist~dist(vegan::scores(terndiet_NMDS))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress<-terndiet_NMDS$stress #calculate the stress of this chosen NMDS ord
stressplot(terndiet_NMDS) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


  #Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_terns<-as.data.frame(terndiet_NMDS$species)
allNMDS_samples_terns<-as.data.frame(terndiet_NMDS$points)
allNMDS_env_terns<-as.data.frame(NMDS_2)


  #Plot
ggplot()+
  geom_point(data=allNMDS_samples_terns,aes(MDS1,MDS2),shape=21,size=2,alpha=0.5)+ #Each point is a sample (trawl)
  geom_segment(data=allNMDS_species_terns,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_terns,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns))),size=8)+ #Label the lines with the species
  #stat_ellipse(data=allNMDS_samples_terns,aes(MDS1,MDS2),lwd=1.1)+  #Draws a 95% ellipse around points in different categories--an example of understanding a categorical variable with NMDS
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  #scale_x_continuous(limits=c(-0.7,0.7))+
  #scale_y_continuous(limits=c(-0.301,0.455))+
  geom_text(aes(-.65,1.27,label=paste("Stress =",round(actualStress,digits=4)*100)),size=10,fontface="bold") #Always provide ordination stress

#  NMDS with pres abs on only trawl data #
#----------------------------------------#

setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
trawl<-read.csv("buffer_pa_tern.MV.csv")

trawldfNMDS<-trawl %>%
  mutate(herring=(ATLANTIC_HERRING+BLUEBACK_HERRING+ALEWIFE),
         hake=(RED_HAKE+SILVER_HAKE+WHITE_HAKE)) %>%
  rename(uniqueID=ID,sandlance=NORTHERN_SAND_LANCE, mackerel=ATLANTIC_MACKEREL, butterfish=BUTTERFISH) %>%
  select(uniqueID, butterfish, herring, hake, sandlance, mackerel)


NMDS_1trawl<-trawldfNMDS[,2:6]
NMDS_2trawl<-trawldfNMDS[,1]

NMDS_1trawlpa<-decostand(x=NMDS_1trawl, method="pa")
original.dist<-vegdist(NMDS_1trawlpa) #Need to choose an initial distance matrix, the default is using bray-curtis distance
stress_values<-numeric(4) #Choose n-dimensions you want to test, carries through in the next few lines
r2<-numeric(4)

for (n in 1:4) {
  nmds.resu <- metaMDS(NMDS_1trawlpa, k=n, distance = "bray", try=250, autotransform=F) #Runs an NMDS at each dimension with 250 random starts
  stress_values[n]<-nmds.resu$stress*100 #Extract the ideal stress at that dimension and put it into the vector
  nmds.scores<-scores(nmds.resu)
  nmds.dist<-dist(nmds.scores)
  r2[n]<-summary(lm(original.dist~nmds.dist))[[8]] #Calculate the r-squared of that n-dim ordination
}
plot(stress_values, xlab="Number of axes", ylab="Stress",type="b")
abline(h=20,col="red") #Rule of thumb, a good ordination will have stress below 20 at least

View(stress_values) #Second rule of thumb is stress should be <5 more than the stress at n+1-dimension

  #Go back and create the output for the 2 dimensions NMDS--chosen based on rules of thumb (1) below 20 and (2) drop <5 to next n-dimension
trawl_NMDS<-metaMDS(NMDS_1trawlpa, distance = "bray", k = 2, try=250, autotransform=F)
r2<-summary(lm(original.dist~dist(vegan::scores(trawl_NMDS))))[[8]] #calculate the r-squared of this chosen NMDS ord
actualStress<-trawlt_NMDS$stress #calculate the stress of this chosen NMDS ord
stressplot(trawl_NMDS) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions


  #Ordination plots for the NMDS 2D results--put them into dfs for ggplot to use, better visuals than vegan plots (I think)
allNMDS_species_trawl<-as.data.frame(trawl_NMDS$species)
allNMDS_samples_trawl<-as.data.frame(trawl_NMDS$points)
allNMDS_env_trawl<-as.data.frame(NMDS_2trawl)


  #Plot
ggplot()+
  geom_point(data=allNMDS_samples_trawl,aes(MDS1,MDS2),shape=21,size=2,alpha=0.5)+ #Each point is a sample (trawl)
  geom_segment(data=allNMDS_species_trawls,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ #Each line is to the species ordination points
  geom_label(data=allNMDS_species_trawl,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species_terns))),size=8)+ #Label the lines with the species
  xlab("Axis 1")+ylab("Axis 2")+ #Can add the r-squared, but have to do it separately for the two axes and R doesn't calculate this (controversial among statisticians as to whether it is accurate/meaningful)
  #scale_x_continuous(limits=c(-0.7,0.7))+
  #scale_y_continuous(limits=c(-0.301,0.455))+
  geom_text(aes(-.65,1.27,label=paste("Stress =",round(actualStress,digits=4)*100)),size=10,fontface="bold") #Always provide ordination stress



# NMDS with presence absence data between tern and NMFS #
#-----------------------------------------------------------#



# SIZE DISTRIBUTION TERN DATA -----------------------------------


tprovsize0 <- dfclean %>% 
  drop_na(size) %>% 
  filter(prey %in% c("herring","hake","butterfish","mackerel","sandlance","pollock")) %>% 
  select(year,prey,size)

tprovsizebyyear<-tprovsize0 %>% 
  group_by(prey,year) %>% 
  summarise(meansize=mean(as.numeric(size)),minsize=min(as.numeric(size)),maxsize=max(as.numeric(size))) 

tprovsizeallyrs<-tprovsize0 %>% 
  group_by(prey) %>% 
  summarise(meansize=mean(as.numeric(size)),minsize=min(as.numeric(size)),maxsize=max(as.numeric(size))) 

