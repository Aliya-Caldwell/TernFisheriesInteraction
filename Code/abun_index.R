
setwd("C:/Users/nh1087/Documents/UNH Research/Seabirds/Data/")
rm(list=ls())
set.seed(42)

library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(RColorBrewer)
library(MCMCglmm)
library(lubridate)
library(ggmap)
library(nlme)
library(argosfilter)
library(rgdal)
library(tidyr)


theme_set(theme_bw(base_size=25))

#Better categorization function
spNMFSCats<-function(x,y) {
  if (grepl(paste(c("HAKE"),collapse="|"),x,ignore.case=T)) { #Maybe rockling should be included here?
    y="HAKE"
  } else if (grepl("BUTTERFISH",x,ignore.case=T)) {
    y="BUTTERFISH"
  } else if (grepl("LANCE",x,ignore.case=T)) {
    y="SAND LANCE"
  } else if (grepl("POLLOCK",x,ignore.case=T)) {
    y="POLLOCK"
  } else if (grepl("MACKEREL",x,ignore.case=T)) {
    y="MACKEREL"
  } else if (grepl(paste(c("HERRING","ALEWIFE"),collapse="|"),x,ignore.case=T)) {
    y="HERRING"
  }  else {
    y="OTHER"
  }
}

#A better is.nan function
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

#Not in
`%notin%` <- Negate(`%in%`)


#Total catch for the different families (and species) in each year-season
GoM_catch<-read_csv("GoM_NMFS_preppedData.csv") #Use this if you just want the families--but having the species will be important



#This is all copied from the exploration script, then adjusted to keep species too
springT<-read_csv("NMFS Trawls/Spring/22561_UNION_FSCS_SVCAT.csv", 
                  col_types = cols(CRUISE6 = col_character(), ID = col_character(), 
                                   STATION = col_double(), STRATUM = col_double(), 
                                   TOW = col_double()))

springT$Year<-substr(springT$CRUISE6,1,4)

fallT<-read_csv("NMFS Trawls/Fall/22560_UNION_FSCS_SVCAT.csv", 
                col_types = cols(CRUISE6 = col_character(), ID = col_character(), 
                                 STATION = col_double(), STRATUM = col_double(), 
                                 TOW = col_double()))

fallT$Year<-substr(fallT$CRUISE6,1,4)

#Extracting the species to keep, should ROCKLING be added? They were never aged...
species_trawl<-unique(springT$LOGGED_SPECIES_NAME)
seabird_trawl<-subset(springT,
                      grepl(paste(c("HAKE","BUTTERFISH","POLLOCK","MACKEREL","ALEWIFE","HERRING","SAND LANCE"),
                                  collapse="|"),ignore.case=T,springT$LOGGED_SPECIES_NAME))

seabird_trawl<-seabird_trawl[!duplicated(seabird_trawl$SVSPP),6:7]
seabird_trawl<-seabird_trawl[c(1:8,10,13),]



trawlLocations_Fall<-read_csv("~/UNH Research/Seabirds/Data/NMFS Trawls/Fall/22560_UNION_FSCS_SVSTA.csv", 
                              col_types = cols(CRUISE6 = col_character(), ID = col_character(), 
                                               STATION = col_double(), STRATUM = col_double(), 
                                               TOW = col_double(), AREA = col_character()))%>%
  dplyr::select(CRUISE6,ID,STATION,STRATUM,TOW,LONG=DECDEG_BEGLON,LAT=DECDEG_BEGLAT,
                Year=EST_YEAR,Month=EST_MONTH,Day=EST_DAY,Time=EST_TIME,AREA,DESSPEED,TOWDUR)

trawlLocations_Spring<-read_csv("~/UNH Research/Seabirds/Data/NMFS Trawls/Spring/22561_UNION_FSCS_SVSTA.csv", 
                                col_types = cols(CRUISE6 = col_character(), ID = col_character(), 
                                                 STATION = col_double(), STRATUM = col_double(), 
                                                 TOW = col_double(), AREA=col_character()))%>%
  dplyr::select(CRUISE6,ID,STATION,STRATUM,TOW,LONG=DECDEG_BEGLON,LAT=DECDEG_BEGLAT,
                Year=EST_YEAR,Month=EST_MONTH,Day=EST_DAY,Time=EST_TIME,AREA,DESSPEED,TOWDUR)

springT$season<-"SPRING"
fallT$season<-"FALL"

catchLocations<-merge(rbind(springT,fallT),rbind(trawlLocations_Fall,trawlLocations_Spring),all.x=T)

#Both are missing values quite a bit, 20% for duration and 35% for speed
  #Replaced those with the average
catchLocations$TOWDUR<-ifelse(is.na(catchLocations$TOWDUR),mean(catchLocations$TOWDUR,na.rm=T),catchLocations$TOWDUR)
catchLocations$DESSPEED<-ifelse(is.na(catchLocations$DESSPEED),mean(catchLocations$DESSPEED,na.rm=T),catchLocations$DESSPEED)
catchLocations$Effort<-catchLocations$DESSPEED*(catchLocations$TOWDUR/60)
#Dates for trawls
catchLocations$Date<-ymd(paste(catchLocations$Year,catchLocations$Month,catchLocations$Day,sep="-"))
catchLocations$season<-factor(catchLocations$season,levels=c("SPRING","FALL"))

GOMtrawls<-subset(catchLocations,AREA<523)

#Brief exploration into rarer species in the tern diets
rare<-GOMtrawls%>%
  filter(grepl(paste(c("LUMPFISH","CUNNER","SILVERSIDE","BLUEFISH"),collapse="|"),LOGGED_SPECIES_NAME,ignore.case=T))
rare2<-rare%>%
  group_by(Year,season,LOGGED_SPECIES_NAME)%>%
  dplyr::summarise(CatchNum=sum(EXPCATCHNUM,na.rm=T),
                   CatchWgt=sum(EXPCATCHWT,na.rm=T))
View(unique(dplyr::select(rare2,Year,season)))



GoM_Seabirds<-filter(GOMtrawls,grepl(paste(seabird_trawl$SVSPP,collapse="|"),
                                     GOMtrawls$SVSPP,ignore.case=T))%>%
  dplyr::select(Year,CRUISE6,STRATUM,STATION,SVSPP,season,
                Pounds=EXPCATCHWT,Count=EXPCATCHNUM,LONG:Effort)

GoM_Seabirds<-merge(GoM_Seabirds,seabird_trawl)%>%
  rename(Species=LOGGED_SPECIES_NAME)%>%
  arrange(Year,Species)
GoM_Seabirds<-GoM_Seabirds[!is.na(GoM_Seabirds$Pounds) & !is.na(GoM_Seabirds$Count),]


GoM_Seabirds$Family<-sapply(GoM_Seabirds$Species,spNMFSCats)


GOM_simple<-GoM_Seabirds%>%
  dplyr::select(Year,season,CRUISE6,STATION,STRATUM,Effort)%>%unique()%>%
  group_by(Year,season)%>%
  mutate(Trawls_Efforts=sum(Effort))%>%
  right_join(GoM_Seabirds,GOM_simple,by=c("Year","season","CRUISE6","STATION","STRATUM","Effort"))%>%
  group_by(Year,season)%>%
  mutate(Trawls=n_distinct(CRUISE6,STATION,STRATUM))%>%
  ungroup()%>%
  group_by(Year,season,Trawls,Trawls_Efforts,Family)%>%
  mutate(fam_Wt=sum(Pounds),
         fam_Num=sum(Count,na.rm=T),
         famRel_Wt=fam_Wt/Trawls,
         famRel_Num=fam_Num/Trawls,
         famERel_Wt=fam_Wt/Trawls_Efforts,
         famERel_Num=fam_Num/Trawls_Efforts)%>%
  group_by(Year,season,Trawls,Trawls_Efforts,Family,Species)%>%
  mutate(species_Wt=sum(Pounds),
         species_Num=sum(Count,na.rm=T),
         species_Rel_Wt=species_Wt/Trawls,
         species_Rel_Num=species_Num/Trawls,
         species_ERel_Wt=species_Wt/Trawls_Efforts,
         species_ERel_Num=species_Num/Trawls_Efforts)%>%
  dplyr::select(Year,season,Trawls,Trawls_Efforts,Species,Family:species_ERel_Num)%>%unique()%>%
  arrange(Year,season,Family,Species)

GOM_simple$Year<-as.numeric(GOM_simple$Year)


#Proportion of catch in each year
GoM_props<-GoM_Seabirds%>%
  dplyr::group_by(Year,season)%>%
  mutate(Total_Wt=sum(Pounds),
         Total_Num=sum(Count))%>%
  dplyr::group_by(Year,season,Family)%>%
  mutate(famCatch=sum(Pounds),
         famProp=famCatch/Total_Wt)%>%
  dplyr::group_by(Year,season,Family,Species)%>%
  mutate(Catch=sum(Pounds),
         Prop=Catch/Total_Wt)%>%
  dplyr::select(Year,season,Family,Species,Total_Wt,Total_Num,famProp,Prop)%>%unique()%>%
  arrange(Year,season,Family,Species)

GoM_props$Year<-as.numeric(GoM_props$Year)


#Accounting for the fecal DNA proportions for the different species
DNA<-read.csv("fecal dna species.csv",stringsAsFactors = F)

GoM_dna<-GoM_Seabirds%>%
  dplyr::select(Year,season,CRUISE6,STATION,STRATUM,Effort)%>%unique()%>%
  group_by(Year,season)%>%
  mutate(Trawls_Efforts=sum(Effort))%>%
  right_join(GoM_Seabirds,GOM_simple,by=c("Year","season","CRUISE6","STATION","STRATUM","Effort"))%>%
  group_by(Year,season)%>%
  mutate(Trawls=n_distinct(CRUISE6,STATION,STRATUM))%>%
  ungroup()%>%
  merge(DNA,all.x=T)%>%
  dplyr::group_by(Year,season,Family,Species)%>%
  mutate(species_Wt=sum(Pounds),species_Num=sum(Count),
         species_DNAweight_Wt=ifelse(is.na(DNA_count),species_Wt,species_Wt*(DNA_count/35)),
         species_DNAweight_Num=ifelse(is.na(DNA_count),species_Num,species_Num*(DNA_count/35)))%>%
  ungroup()%>%
  dplyr::select(Year,season,Trawls,Trawls_Efforts,Family,Species,species_Wt,species_Num,species_DNAweight_Wt,species_DNAweight_Num)%>%distinct()%>%
  dplyr::group_by(Year,season,Family)%>%
  mutate(fam_Wt=sum(species_Wt),fam_Num=sum(species_Num),
         fam_DNAweight_Wt=sum(species_DNAweight_Wt),
         fam_DNAweight_Num=sum(species_DNAweight_Num),
         fam_DNAweight_Rel_Wt=fam_DNAweight_Wt/Trawls,
         fam_DNAweight_Rel_Num=fam_DNAweight_Num/Trawls,
         fam_DNAweight_ERel_Wt=fam_DNAweight_Wt/Trawls_Efforts,
         fam_DNAweight_ERel_Num=fam_DNAweight_Num/Trawls_Efforts)%>%
  arrange(Year,season,Family,Species)

GoM_dna$Year<-as.numeric(GoM_dna$Year)




GoM_all<-merge(GOM_simple,GoM_props)%>%
  merge(GoM_dna)%>%
  mutate(Year=as.character(Year))


#Doing the same for a buffer range
white<-c(42.966,-70.625)
buffertrawls<-catchLocations%>%
  rowwise()%>%
  mutate(distance_km=argosfilter::distance_m(white[1],white[2],LAT,LONG)/1000)%>%
  filter(distance_km<50)%>% ##############Setting the buffer distance here
  ungroup()


buffer_Seabirds<-filter(buffertrawls,grepl(paste(seabird_trawl$SVSPP,collapse="|"),
                                     buffertrawls$SVSPP,ignore.case=T))%>%
  dplyr::select(Year,CRUISE6,STRATUM,STATION,SVSPP,season,
                Pounds=EXPCATCHWT,Count=EXPCATCHNUM,LONG:Effort)

buffer_Seabirds<-merge(buffer_Seabirds,seabird_trawl)%>%
  rename(Species=LOGGED_SPECIES_NAME)%>%
  arrange(Year,Species)
buffer_Seabirds<-buffer_Seabirds[!is.na(buffer_Seabirds$Pounds),]


buffer_Seabirds$Family<-sapply(buffer_Seabirds$Species,spNMFSCats)


buffer_simple<-buffer_Seabirds%>%
  dplyr::select(Year,season,CRUISE6,STATION,STRATUM,Effort)%>%unique()%>%
  group_by(Year,season)%>%
  mutate(Trawls_Efforts=sum(Effort))%>%
  right_join(buffer_Seabirds,buffer_simple,by=c("Year","season","CRUISE6","STATION","STRATUM","Effort"))%>%
  group_by(Year,season)%>%
  mutate(Trawls=n_distinct(CRUISE6,STATION,STRATUM))%>%
  ungroup()%>%
  group_by(Year,season,Trawls,Trawls_Efforts,Family)%>%
  mutate(famCatch_Wt=sum(Pounds),
         famCatch_Num=sum(Count,na.rm=T),
         famRel_Wt=famCatch_Wt/Trawls,
         famRel_Num=famCatch_Num/Trawls,
         famERel_Wt=famCatch_Wt/Trawls_Efforts,
         famERel_Num=famCatch_Num/Trawls_Efforts)%>%
  group_by(Year,season,Trawls,Trawls_Efforts,Family,Species)%>%
  mutate(Catch_Wt=sum(Pounds),
         Catch_Num=sum(Count,na.rm=T),
         Rel_Wt=Catch_Wt/Trawls,
         Rel_Num=Catch_Num/Trawls,
         ERel_Wt=Catch_Wt/Trawls_Efforts,
         ERel_Num=Catch_Num/Trawls_Efforts)%>%
  dplyr::select(Year,season,Trawls,Trawls_Efforts,Species,Family:ERel_Num)%>%unique()%>%
  arrange(Year,season,Family,Species)

buffer_simple$Year<-as.numeric(buffer_simple$Year)


#Proportion of catch in each year
buffer_props<-buffer_Seabirds%>%
  dplyr::group_by(Year,season)%>%
  mutate(Total_Wt=sum(Pounds),
         Total_Num=sum(Count))%>%
  dplyr::group_by(Year,season,Family)%>%
  mutate(famCatch=sum(Pounds),
         famProp=famCatch/Total_Wt)%>%
  dplyr::group_by(Year,season,Family,Species)%>%
  mutate(Catch=sum(Pounds),
         Prop=Catch/Total_Wt)%>%
  dplyr::select(Year,season,Family,Species,Total_Wt,Total_Num,famProp,Prop)%>%unique()%>%
  arrange(Year,season,Family,Species)

buffer_props$Year<-as.numeric(buffer_props$Year)


#Having the DNA weighting
buffer_dna<-buffer_Seabirds%>%
  dplyr::select(Year,season,CRUISE6,STATION,STRATUM,Effort)%>%unique()%>%
  group_by(Year,season)%>%
  mutate(Trawls_Efforts=sum(Effort))%>%
  right_join(buffer_Seabirds,buffer_simple,by=c("Year","season","CRUISE6","STATION","STRATUM","Effort"))%>%
  group_by(Year,season)%>%
  mutate(Trawls=n_distinct(CRUISE6,STATION,STRATUM))%>%
  ungroup()%>%
  merge(DNA,all.x=T)%>%
  dplyr::group_by(Year,season,Family,Species)%>%
  mutate(species_Wt=sum(Pounds),species_Num=sum(Count),
         species_DNAweight_Wt=ifelse(is.na(DNA_count),species_Wt,species_Wt*(DNA_count/35)),
         species_DNAweight_Num=ifelse(is.na(DNA_count),species_Num,species_Num*(DNA_count/35)))%>%
  ungroup()%>%
  dplyr::select(Year,season,Trawls,Trawls_Efforts,Family,Species,species_Wt,species_Num,species_DNAweight_Wt,species_DNAweight_Num)%>%distinct()%>%
  dplyr::group_by(Year,season,Family)%>%
  mutate(fam_Wt=sum(species_Wt),fam_Num=sum(species_Num),
         fam_DNAweight_Wt=sum(species_DNAweight_Wt),
         fam_DNAweight_Num=sum(species_DNAweight_Num),
         fam_DNAweight_Rel_Wt=fam_DNAweight_Wt/Trawls,
         fam_DNAweight_Rel_Num=fam_DNAweight_Num/Trawls,
         fam_DNAweight_ERel_Wt=fam_DNAweight_Wt/Trawls_Efforts,
         fam_DNAweight_ERel_Num=fam_DNAweight_Num/Trawls_Efforts)%>%
  arrange(Year,season,Family,Species)

buffer_dna$Year<-as.numeric(buffer_dna$Year)




buffer_all<-merge(buffer_simple,buffer_props)%>%
  merge(buffer_dna)%>%
  mutate(Year=as.character(Year))


# Proportion in Tern Range ------------------------------------------------


#Calculate the proportion of each trawl (season and year) that is in the tern size range
#Need the fish lengths (with species) for each trawls (season and year)
fallTrawl_Sizes<-read_csv("~/UNH Research/Seabirds/Data/NMFS Trawls/Fall/22560_UNION_FSCS_SVLEN.csv", 
                          col_types = cols(CATCHSEX = col_character(), ID = col_character(),
                                           CRUISE6 = col_character(), STATION = col_number(),
                                           STRATUM = col_number(), TOW = col_number()))%>%
  mutate(season="Fall")

springTrawl_Sizes<-read_csv("~/UNH Research/Seabirds/Data/NMFS Trawls/Spring/22561_UNION_FSCS_SVLEN.csv", 
                            col_types = cols(CATCHSEX = col_character(), ID = col_character(),
                                             CRUISE6 = col_character(), STATION = col_number(),
                                             STRATUM = col_number(), TOW = col_number()))%>%
  mutate(season="Spring")

allTrawl_Sizes<-bind_rows(fallTrawl_Sizes,springTrawl_Sizes)%>%
  filter(grepl(paste(seabird_trawl$SVSPP,collapse="|"),SVSPP,ignore.case=T))%>%
  mutate(Year=substr(CRUISE6,1,4))%>%
  dplyr::select(Year,CRUISE6,STRATUM,TOW,STATION,SVSPP,season,Sex=CATCHSEX,LENGTH)
allTrawl_Sizes<-left_join(allTrawl_Sizes,unique(catchLocations[,c(1:4,6,13,14,18,22)]))
allTrawl_Sizes<-left_join(allTrawl_Sizes,seabird_trawl)%>%
  rename(Species=LOGGED_SPECIES_NAME)%>%
  arrange(Year,Species,CRUISE6,STRATUM,TOW,STATION)

allTrawl_Sizes$Family<-sapply(allTrawl_Sizes$Species,spNMFSCats)

GoMTrawl_Sizes<-filter(allTrawl_Sizes,AREA<523)

ggplot(allTrawl_Sizes)+
  geom_density(aes(LENGTH,fill=Family),alpha=0.5)+
  facet_wrap(~Family)





ydayDate<-function(y) {
  as.Date(paste0(2020, "-01-01")) + (y-1)
}

trawl_DateRanges<-catchLocations%>%
  group_by(season)%>%
  summarise(firstDate=ydayDate(min(yday(Date))),lastDate=ydayDate(max(yday(Date))))
trawls_onDates<-catchLocations%>%
  group_by(season,ydayDate(yday(Date)))%>%
  summarise(Trawls=n_distinct(ID))



ggplot(unique(dplyr::select(catchLocations,Date,ID,season)))+
  geom_hline(yintercept=0,lwd=1)+
  geom_histogram(aes(x=yday(Date),fill=season),binwidth=1,color="grey20",alpha=0.95)+
  scale_fill_brewer(palette="Set1",name="Season")+
  scale_x_continuous(limits=c(0,366),expand=c(0,0),name="Day of Year",
                     breaks=c(32,92,153,214,275,336),labels=c("Feb","Apr","Jun","Aug","Oct","Dec"))+
  scale_y_continuous(name="Number of Unique Trawls",limits=c(0,385),expand=c(0,1))

ggplot(unique(dplyr::select(subset(catchLocations,Year>1998),Year,Date,ID,season)))+
  geom_hline(yintercept=0,lwd=1)+
  geom_histogram(aes(x=yday(Date),fill=season),binwidth=1,color="grey20",alpha=0.95)+
  facet_wrap(~Year)+
  scale_fill_brewer(palette="Set1",name="Season")+
  scale_x_continuous(limits=c(0,366),expand=c(0,0),name="Day of Year",
                     breaks=c(32,92,153,214,275,336),labels=c("Feb","Apr","Jun","Aug","Oct","Dec"))+
  scale_y_continuous(name="Number of Unique Trawls",expand=c(0,1))




#Sizes with trawl locations and dates
size_loc_date<-left_join(allTrawl_Sizes,dplyr::select(distinct(catchLocations,ID,.keep_all=T),
                                                      CRUISE6,TOW,STRATUM,STATION,LONG,LAT,Date))
size_loc_date$season<-factor(size_loc_date$season,levels=c("Spring","Fall"))

#Crop to the smaller fish, to plot faster and this is really all that's that interesting anyway...
smallSize_loc_date<-subset(size_loc_date,LENGTH<20)

ggplot(smallSize_loc_date)+
  geom_point(aes(yday(Date),LENGTH,fill=season),size=4,shape=21)+
  facet_wrap(~Species)+
  scale_fill_brewer(palette="Set1",name="Season")+
  scale_x_continuous(limits=c(0,366),expand=c(0,0),name="Day of Year",
                     breaks=c(32,92,153,214,275,336),labels=c("Feb","Apr","Jun","Aug","Oct","Dec"))+
  scale_y_continuous(name="Length (cm)",expand=c(0,1))

#Calculating a single mean, min, and max
summarySize_loc_date<-size_loc_date%>%
  group_by(Species,season,yday(Date))%>%
  summarise(minLength=min(LENGTH),meanLength=mean(LENGTH),maxLength=max(LENGTH))%>%
  rename(Date=`yday(Date)`)
  
ggplot()+
  geom_point(data=summarySize_loc_date,aes(Date,minLength,fill=season),shape=21,size=5)+
  geom_smooth(data=summarySize_loc_date,aes(Date,minLength),method="lm",color="black",lwd=1.5,show.legend = F)+
  #geom_point(data=summarySize_loc_date,aes(Date,meanLength,fill=season),shape=22,size=5,show.legend=F)+
  #geom_point(data=summarySize_loc_date,aes(Date,maxLength,fill=season),shape=23,size=5,show.legend=F)+
  facet_wrap(~Species)+
  scale_fill_brewer(palette="Set1",name="Season")+
  scale_x_continuous(limits=c(0,366),expand=c(0,0),name="Day of Year",
                     breaks=c(32,92,153,214,275,336),labels=c("Feb","Apr","Jun","Aug","Oct","Dec"))+
  scale_y_continuous(name="Minimum Length (cm)",expand=c(0,3),limits=c(0,20))










# Aging-Von Bs ------------------------------------------------------------

fallAges<-read_csv("NMFS Trawls/Fall/22560_UNION_FSCS_SVBIO.csv",
                   col_types=cols(ID=col_character(),CRUISE6=col_character(),
                                  STRATUM=col_double(),TOW=col_double(),STATION=col_double(),
                                  STOM_VOLUME=col_double(),STOM_WGT=col_double()))
springAges<-read_csv("NMFS Trawls/Spring/22561_UNION_FSCS_SVBIO.csv",
                     col_types=cols(ID=col_character(),CRUISE6=col_character(),
                                    STRATUM=col_double(),TOW=col_double(),STATION=col_double(),
                                    STOM_VOLUME=col_double(),STOM_WGT=col_double()))

fallAges$season<-"FALL"
springAges$season<-"SPRING"
springAges$Year<-(substr(springAges$CRUISE6,1,4))
fallAges$Year<-(substr(fallAges$CRUISE6,1,4))


ages<-left_join(rbind(fallAges,springAges),seabird_trawl)
ages_seabirds<-ages[!is.na(ages$LOGGED_SPECIES_NAME),c(1:5,6,9,13:18)]
ages_seabirds<-rename(ages_seabirds,Species=LOGGED_SPECIES_NAME)

ages_seabirds$Family<-sapply(ages_seabirds$Species,spNMFSCats)

ages_seabirds<-ages_seabirds[!is.na(ages_seabirds$AGE),]
ages_seabirds$season<-factor(ages_seabirds$season,levels=c("SPRING","FALL"))

ages_Locations<-left_join(ages_seabirds,catchLocations)

ages_GoM<-filter(ages_Locations,AREA<523)

#Brief look at these rare prey species to see what data exists on them
#176-CUNNER, #168/249-LUMPFISH (and UNCL LUMP/SNAILFISH), #113-ATL SILVERSIDE, #135-BLUEFISH
rare3<-bind_rows(fallAges,springAges)%>%
  filter(grepl(paste(c("176","168","113","135","249"),collapse="|"),SVSPP,ignore.case=T),
         !is.na(AGE))
rare4<-left_join(rare3,catchLocations[,c(-9)])%>%
  filter(AREA<523)



#Spring Trawl vonB for each species
library(nlme)

springVB <- nlsList(LENGTH ~ Li*(1-exp(-1*k*(AGE-t0))) | Species,
                   data=subset(ages_GoM,season=="SPRING"),
                   start=c(Li=50, k=0.26, t0=-1.10),
                   na.action=na.pass)
springVB<-coef(springVB)
springVB$Species<-rownames(springVB)
springVB<-merge(springVB,as.data.frame(table(subset(ages_GoM,season=="SPRING")$Species,dnn="Species")))
springVB$season<-ifelse(springVB$Species=="BLUEBACK HERRING","NA","SPRING")



#Fall Trawl vonB for each species
fallVB <- nlsList(LENGTH ~ Li*(1-exp(-1*k*(AGE-t0))) | Species,
                    data=subset(ages_GoM,season=="FALL"),
                    start=c(Li=50, k=0.26, t0=-1.10),
                    na.action=na.pass)
fallVB<-coef(fallVB)
fallVB$Species<-rownames(fallVB)
fallVB<-merge(fallVB,as.data.frame(table(subset(ages_GoM,season=="FALL")$Species,dnn="Species")))
fallVB$season<-ifelse(fallVB$Species=="BLUEBACK HERRING","NA","FALL")

VB<-rbind(springVB,fallVB)

#Pred Sizes
#Take the difference of date caught and the first day of tern catch (06-16; 168) and last day of tern catch (08-06; 219)
#Add (for spring) or subtract (for fall) those differences from the age estimated
#Estimate the size of a fish at those two ages, the min and max size that fish was during the tern period

#But first, fill in the missing dates with the average date in each season
ydayDate(round(mean(yday(subset(ages_Locations,season=="FALL")$Date),na.rm=T)))
ydayDate(round(mean(yday(subset(ages_Locations,season=="SPRING")$Date),na.rm=T)))

ages_Locations$Month<-ifelse(!is.na(ages_Locations$Date),ages_Locations$Month,
                            ifelse(ages_Locations$season=="FALL","10","04"))
ages_Locations$Day<-ifelse(!is.na(ages_Locations$Date),ages_Locations$Day,
                             ifelse(ages_Locations$season=="FALL","16","09"))
ages_Locations$Date<-ymd(paste(ages_Locations$Year,ages_Locations$Month,ages_Locations$Day,sep="-"))

#Drawing out the Sand Lance to PREDICT their ages (rounded down to the whole number) from sizes
#Uses the vonB from literature
#Northern Sand Lance VonB parameters--note that they're in mm, so convert LENGTH to mm first
NSLli<-249
NSLk<-0.26
NSLt0<--1.10

#von Bertalanffy curves for the different species
vonB<-function(Li,k,t0,t) {
  l=Li*(1-exp(-1*k*(t-t0)))
}
#Inverse vonB function--predict AGE from LENGTH
iVonB<-function(Li,k,t0,L) {
  t=t0+(log(1-L/Li)/(-k))
}

#Fit the VonB to the blueback--only 6 aged in GoM so will use all Bluebacks in the dataset (119)
blueback<-nls(LENGTH ~ Li*(1-exp(-1*k*(AGE-t0))),
              data=subset(ages_Locations, Species=="BLUEBACK HERRING"),
              start=c(Li=50, k=0.26, t0=-1.10),
              na.action=na.pass)
bluebackVB<-data.frame(Species="BLUEBACK HERRING",Li=coef(blueback)[1],k=coef(blueback)[2],
                       t0=coef(blueback)[3],Freq=c(106,13),season=c("SPRING","FALL"))


#Now apply this to the sand lance measurements in the trawl data
sandlance<-subset(size_loc_date,Species=="NORTHERN SAND LANCE")%>%
  dplyr::select(-Sex)%>%
  mutate(AGE=floor(iVonB(NSLli,NSLk,NSLt0,(LENGTH*10))),
         season=toupper(season))
sandlance[is.nan(sandlance)]<-max(sandlance$AGE,na.rm=T)
sandlance$AGE<-ifelse(sandlance$AGE<0,0,sandlance$AGE)
#Merge it with the catchLocations to get the additional data ages_locations has
sandlance<-left_join(sandlance,catchLocations)
#Bind it back in with the rest of the size-age data
ages_GoM<-bind_rows(sandlance,ages_GoM)

ages_GoM$minTime<-ifelse(ages_GoM$season=="FALL",(yday(ages_GoM$Date)-219)/366,(168-yday(ages_GoM$Date))/366)
ages_GoM$minTime<-ifelse(ages_GoM$minTime<0,0,ages_GoM$minTime)
ages_GoM$maxTime<-ifelse(ages_GoM$season=="FALL",(yday(ages_GoM$Date)-168)/366,(219-yday(ages_GoM$Date))/366)



#Adding Sand Lance to the VB Data frame for this next step
sandlanceVB<-data.frame(Species=c("NORTHERN SAND LANCE","NORTHERN SAND LANCE"),Li=c(NSLli,NSLli),
                        k=c(NSLk,NSLk),t0=c(NSLt0,NSLt0),Freq=c(13109,13109),season=c("SPRING","FALL"))
VB_wsandlance<-bind_rows(VB,bluebackVB,sandlanceVB)

#estimating all ages to compare the spring and fall growth estimates (except sand lance and blueback that only have one)
VBages_growth<-data.frame(Species=sort(rep(VB_wsandlance$Species,11)),season=c(rep("SPRING",11),rep("FALL",11)),Age=seq(0:10)-1)%>%
  left_join(VB_wsandlance)%>%
  mutate(Length=ifelse(Species=="NORTHERN SAND LANCE",vonB(Li,k,t0,Age)/10,vonB(Li,k,t0,Age)),
         season=factor(season,levels=c("SPRING","FALL")))

ggplot()+
 # geom_jitter(data=ages_Locations,aes(AGE,LENGTH,fill=season),shape=21,size=2,alpha=0.2,width=0.3,height=0)+
  geom_line(data=VBages_growth,aes(Age,Length,color=season),lwd=2,alpha=0.7)+
  facet_wrap(~Species,scales="free_y")+
  scale_x_continuous(limits=c(0,10))+
  scale_color_brewer(palette="Set1",name="Trawl Season")+
  scale_fill_brewer(palette="Set1",name="Trawl Season")


ages_preds<-dplyr::select(ages_GoM,ID,Year,season,Date,LENGTH,Species,Family,AGE,minTime,maxTime,AREA,LONG,LAT)%>%
  left_join(VB_wsandlance)

ages_preds<-ages_preds%>%
  mutate(minSize=ifelse(season=="FALL",
                            ifelse(Species=="NORTHERN SAND LANCE",
                                   LENGTH-((vonB(Li,k,t0,AGE)/10)-(vonB(Li,k,t0,AGE-maxTime)/10)),
                                   LENGTH-(vonB(Li,k,t0,AGE)-vonB(Li,k,t0,AGE-maxTime))),
                            ifelse(Species=="NORTHERN SAND LANCE",
                                   LENGTH+((vonB(Li,k,t0,AGE+minTime)/10)-(vonB(Li,k,t0,AGE)/10)),
                                   LENGTH+(vonB(Li,k,t0,AGE+minTime)-vonB(Li,k,t0,AGE)))),
                            
         maxSize=ifelse(season=="FALL",
                            ifelse(Species=="NORTHERN SAND LANCE",
                                   LENGTH-((vonB(Li,k,t0,AGE)/10)-(vonB(Li,k,t0,AGE-minTime)/10)),
                                   LENGTH-(vonB(Li,k,t0,AGE)-vonB(Li,k,t0,AGE-minTime))),
                            ifelse(Species=="NORTHERN SAND LANCE",
                                   LENGTH+((vonB(Li,k,t0,AGE+maxTime)/10)-(vonB(Li,k,t0,AGE)/10)),
                                   LENGTH+(vonB(Li,k,t0,AGE+maxTime)-vonB(Li,k,t0,AGE)))))
                            





ggplot(subset(ages_preds,LENGTH<25))+
  geom_abline(slope=1,intercept=0,lwd=1.25)+
  geom_segment(aes(x=LENGTH,xend=LENGTH,y=minSize,yend=maxSize,color=season),lwd=0.75)+
  geom_point(aes(LENGTH,minSize,fill=season),shape=21,size=4)+
  geom_point(aes(LENGTH,maxSize,fill=season),shape=21,size=4)+
  facet_wrap(~Species)+
  scale_fill_brewer(name="Season",palette="Set1")+
  scale_color_brewer(name="Season",palette="Set1")+
  scale_x_continuous(name="Length in Trawl (cm)")+
  scale_y_continuous(name="Length in Summer (cm)")







# Tern Data ---------------------------------------------------------------

terncatch<-read_csv("terncatch.csv",col_types = cols(Date=col_date(format="%m/%d/%Y")))

######
#Assuming an equal catch range across all years--that range is set by tern ability/choice consistently
ternRanges<-terncatch%>%
  mutate(Family=toupper(Species))%>%
  group_by(Family)%>%
  summarise(minTern=min(Length,na.rm=T),
            maxTern=max(Length,na.rm=T))
ternRanges$Family<-ifelse(ternRanges$Family=="SANDLANCE","SAND LANCE",ternRanges$Family)


#What age is the min and max tern sizes
#What size is the minimum possible by making that fish younger (age of min size - last tern day - first spring trawl day)
#what size is the max possible for a fish that is younger (age of max size - first tern day - last spring trawl day)
#What size is the min possible for an older fish (age of min size + first fall trawl day - last tern day)
#What size is the max possible for older fish (age of max size + last trawl day - first tern day)
#Then you can take this range directly to the length survey samples

VB_wsandlance$Family<-sapply(VB_wsandlance$Species,spNMFSCats)
ternRanges_VB<-right_join(ternRanges,VB_wsandlance)
#first day of tern catch (06-16; 168) and last day of tern catch (08-06; 219)
springTrawls<-sort(unique(yday(filter(GoMTrawl_Sizes,season=="Spring")$Date)))
fallTrawls<-sort(unique(yday(filter(GoMTrawl_Sizes,season=="Fall")$Date)))
ternRanges_VB<-ternRanges_VB%>%
  mutate(minAge=ifelse(Species=="NORTHERN SAND LANCE",iVonB(Li,k,t0,(minTern*10)),iVonB(Li,k,t0,minTern)),
         maxAge=ifelse(Species=="NORTHERN SAND LANCE",iVonB(Li,k,t0,(maxTern*10)),iVonB(Li,k,t0,maxTern)),
         minTrawl=ifelse(season=="SPRING",vonB(Li,k,t0,minAge-((219-springTrawls[1])/366)),
                         vonB(Li,k,t0,minAge+((fallTrawls[1]-219)/366))),
         maxTrawl=ifelse(season=="SPRING",vonB(Li,k,t0,maxAge-((168-springTrawls[length(springTrawls)-1])/366)),
                         vonB(Li,k,t0,maxAge+(((fallTrawls[length(fallTrawls)]-168)/366)))),
         minTrawl=ifelse(Species=="NORTHERN SAND LANCE",minTrawl/10,minTrawl),
         maxTrawl=ifelse(Species=="NORTHERN SAND LANCE",maxTrawl/10,maxTrawl))

GoM_ternRanges<-GoMTrawl_Sizes%>%
  mutate(season=toupper(season))%>%
  left_join(ternRanges_VB)%>%
  mutate(ternable=ifelse(LENGTH<minTrawl,"Too Small",ifelse(LENGTH>maxTrawl,"Too Big","Catchable Size")))

table(GoM_ternRanges$Species,GoM_ternRanges$ternable)



#Just keep the proportion of the "tern catchable" fish for each season-year's species catch
#Which I'm not sure how I would plot that (there are too many factors (season, year, and species))
propsCatchable<-GoM_ternRanges%>%
  group_by(season,Year,Species)%>%
  mutate(ssy_Count=n())%>%
  group_by(season,Year,Species,ternable,ssy_Count)%>%
  summarise(Size_Prop=n()/ssy_Count)%>%distinct()%>%
  mutate(nSize_Prop=ifelse((ternable=="Too Big"|ternable=="Too Small"|is.na(ternable))&Size_Prop==1,0,Size_Prop),
         nternable=ifelse(nSize_Prop==Size_Prop,ternable,"Catchable Size"))
propsCatchable$ternable<-factor(propsCatchable$ternable,levels=c("Too Big","Catchable Size","Too Small"))

#Take that catchable prop out of the absolute and relative catches
GoM_catchable<-left_join(GoM_all,propsCatchable)%>%
  filter(nternable=="Catchable Size")%>%
  mutate(propMeasured=ssy_Count/species_Num,
         trawlCatch_catchableNum=nSize_Prop*species_Num)%>%
  dplyr::select(Year,season,Trawls,Trawls_Efforts,Family,Species,trawlCatch_Num=species_Num,trawlMeasured_Num=ssy_Count,
                propMeasured,propMeasured_catchable=nSize_Prop,trawlCatch_catchableNum)%>%
  mutate(trawlCatch_catchablePerTrawl=trawlCatch_catchableNum/Trawls,
         trawlCatch_catchablePerEffort=trawlCatch_catchableNum/Trawls_Efforts)



#write.csv(GoM_catchable,"GoM_catchableNum_1117.csv",row.names = F)


#Way to see where missing rows might be--not an issue if done above correctly
gom<-paste(GoM_catchable$Year,GoM_catchable$season,GoM_catchable$Species)
all<-paste(GoM_all$Year,GoM_all$season,GoM_all$Species)
all[which(all%notin%gom)]



#And the buffer--50 km
white<-c(42.966,-70.625)

buffer_ternRanges<-GoMTrawl_Sizes%>%
  mutate(season=toupper(season),
         distance_km=distance(white[1],LAT,white[2],LONG))%>%
  filter(distance_km<50)%>%
  left_join(ternRanges_VB)%>%
  mutate(ternable=ifelse(LENGTH<minTrawl,"Too Small",ifelse(LENGTH>maxTrawl,"Too Big","Catchable Size")))

table(buffer_ternRanges$Species,buffer_ternRanges$ternable)



#Just keep the proportion of the "tern catchable" fish for each season-year's species catch
#Which I'm not sure how I would plot that (there are too many factors (season, year, and species))
buffer_propsCatchable<-buffer_ternRanges%>%
  group_by(season,Year,Species)%>%
  mutate(ssy_Count=n())%>%
  group_by(season,Year,Species,ternable,ssy_Count)%>%
  summarise(Size_Prop=n()/ssy_Count)%>%distinct()%>%
  mutate(nSize_Prop=ifelse((ternable=="Too Big"|ternable=="Too Small"|is.na(ternable))&Size_Prop==1,0,Size_Prop),
         nternable=ifelse(nSize_Prop==Size_Prop,ternable,"Catchable Size"))


#Take that catchable prop out of the absolute and relative catches
buffer_catchable<-left_join(buffer_all,buffer_propsCatchable)%>%
  filter(nternable=="Catchable Size")%>%
  mutate(propMeasured=ssy_Count/species_Num,
         trawlCatch_catchableNum=nSize_Prop*species_Num)%>%
  dplyr::select(Year,season,Trawls,Trawls_Efforts,Family,Species,trawlCatch_Num=species_Num,trawlMeasured_Num=ssy_Count,
                propMeasured,propMeasured_catchable=nSize_Prop,trawlCatch_catchableNum)%>%
  mutate(trawlCatch_catchablePerTrawl=trawlCatch_catchableNum/Trawls,
         trawlCatch_catchablePerEffort=trawlCatch_catchableNum/Trawls_Efforts)



#write.csv(buffer_catchable,"buffer_catchableNum_1117.csv",row.names = F)




# Correlations of species -------------------------------------------------

GoM_allCatch_tern<-left_join(GoM_Seabirds,filter(propsCatchable,nternable=="Catchable Size"))%>%
  mutate(ternNum=Count*nSize_Prop,STRATUM=as.character(STRATUM),STATION=as.character(STATION),
         ID=paste0(CRUISE6,STRATUM,STATION),
         Date_and_Time=paste(paste(Year,Month,Day,sep="-"),Time),Date_and_Time=ymd_hms(Date_and_Time))%>%
  dplyr::select(ID,Year,season,Date_and_Time,AREA,LONG,LAT,Species,ternNum)


GoM_allCatch_tern.MV<-pivot_wider(GoM_allCatch_tern,id_cols=c("ID","Year","season","Date_and_Time","AREA","LONG","LAT"),
                                  names_from="Species",values_from="ternNum")
GoM_allCatch_tern.MV[is.na(GoM_allCatch_tern.MV)]<-0
colnames(GoM_allCatch_tern.MV)<-gsub(" ","_",colnames(GoM_allCatch_tern.MV))

plot(GoM_allCatch_tern.MV[,4:ncol(GoM_allCatch_tern.MV)])
library(corrplot)
GoMCatch_cor<-cor(GoM_allCatch_tern.MV[,4:ncol(GoM_allCatch_tern.MV)],method="kendall")
corrplot(GoMCatch_cor,method="number")

library(vegan)
library(moments)

#Trying first with the 2017 counts
GoMCatch_mat0<-as.matrix(GoM_allCatch_tern.MV[,8:ncol(GoM_allCatch_tern.MV)])
GoMCatch_Env.Mat0<-GoM_allCatch_tern.MV[,1:7]

#Remove rows (trawls) that are empty (caught none of these species)
GoMCatch_mat<-GoMCatch_mat0[-which(apply(GoMCatch_mat0, 1, sum)==0),]
GoMCatch_Env.Mat<-GoMCatch_Env.Mat0[-which(apply(GoMCatch_mat0,1,sum)==0),]

tGoMCatch_mat<-t(GoMCatch_mat)

#### Conduct a row and column summary on your Main matrix.  Look at row (plots) and column (soil variables) skewness and kurtosis, coefficient of variation (CV) of totals for rows and columns, empty rows, and % of zeros in the dataset.
#make sure the moments package is loaded
apply(GoMCatch_mat, 1, skewness)
apply(GoMCatch_mat, 1, kurtosis)
GoMCatch_mat.rowmean<-apply(GoMCatch_mat, 1, mean)
GoMCatch_mat.rowSD<-apply(GoMCatch_mat, 1, sd)
GoMCatch_mat.COV<-GoMCatch_mat.rowSD/GoMCatch_mat.rowmean

apply(GoMCatch_mat, 2, skewness)
apply(GoMCatch_mat, 2, kurtosis)
GoMCatch_mat.colmean<-apply(GoMCatch_mat, 2, mean)
GoMCatch_mat.colSD<-apply(GoMCatch_mat, 2, sd)
GoMCatch_mat.COV<-GoMCatch_mat.colSD/GoMCatch_mat.colmean


###calculating percent of nonzeros
dim(GoMCatch_mat)
GoMCatch_mat.pNzero<-100*(nnzero(GoMCatch_mat)/(11304*10))
##so this is the percent zeros
GoMCatch_mat.pzero<-100-GoMCatch_mat.pNzero

#Percent of trawls that have each species, all are greater than 5%
GoMCatch_mat.pres<-apply(GoMCatch_mat > 0, 2, sum)
GoMCatch_mat.Matp<-100*GoMCatch_mat.pres/nrow(GoMCatch_mat)

#For the all diets NMDS
original.dist<-vegdist(tGoMCatch_mat)
stress_values<-numeric(6)
r2<-numeric(6)

for (n in 1:6) {
  nmds.resu <- metaMDS(tGoMCatch_mat, k=n, distance = "bray", try=250, autotransform=F)
  stress_values[n]<-nmds.resu$stress*100
  nmds.scores<-scores(nmds.resu)
  nmds.dist<-dist(nmds.scores)
  r2[n]<-summary(lm(original.dist~nmds.dist))[[8]]
}
plot(stress_values, xlab="Number of axes", ylab="Stress",type="b")
abline(h=20,col="red")

View(stress_values) 

#Go back and create the output for the 2 dimensions NMDS
GoMCatch_NMDS<-metaMDS(tGoMCatch_mat, distance = "bray", k = 2, try=250, autotransform=F)
r2<-summary(lm(original.dist~dist(vegan::scores(GoMCatch_NMDS))))[[8]]
actualStress<-GoMCatch_NMDS$stress
stressplot(GoMCatch_NMDS) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions

write.csv(tGoMCatch_mat,"tGoMCatch_mat.csv",row.names = T)

#Ordination plots for the NMDS 2D results
allNMDS_trawls<-as.data.frame(GoMCatch_NMDS$species)
allNMDS_species<-as.data.frame(GoMCatch_NMDS$points)
allNMDS_env<-as.data.frame(GoMCatch_Env.Mat)

#Technically the NMDS that I ran--slow and doesn't make much sense at all
ggplot(data=allNMDS_species,aes(MDS1,MDS2))+
  geom_point()+
  xlab("Axis 1")+ylab("Axis 2")+
  geom_segment(data=allNMDS_trawls,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ 
  geom_label(data=allNMDS_trawls,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_trawls))),size=2)+
  scale_x_continuous(limits=c(-0.61,0.53))+scale_y_continuous(limits=c(-0.2,0.3))



#The way it SHOULD look--keep playing with overlays and eventually MRPPs of those overlays
ggplot(data=allNMDS_trawls,aes(MDS1,MDS2))+
  geom_segment(data=allNMDS_species,aes(x=0,y=0,xend=MDS1,yend=MDS2),color="grey50",lwd=1.5,alpha=0.7)+ 
  geom_label(data=allNMDS_species,aes(x=MDS1,y=MDS2,label=gsub("_","\n",rownames(allNMDS_species))),size=8)+
  geom_point(aes(fill=allNMDS_env$AREA),shape=21,size=2,alpha=0.5)+
  stat_ellipse(aes(color=allNMDS_env$AREA),lwd=1.1)+
  #geom_text(aes(label=allNMDS_env$ID),hjust=0,vjust=0,size=2)+
  xlab("Axis 1")+ylab("Axis 2")+
  scale_x_continuous(limits=c(-0.61,0.53))+
  scale_y_continuous(limits=c(-0.301,0.3))+
  #scale_fill_brewer(palette = "Set1",name="AREA")+
  #scale_color_brewer(palette = "Set1",name="AREA")+
  geom_text(aes(-0.49,0.27,label=paste("Stress =",round(actualStress,digits=4)*100)),size=10,fontface="bold")






























#After this is the older way of doing it: where the aged fish had time added to that age and that new age was estimated for length
#This requires an extra step to be able to find the props in the fishes that just got lengths (props of the aged fish)

fisheryRanges<-left_join(ages_preds,ternRanges)%>%
  dplyr::select(Family,season,Species,Year,LENGTH,AGE,Date,AREA,LONG,LAT,minSize:maxTern)%>%
  mutate(ternable=ifelse((minSize>=minTern & minSize<=maxTern) | (maxSize>=minTern & maxSize<=maxTern),
                         "Catchable Size",ifelse(maxSize<minTern,"Too Small","Too Big")))


#Crop the fishery Ranges down to the GoM area, roughly?
GoM_fisheryRanges<-filter(fisheryRanges,AREA<523)

#How many individuals existed in each of the three categories (small, big, and juuuuuuust right)
table(GoM_fisheryRanges$Species,GoM_fisheryRanges$ternable)

#summaries of the LENGTHs (from the trawls) in each size "catch" group using the full dataset of aged fish
lengthRanges<-GoM_fisheryRanges%>%
  group_by(Species,season,ternable)%>%
  summarise(meanLength=mean(LENGTH),minLength=min(LENGTH),maxLength=max(LENGTH),N=n())%>%
  subset(ternable=="Catchable Size")%>%
  #Changing the alewife min to 6cm in the fall, just like atlantic and blueback herring (there were none caught that small)
  mutate(minLength=ifelse(Species=="ALEWIFE"&season=="FALL",6,minLength))


#How many (and what proportion) of individuals for each species were in each category
speciesRanges<-GoM_fisheryRanges%>%
  group_by(Species)%>%
  mutate(Species_Count=n())%>%
  group_by(ternable,Species)%>%
  mutate(Size_Prop=n()/Species_Count)%>%
  dplyr::select(Species,ternable,Species_Count,Size_Prop)%>%unique()%>%
  arrange(Species,ternable)
speciesRanges[is.na(speciesRanges)]<-"No Measure"
speciesRanges$ternable<-factor(speciesRanges$ternable,levels=c("No Measure","Too Big","Catchable Size","Too Small"))
speciesRanges$pSpecies<-gsub(" ","\n",speciesRanges$Species)

ggplot(speciesRanges)+
  geom_col(aes(pSpecies,Size_Prop,fill=ternable,color=ternable),size=1)+
  scale_fill_manual(values=c(rgb(0.3,0.4,0.4,alpha=0),"red3","forestgreen","royalblue4"),
                    name="Size during\nTern Summer",labels=c("","Too Big","Catchable Size","Too Small"))+
  scale_color_manual(values=c(rgb(0.43,0.4,0.3,alpha=0),"black","black","black"))+
  scale_y_continuous(expand=c(0,0.005),name="Proportion of Individuals\nCaught in Trawls")+
  xlab("Species")+
  theme(axis.text.x=element_text(size=20,angle=0,color="black",vjust=0.6),
        legend.text=element_text(size=30),legend.title=element_text(size=35))+
  guides(color=F)


yearRanges<-fisheryRanges%>%
  group_by(Year)%>%
  mutate(Year_Count=n())%>%
  group_by(ternable,Year)%>%
  mutate(Size_Prop=n()/Year_Count)%>%
  dplyr::select(Year,ternable,Year_Count,Size_Prop)%>%unique()%>%
  arrange(Year,ternable)
yearRanges[is.na(yearRanges)]<-"No Measure"
yearRanges$ternable<-factor(yearRanges$ternable,levels=c("No Measure","Too Big","Catchable Size","Too Small"))

ggplot(yearRanges)+
  geom_col(aes(Year,Size_Prop,fill=ternable,color=ternable),size=1)+
  scale_fill_manual(values=c(rgb(0.3,0.4,0.4,alpha=0),"red3","forestgreen","royalblue4"),
                    name="Size during\nTern Summer",labels=c("","Too Big","Catchable Size","Too Small"))+
  scale_color_manual(values=c(rgb(0.43,0.4,0.3,alpha=0),"black","black","black"))+
  scale_y_continuous(expand=c(0,0.005),name="Proportion of Individuals\nCaught in Trawls")+
  scale_x_continuous(expand=c(0,1),name="Year")+
  theme(axis.text.x=element_text(size=20,angle=0,color="black",vjust=0.6),
        legend.text=element_text(size=30),legend.title=element_text(size=35))+
  guides(color=F)


spyrRanges<-fisheryRanges%>%
  group_by(Year,Species)%>%
  mutate(SpYr_Count=n())%>%
  group_by(ternable,Year,Species)%>%
  mutate(Size_Prop=n()/SpYr_Count)%>%
  dplyr::select(Species,Year,ternable,SpYr_Count,Size_Prop)%>%unique()%>%
  arrange(Year,Species,ternable)
spyrRanges[is.na(spyrRanges)]<-"No Measure"
spyrRanges$ternable<-factor(spyrRanges$ternable,levels=c("No Measure","Too Big","Catchable Size","Too Small"))

ggplot(spyrRanges)+
  geom_col(aes(Year,Size_Prop,fill=ternable,color=ternable),size=1)+
  facet_wrap(~Species)+
  scale_fill_manual(values=c(rgb(0.3,0.4,0.4,alpha=0),"red3","forestgreen","royalblue4"),
                    name="Size during\nTern Summer",labels=c("","Too Big","Catchable Size","Too Small"))+
  scale_color_manual(values=c(rgb(0.43,0.4,0.3,alpha=0),"black","black","black"))+
  scale_y_continuous(expand=c(0,0.005),name="Proportion of Individuals\nCaught in Trawls")+
  scale_x_continuous(expand=c(0,1),name="Year")+
  theme(axis.text.x=element_text(size=20,angle=0,color="black",vjust=0.6),
        legend.text=element_text(size=30),legend.title=element_text(size=35))+
  guides(color=F)




#Merge these LENGTH ranges with the broader trawl length measures to get those "catch" groups
allTrawl_SizeGroups<-allTrawl_Sizes%>%
  dplyr::select(Year,season,LENGTH:Family)%>%
  mutate(season=toupper(season),Year=as.numeric(Year))
GoMTrawl_SizeGroups<-left_join(allTrawl_SizeGroups,lengthRanges[,c(1,2,5,6)])%>%
  filter(AREA<523)%>%
  mutate(ternable=ifelse((season=="FALL" & (LENGTH>=minLength & LENGTH<=maxLength)) |  #If a fall catch is between the min and max length
                           (season=="SPRING" & LENGTH<=maxLength), #By rule, no fish caught in the spring will be "Too Small"
                         "Catchable Size",ifelse(LENGTH<minLength,"Too Small","Too Big")))
GoMTrawl_lengthRanges<-GoMTrawl_SizeGroups%>%
  ungroup()%>%
  group_by(Species,season,ternable)%>%
  summarise(meanLength=mean(LENGTH),minLength=min(LENGTH),maxLength=max(LENGTH),N=n())


table(GoMTrawl_SizeGroups$Species,GoMTrawl_SizeGroups$ternable)

  

#How many (and what proportion) of individuals for each species were in each category
all_speciesRanges<-GoM_sizeGroups%>%
  group_by(Species)%>%
  mutate(Species_Count=n())%>%
  group_by(ternable,Species)%>%
  mutate(Size_Prop=n()/Species_Count)%>%
  dplyr::select(Species,ternable,Species_Count,Size_Prop)%>%unique()%>%
  arrange(Species,ternable)
all_speciesRanges$ternable<-factor(all_speciesRanges$ternable,levels=c("Too Big","Catchable Size","Too Small"))
all_speciesRanges$pSpecies<-gsub(" ","\n",all_speciesRanges$Species)

ggplot(all_speciesRanges)+
  geom_col(aes(pSpecies,Size_Prop,fill=ternable),color="black",size=1)+
  scale_fill_manual(values=c("red3","forestgreen","royalblue4"),
                    name="Size during\nTern Summer",labels=c("Too Big","Catchable Size","Too Small"))+
  scale_y_continuous(expand=c(0,0.005),name="Proportion of Individuals\nCaught in Trawls")+
  xlab("Species")+
  theme(axis.text.x=element_text(size=20,angle=0,color="black",vjust=0.6),
        legend.text=element_text(size=30),legend.title=element_text(size=35))+
  guides(color=F)


all_yearRanges<-allTrawl_SizeGroups%>%
  group_by(Year)%>%
  mutate(Year_Count=n())%>%
  group_by(ternable,Year)%>%
  mutate(Size_Prop=n()/Year_Count)%>%
  dplyr::select(Year,ternable,Year_Count,Size_Prop)%>%unique()%>%
  arrange(Year,ternable)
all_yearRanges$ternable<-factor(all_yearRanges$ternable,levels=c("Too Big","Catchable Size","Too Small"))

ggplot(all_yearRanges)+
  geom_col(aes(Year,Size_Prop,fill=ternable),color="black",size=1)+
  scale_fill_manual(values=c("red3","forestgreen","royalblue4"),
                    name="Size during\nTern Summer",labels=c("Too Big","Catchable Size","Too Small"))+
  scale_y_continuous(expand=c(0,0.005),name="Proportion of Individuals\nCaught in Trawls")+
  scale_x_continuous(expand=c(0,1),name="Year")+
  theme(axis.text.x=element_text(size=20,angle=0,color="black",vjust=0.6),
        legend.text=element_text(size=30),legend.title=element_text(size=35))+
  guides(color=F)


all_spyrRanges<-allTrawl_SizeGroups%>%
  group_by(Year,Species)%>%
  mutate(SpYr_Count=n())%>%
  group_by(ternable,Year,Species)%>%
  mutate(Size_Prop=n()/SpYr_Count)%>%
  dplyr::select(Species,Year,ternable,SpYr_Count,Size_Prop)%>%unique()%>%
  arrange(Year,Species,ternable)
all_spyrRanges$ternable<-factor(all_spyrRanges$ternable,levels=c("Too Big","Catchable Size","Too Small"))

ggplot(all_spyrRanges)+
  geom_col(aes(Year,Size_Prop,fill=ternable),color="black",size=1)+
  facet_wrap(~Species)+
  scale_fill_manual(values=c("red3","forestgreen","royalblue4"),
                    name="Size during\nTern Summer",labels=c("Too Big","Catchable Size","Too Small"))+
  scale_y_continuous(expand=c(0,0.005),name="Proportion of Individuals\nCaught in Trawls")+
  scale_x_continuous(expand=c(0,1),name="Year")+
  theme(axis.text.x=element_text(size=20,angle=0,color="black",vjust=0.6),
        legend.text=element_text(size=30),legend.title=element_text(size=35))+
  guides(color=F)


all_seasonRanges<-GoM_sizeGroups%>%
  group_by(season)%>%
  mutate(Season_Count=n())%>%
  group_by(ternable,season)%>%
  mutate(Size_Prop=n()/Season_Count)%>%
  dplyr::select(season,ternable,Season_Count,Size_Prop)%>%unique()%>%
  arrange(season,ternable)
all_seasonRanges$ternable<-factor(all_seasonRanges$ternable,levels=c("Too Big","Catchable Size","Too Small"))

ggplot(all_seasonRanges)+
  geom_col(aes(season,Size_Prop,fill=ternable),color="black",size=1)+
  scale_fill_manual(values=c("red3","forestgreen","royalblue4"),
                    name="Size during\nTern Summer",labels=c("Too Big","Catchable Size","Too Small"))+
  scale_y_continuous(expand=c(0,0.005),name="Proportion of Individuals\nCaught in Trawls")+
  scale_x_discrete(expand=c(0,0.5),name="Season")+
  theme(axis.text.x=element_text(size=20,angle=0,color="black",vjust=0.6),
        legend.text=element_text(size=30),legend.title=element_text(size=35))+
  guides(color=F)



#Just keep the proportion of the "tern catchable" fish for each season-year's species catch
#Which I'm not sure how I would plot that (there are too many factors (season, year, and species))
all_ssyRanges<-GoM_sizeGroups%>%
  group_by(season,Year,Species)%>%
  mutate(ssy_Count=n())%>%
  group_by(ternable,season,Year,Species)%>%
  mutate(Size_Prop=n()/ssy_Count)%>%
  dplyr::select(Year,season,Species,ternable,ssy_Count,Size_Prop)%>%unique()%>%
  arrange(Year,season,Species,ternable)%>%
  ungroup()
all_ssyRanges$ternable<-factor(all_ssyRanges$ternable,levels=c("Too Big","Catchable Size","Too Small"))

ssy_catchable<-filter(all_ssyRanges,ternable=="Catchable Size")%>%
  dplyr::select(Year,season,Species,propCatchable=Size_Prop)

#Arrange to look at for season effects
ssy_look<-arrange(ssy_catchable,Species,Year,season)
ssy_look$season<-factor(ssy_look$season,levels=c("SPRING","FALL"))
ggplot(ssy_look)+
  geom_boxplot(aes(season,propCatchable,fill=season),color="black",position=position_dodge(),show.legend=F)+
  facet_wrap(~Species)


#Sizes over time for each of the species
GoM_preds<-filter(ages_preds,AREA<523)
ggplot(GoM_preds,aes(LONG,LAT))+geom_point()

ggplot(GoM_preds)+
  geom_point(aes(yday(Date),LENGTH,fill=Species),shape=21,size=4,show.legend = F)+
  facet_wrap(~Species)

#Keeping only the smallest size for each day
smallest<-GoM_preds%>%
  group_by(Species,Date=yday(Date))%>%
  summarise(minLength=min(LENGTH))
ggplot(smallest)+
  geom_point(aes(Date,minLength,fill=Species),shape=21,size=4,show.legend = F)+
  facet_wrap(~Species)




#Take that catchable prop out of the absolute and relative catches
GoM_catchable<-merge(GoM_all,ssy_catchable,all.x=T)
#If there isn't a catchable prop for a species in a season-year, then use fill with the prop from previous year
#Then
#ADD A MUTATE TO HERE FOR EVERY VARIABLE
#For species, just divide catches by propCatchable
#For family, group by family and divide catches by mean(propCatchable)
#For total, group by season-year and divide Total by mean(propCatchable)
#Then recalculate the species and family proportions of that total (do numerical totals and props too)

GoM_catchable<-left_join(GoM_catchable,DNA)%>%
  arrange(Species,Year,season)%>%
  tidyr::fill(propCatchable,.direction="downup")%>%
  mutate(catchable_Wt=species_Wt, 
         catchable_Num=species_Num,
         catchable_RelWt=catchable_Wt/Trawls,
         catchable_RelNum=catchable_Num/Trawls,
         catchable_ERelWt=catchable_Wt/Trawls_Efforts,
         catchable_ERelNum=catchable_Num/Trawls_Efforts,
         catchable_DNAweight_Wt=species_DNAweight_Wt,     #Calculating all the same but using weighted species abundances
         catchable_DNAweight_Num=species_DNAweight_Num,
         catchable_DNAweight_RelWt=catchable_DNAweight_Wt/Trawls,
         catchable_DNAweight_RelNum=catchable_DNAweight_Num/Trawls,
         catchable_DNAweight_ERelWt=catchable_DNAweight_Wt/Trawls_Efforts,
         catchable_DNAweight_ERelNum=catchable_DNAweight_Num/Trawls_Efforts)%>%
  group_by(Year,season)%>%
  mutate(catchableTotal_Wt=sum(catchable_Wt),
         catchableTotal_Num=sum(catchable_Num),
         catchableTotal_DNAweight_Wt=sum(catchable_DNAweight_Wt),
         catchableTotal_DNAweight_Num=sum(catchable_DNAweight_Num))%>%
  ungroup()%>%
  group_by(Year,season,Family)%>%
  mutate(famcatchable_Wt=sum(catchable_Wt),
         famcatchable_Num=sum(catchable_Num),
         famcatchable_RelWt=sum(catchable_RelWt),
         famcatchable_RelNum=sum(catchable_RelNum),
         famcatchable_ERelWt=sum(catchable_ERelWt),
         famcatchable_ERelNum=sum(catchable_ERelNum),
         famcatchableProp_Wt=famcatchable_Wt/catchableTotal_Wt,
         famcatchableProp_Num=famcatchable_Num/catchableTotal_Num,
         famcatchable_DNAweight_Wt=sum(catchable_DNAweight_Wt),
         famcatchable_DNAweight_Num=sum(catchable_DNAweight_Num),
         famcatchable_DNAweight_RelWt=sum(catchable_DNAweight_RelWt),
         famcatchable_DNAweight_RelNum=sum(catchable_DNAweight_RelNum),
         famcatchable_DNAweight_ERelWt=sum(catchable_DNAweight_ERelWt),
         famcatchable_DNAweight_ERelNum=sum(catchable_DNAweight_ERelNum),
         famcatchableProp_DNAweight_Wt=famcatchable_DNAweight_Wt/catchableTotal_DNAweight_Wt,
         famcatchableProp_DNAweight_Num=famcatchable_DNAweight_Num/catchableTotal_DNAweight_Num)%>%
  ungroup()%>%
  mutate(catchableProp_Wt=catchable_Wt/catchableTotal_Wt,
         catchableProp_Num=catchable_Num/catchableTotal_Num,
         catchableProp_DNAweight_Wt=catchable_DNAweight_Wt/catchableTotal_DNAweight_Wt,
         catchableProp_DNAweight_Num=catchable_DNAweight_Num/catchableTotal_DNAweight_Num)%>%
  arrange(Year,season,Species)
  


#Write out the data for Aliya's usage
write_GOM<-dplyr::select(GoM_catchable,c(1,2,4:8,11:14,25:30,49:54,57:62))%>%distinct()
#colnames(write_GOM)<-gsub("catchable_?","",colnames(write_GOM))


all<-tidyr::expand(write_GOM,Year,season,Family)%>%
  full_join(write_GOM)
all$Family<-sapply(all$Species,spNMFSCats)
all<-all%>%
  group_by(Year,season)%>%
  tidyr::fill(Trawls,Trawls_Efforts,.direction="downup")
all[is.na(all)]<-0
all<-filter(all,Trawls>0)

write.csv(all,file="GoM_fam_ternCatch_DNAweight.csv",row.names=F)



#Take that catchable prop out of the absolute and relative catches
buffer_catchable<-merge(buffer_all,ssy_catchable,all.x=T)
#If there isn't a catchable prop for a species in a season-year, then use fill with the prop from previous year
#Then
#ADD A MUTATE TO HERE FOR EVERY VARIABLE
#For species, just divide catches by propCatchable
#For family, group by family and divide catches by mean(propCatchable)
#For total, group by season-year and divide Total by mean(propCatchable)
#Then recalculate the species and family proportions of that total (do numerical totals and props too)

buffer_catchable<-buffer_catchable%>%
  arrange(Species,Year,season)%>%
  tidyr::fill(propCatchable,.direction="downup")%>%
  mutate(catchable_Wt=Catch_Wt*propCatchable,
         catchable_Num=Catch_Num*propCatchable,
         catchable_RelWt=Rel_Wt*propCatchable,
         catchable_RelNum=Rel_Num*propCatchable,
         catchable_ERelWt=ERel_Wt*propCatchable,
         catchable_ERelNum=ERel_Num*propCatchable)%>%
  group_by(Year,season)%>%
  mutate(catchableTotal_Wt=sum(catchable_Wt),
         catchableTotal_Num=sum(catchable_Num))%>%
  ungroup()%>%
  group_by(Year,season,Family)%>%
  mutate(famcatchable_Wt=sum(catchable_Wt),
         famcatchable_Num=sum(catchable_Num),
         famcatchable_RelWt=sum(catchable_RelWt),
         famcatchable_RelNum=sum(catchable_RelNum),
         famcatchable_ERelWt=sum(catchable_ERelWt),
         famcatchable_ERelNum=sum(catchable_ERelNum),
         famcatchableProp_Wt=famcatchable_Wt/catchableTotal_Wt,
         famcatchableProp_Num=famcatchable_Num/catchableTotal_Num)%>%
  ungroup()%>%
  mutate(catchableProp_Wt=catchable_Wt/catchableTotal_Wt,
         catchableProp_Num=catchable_Num/catchableTotal_Num)%>%
  arrange(Year,season,Species)

#With the DNA weights
buffer_catchable<-left_join(buffer_catchable,DNA)%>%
  arrange(Species,Year,season)%>%
  tidyr::fill(propCatchable,.direction="downup")%>%
  mutate(catchable_Wt=species_Wt, 
         catchable_Num=species_Num,
         catchable_RelWt=catchable_Wt/Trawls,
         catchable_RelNum=catchable_Num/Trawls,
         catchable_ERelWt=catchable_Wt/Trawls_Efforts,
         catchable_ERelNum=catchable_Num/Trawls_Efforts,
         catchable_DNAweight_Wt=species_DNAweight_Wt,     #Calculating all the same but using weighted species abundances
         catchable_DNAweight_Num=species_DNAweight_Num,
         catchable_DNAweight_RelWt=catchable_DNAweight_Wt/Trawls,
         catchable_DNAweight_RelNum=catchable_DNAweight_Num/Trawls,
         catchable_DNAweight_ERelWt=catchable_DNAweight_Wt/Trawls_Efforts,
         catchable_DNAweight_ERelNum=catchable_DNAweight_Num/Trawls_Efforts)%>%
  group_by(Year,season)%>%
  mutate(catchableTotal_Wt=sum(catchable_Wt),
         catchableTotal_Num=sum(catchable_Num),
         catchableTotal_DNAweight_Wt=sum(catchable_DNAweight_Wt),
         catchableTotal_DNAweight_Num=sum(catchable_DNAweight_Num))%>%
  ungroup()%>%
  group_by(Year,season,Family)%>%
  mutate(famcatchable_Wt=sum(catchable_Wt),
         famcatchable_Num=sum(catchable_Num),
         famcatchable_RelWt=sum(catchable_RelWt),
         famcatchable_RelNum=sum(catchable_RelNum),
         famcatchable_ERelWt=sum(catchable_ERelWt),
         famcatchable_ERelNum=sum(catchable_ERelNum),
         famcatchableProp_Wt=famcatchable_Wt/catchableTotal_Wt,
         famcatchableProp_Num=famcatchable_Num/catchableTotal_Num,
         famcatchable_DNAweight_Wt=sum(catchable_DNAweight_Wt),
         famcatchable_DNAweight_Num=sum(catchable_DNAweight_Num),
         famcatchable_DNAweight_RelWt=sum(catchable_DNAweight_RelWt),
         famcatchable_DNAweight_RelNum=sum(catchable_DNAweight_RelNum),
         famcatchable_DNAweight_ERelWt=sum(catchable_DNAweight_ERelWt),
         famcatchable_DNAweight_ERelNum=sum(catchable_DNAweight_ERelNum),
         famcatchableProp_DNAweight_Wt=famcatchable_DNAweight_Wt/catchableTotal_DNAweight_Wt,
         famcatchableProp_DNAweight_Num=famcatchable_DNAweight_Num/catchableTotal_DNAweight_Num)%>%
  ungroup()%>%
  mutate(catchableProp_Wt=catchable_Wt/catchableTotal_Wt,
         catchableProp_Num=catchable_Num/catchableTotal_Num,
         catchableProp_DNAweight_Wt=catchable_DNAweight_Wt/catchableTotal_DNAweight_Wt,
         catchableProp_DNAweight_Num=catchable_DNAweight_Num/catchableTotal_DNAweight_Num)%>%
  arrange(Year,season,Species)


#Write out the data for Aliya's usage
write_buffer<-dplyr::select(buffer_catchable,-c(7:23,32:39))
colnames(write_buffer)<-gsub("catchable_?","",colnames(write_buffer))
#Or with the family DNA weighting
write_buffer<-dplyr::select(buffer_catchable,c(1,2,4:6,27,28,9:12,29:34,53:58,61:66))%>%distinct()

all<-tidyr::expand(write_buffer,Year,season,Family)%>%
  full_join(write_buffer)
all$Family<-sapply(all$Species,spNMFSCats)
all<-all%>%
  group_by(Year,season)%>%
  tidyr::fill(Trawls,Trawls_Efforts,.direction="downup")
all[is.na(all)]<-0
all<-filter(all,Trawls>0)

write.csv(all,file="100km_fam_ternCatch_DNAweight.csv",row.names=F)

1.02*10^-5*107^3.19

# Plotting ----------------------------------------------------------------
library(stringr)
library(wesanderson)
famGoM_catchable<-dplyr::select(GoM_catchable,1,2,4,catchableTotal_Wt,catchableTotal_Num,Total_Wt,Total_Num,
                                grep("fam",colnames(GoM_catchable)))%>%unique()

ggplot(subset(famGoM_catchable,Year>1998))+
  geom_col(aes(x=Year,y=famcatchableProp_Wt,fill=Family))+
  geom_point(aes(x=Year,y=catchableTotal_Wt/max(catchableTotal_Wt)),alpha=0.6)+
  geom_line(aes(x=Year,y=catchableTotal_Wt/max(catchableTotal_Wt)),alpha=0.6)+
  facet_wrap(~season,nrow=2)+
  scale_fill_manual(values=wes_palette("Darjeeling1",6,"continuous"))+
  scale_y_continuous(expand=c(0,0.005),
                     sec.axis=sec_axis(~.*max(famGoM_catchable$catchableTotal_Wt),name="Total Catch (lbs)"))+
  scale_x_continuous(breaks=seq(min(famGoM_catchable$Year),
                                max(famGoM_catchable$Year),by=2),expand=c(0,0.1))+
  ylab("Proportion of Total Catch Weight")


#With number/count of catch--probably better since that's the unit of tern catch
ggplot(subset(famGoM_catchable,Year>1998))+
  geom_col(aes(x=Year,y=famcatchableProp_Num,fill=Family))+
  geom_point(aes(x=Year,y=catchableTotal_Num/max(catchableTotal_Num)),alpha=0.6)+
  geom_line(aes(x=Year,y=catchableTotal_Num/max(catchableTotal_Num)),alpha=0.6)+
  facet_wrap(~season,nrow=2)+
  scale_fill_manual(values=wes_palette("Darjeeling1",6,"continuous"))+
  scale_y_continuous(expand=c(0,0.005),
                     sec.axis=sec_axis(~.*max(famGoM_catchable$catchableTotal_Num),name="Total Catch Count"))+
  scale_x_continuous(breaks=seq(min(famGoM_catchable$Year),
                                max(famGoM_catchable$Year),by=2),expand=c(0,0.1))+
  ylab("Proportion of Total Catch Count")

write.csv(famGoM_catchable,"GoM_adjCatch_ternsized.csv",row.names=F)






#How I did it before...
#What proportion (in each year) are under the tern limit
buffer_ternable<-merge(dplyr::select(bufferSizes_plot,Year,season,Species=LOGGED_SPECIES_NAME,Family,LENGTH,predAge),
                       dplyr::select(maxTernLengths,Species,maxLength,maxAge),by="Species")%>%
  mutate(maxLength=as.numeric(maxLength),
         ternable_Length=ifelse(LENGTH<=maxLength,"Yes","No"),
         ternable_Age=ifelse(predAge<=maxAge,"Yes","No"),
         conflict=ifelse(ternable_Length==ternable_Age,"No","Yes"))

buffer_propTern<-buffer_ternable%>%
  group_by(Family)%>%
  mutate(total=n())%>%
  group_by(Family,ternable_Length)%>%
  mutate(ternTotal=n(),
         propTernable=ternTotal/total)%>%
  filter(ternable_Length=="Yes")%>%
  dplyr::select(Family,ternable_Length,ternTotal,total,propTernable)%>%unique()


#Calculate a different prop available in each year
buffer_propyearTern<-buffer_ternable%>%
  group_by(Family,Year)%>%
  mutate(total=n())%>%
  group_by(Family,Year,ternable_Length)%>%
  mutate(ternTotal=n(),
         propTernable=ternTotal/total)%>%
  filter(ternable_Length=="Yes")%>%
  dplyr::select(Family,Year,ternable_Length,ternTotal,total,propTernable)%>%unique()


#Reduce the all catch by the proportion in the tern wheelhouse
buffer_allTern<-merge(buffer_all,buffer_propyearTern[,c(1,2,6)],by=c("Year","Family"),all.x=T)
buffer_allTern<-merge(buffer_allTern,buffer_propTern[,c(1,5)],by="Family",all.x=T)
buffer_allTern<-buffer_allTern%>%
  mutate(propTernable=ifelse(Family=="MACKEREL",0,
                             ifelse(is.na(propTernable.x),propTernable.y,propTernable.x)),
         ternCatch_Wt=Catch_Wt*propTernable,ternCatch_Num=Catch_Num*propTernable,
         ternRel_Wt=ternCatch_Wt/Trawls,ternRel_Num=ternCatch_Num/Trawls,
         ternERel_Wt=ternCatch_Wt/Trawls_Efforts,ternERel_Num=ternCatch_Num/Trawls_Efforts)%>%
  group_by(Year,season)%>%
  mutate(ternTotal=sum(ternCatch_Wt))%>%
  ungroup()%>%
  mutate(ternProp=ternCatch_Wt/ternTotal,
         ternableTotal=ternTotal/Total)%>%
  dplyr::select(-c(propTernable.x,propTernable.y))











# Location of Catches -----------------------------------------------------
library(ggmap)
load("GoM_Map.R")
white<-c(42.966,-70.625)

#Every trawl (in the GoM area)
GoM_Seabirds
#The proportion of the catch (for a species) that could be eaten by terns (for that season-year)
ssy_catchable

#Put it all together, then have date, location, and distance from island data
GoM_tcatch_st<-merge(GoM_Seabirds,ssy_catchable,by=c("Year","season","Species"),all.x=T)%>%
  arrange(Species,Year,season)%>%
  tidyr::fill(propCatchable,.direction="downup")%>%
  mutate(catchable_Wt=Pounds*propCatchable,
         catchable_Num=Count*propCatchable,
         catchable_ERelWt=(Pounds/Effort)*propCatchable,
         catchable_ERelNum=(Count/Effort)*propCatchable,
         Date=ymd(paste(Year,Month,Day,sep="-")),
         shoalsDistance_km=distance(LAT,white[1],LONG,white[2]))%>%
  filter(catchable_ERelNum<100000)




#Plot the location of the trawls, based on the starting points being in ~90km2 area
spatialSummary<-GoM_tcatch_st%>%
  mutate(long=round(LONG,digits=1),lat=round(LAT,digits=1))%>%
  group_by(season,Species,long,lat)%>%
  summarise(catch=mean(catchable_ERelNum),trawls=n_distinct(Date))



ggmap(zoomGOM)+
  geom_point(data=spatialSummary,aes(long,lat,color=catch),show.legend = F)+
  scale_color_viridis_c(name="Catch Density\n(count)",rescaler = function(x, to = c(0, 1), from = NULL)
    {ifelse(x<10,scales::rescale(x,to = to,from = c(min(x, na.rm = TRUE), 10)), 1)})+
  facet_wrap(~Species+season)+
  geom_point(aes(white[2],white[1]),size=7,stroke=3,shape=8,color="firebrick3")+
  xlab("Longitude")+ylab("Longitude")
  

#Where was the most effort put in?
sampleEffort<-GoM_tcatch_st%>%
  mutate(long=round(LONG,digits=1),lat=round(LAT,digits=1))%>%
  dplyr::select(long,lat,season,Effort,Date,Year)%>%unique()%>%
  group_by(season,long,lat)%>%
  summarise(Effort=sum(Effort),trawls=n_distinct(Date),years=n_distinct(Year),epy=Effort/years)
ggplot(sampleEffort,aes(trawls,Effort))+
  geom_point()+geom_smooth()

ggmap(zoomGOM)+
  geom_point(data=sampleEffort,aes(long,lat,color=Effort),size=4,shape=15,show.legend = T)+
  scale_color_distiller(palette="RdYlBu",name="Effort\n(NM sampled)")+
  facet_wrap(~season)+
  geom_point(aes(white[2],white[1]),size=5,stroke=3,shape=8,color="goldenrod2")+
  xlab("Longitude")+ylab("Longitude")

#Smoothing with a TPS
springTPS<-Tps(cbind(filter(sampleEffort,season=="SPRING")$long,filter(sampleEffort,season=="SPRING")$lat),
               filter(sampleEffort,season=="SPRING")$Effort,lon.lat=T,miles=F)
predspringTPS<-predict(springTPS,cbind(sampleEffort$long,sampleEffort$lat))
springPreds<-data.frame(long=sampleEffort$long,lat=sampleEffort$lat,Effort=sampleEffort$Effort,season="SPRING",preds=predspringTPS)

fallTPS<-Tps(cbind(filter(sampleEffort,season=="FALL")$long,filter(sampleEffort,season=="FALL")$lat),
               filter(sampleEffort,season=="FALL")$Effort,lon.lat=T,miles=F)
predfallTPS<-predict(fallTPS,cbind(sampleEffort$long,sampleEffort$lat))

fallPreds<-data.frame(long=sampleEffort$long,lat=sampleEffort$lat,Effort=sampleEffort$Effort,season="FALL",preds=predfallTPS)

seasonPreds<-bind_rows(springPreds,fallPreds)%>%
  mutate(season=factor(season,levels=c("SPRING","FALL")),
         res=preds-Effort)
ggmap(zoomGOM)+
  geom_point(data=seasonPreds,aes(long,lat,color=preds),shape=15,size=4)+
  facet_wrap(~season)+
  scale_color_distiller(palette="RdYlBu",name="Sampling Effort\n(NM sampled)")+
  xlab("Longitude")+ylab("Latitude")+
  theme(legend.position = c(0.13,0.815),legend.background = element_rect(fill=NA),
        legend.text=element_text(color="white"),legend.title=element_text(color="white"))






#Spatial autocorrelation of catch
#Simple density of the catch for each species--presence/absence data
ggmap(zoomGOM)+
  stat_density_2d(data=GoM_tcatch_st,aes(LONG,LAT,fill = ..level..),binwidth=0.05, geom = "polygon", colour=rgb(1,1,1,alpha=0))+
  facet_wrap(~Species)+
  geom_point(aes(white[2],white[1]),size=7,stroke=3,shape=8,color="dodgerblue3")+
  scale_fill_distiller(palette="PuRd")+
  xlab("Longitude")+ylab("Longitude")





#Detrending--doing a simple LAT-LONG trend for each species and season
library(geoR)
library(sp)
library(fields)
library(rgeos)


#Return to the spatialSummary because the repeated points are probably the issue with the spatial dependence?
#Summarized points cover an ~90 km2 area for the trawl starting points
ggmap(zoomGOM)+
  geom_point(data=spatialSummary,aes(long,lat,color=log(catch)),shape=15,size=4,show.legend = T)+
  scale_color_distiller(palette="RdYlBu",name="log(Catch)\n(count)")+
  facet_wrap(~Species+season,nrow=5)+
  geom_point(aes(white[2],white[1]),size=5,stroke=3,shape=8,color="goldenrod2")+
  xlab("Longitude")+ylab("Longitude")


#Trying the kriging with the spatialSummary--where catch is averaged across trawls for regular areas
#Doing just one species-season to start
spHerring<-filter(spatialSummary,Species=="ATLANTIC HERRING"&season=="SPRING")
krigHerring<-spHerring
coordinates(krigHerring)<-~long+lat
krigHerring.geoR<-as.geodata(krigHerring,data.col = 3)

#Variogram
ghat<-variog(krigHerring.geoR)
plot(ghat)

ghat.exp<-variofit(ghat,ini.cov.pars = c(6000,2),cov.model="exponential")
ghat.mat<-variofit(ghat,ini.cov.pars = c(6000,2),cov.model="matern",kappa=1.5,fix.kappa=F)
lines(ghat.exp)
lines(ghat.mat,col="red")

#Showing another factor, probably distance from the shore
#Project the points to UTM
wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Arrange spatialSummary so that I can cbind distances back in later
spatialSummary<-arrange(spatialSummary,long)

summary19<-filter(spatialSummary,long<=-66)
wgsSummary19<-SpatialPointsDataFrame(coords=summary19[,c("long","lat")],data=summary19,proj4string=CRS(wgs.84))
utmSummary19<-spTransform(wgsSummary19,CRS("+proj=utm +zone=19 ellps=WGS84"))

summary20<-filter(spatialSummary,long>-66)
wgsSummary20<-SpatialPointsDataFrame(coords=summary20[,c("long","lat")],data=summary20,proj4string=CRS(wgs.84))
utmSummary20<-spTransform(wgsSummary20,CRS("+proj=utm +zone=20 ellps=WGS84"))


#Coastline
coast  <- readOGR(dsn=path.expand("./Shorelines"),layer="ne_10m_coastline",p4s=wgs.84)
plot(coast)
library(raster)
coast19<-crop(coast,wgsSummary19)
coast19<-spTransform(coast19,CRS("+proj=utm +zone=19 ellps=WGS84"))

coast20<-crop(coast,wgsSummary20)
coast20<-spTransform(coast20,CRS("+proj=utm +zone=20 ellps=WGS84"))

distance20 <- sapply(seq(1,length(utmSummary20),1),function(i)gDistance(utmSummary20[i,],coast20)/1000)
distance19<-sapply(seq(1,length(utmSummary19),1),function(i)gDistance(utmSummary19[i,],coast19)/1000)

spatialSummary$shoreDistance_km<-c(distance19,distance20)
#Check to see they calculated well and bound back in well
ggmap(zoomGOM)+
  geom_point(data=spatialSummary,aes(long,lat,color=shoreDistance_km),shape=15,size=4)+
  scale_color_distiller(palette = "RdYlBu",direction=1,name="Distance to Shore (km)")


#Detrend with the distance from shore--still just using the spring herring
spHerring<-filter(spatialSummary,Species=="ATLANTIC HERRING"&season=="SPRING")
summary(shoreLM<-lm(log(catch)~shoreDistance_km+long+lat,data=spHerring))

spHerring$mResid<-residuals(shoreLM)
spHerring$pred<-predict(shoreLM,data.frame(shoreDistance_km=spHerring$shoreDistance_km,long=spHerring$long,lat=spHerring$lat))

ggmap(zoomGOM)+
  geom_point(data=spHerring,aes(long,lat,color=mResid),shape=15,size=4)+
  scale_color_distiller(palette = "RdYlBu",direction=1,name="Residuals")


#Using these residuals for the Kriging
krigHerring2<-spHerring
coordinates(krigHerring2)<-~long+lat
krigHerring2.geoR<-as.geodata(krigHerring2,data.col=6) 

ghat2<-variog(krigHerring2.geoR)
plot(ghat2)

ghat2.exp<-variofit(ghat2,ini.cov.pars = c(2,2),cov.model="exponential")
ghat2.mat<-variofit(ghat2,ini.cov.pars = c(2,2),cov.model="matern",kappa=1.5,fix.kappa=F)
ghat2.sph<-variofit(ghat2,ini.cov.pars = c(2,2),cov.model="spherical")
lines(ghat2.exp,lwd=2) #I think the exponential is best...
lines(ghat2.mat,lwd=2,col="red")
lines(ghat2.sph,lwd=2,col="blue") 

#Kriging using the exponential variogram
kc<-krige.control(type="ok",obj.model = ghat2.exp)
sk<-krige.conv(krigHerring2.geoR,krige=kc,loc=cbind(spHerring$long,spHerring$lat))

quilt.plot(spHerring$long,spHerring$lat,sk$predict)

quilt.plot(spHerring$long,spHerring$lat,sk$predict+exp(spHerring$pred))

quilt.plot(spHerring$long,spHerring$lat,exp(spHerring$pred))

#Put into the spHerring to make in ggmap
spHerring$krige_pred<-sk$predict

ggmap(zoomGOM)+
  geom_point(data=spHerring,aes(long,lat,color=krige_pred),shape=15,size=4)+
  scale_color_distiller(palette = "RdYlBu",direction=-1,name="Krige Predictions\non Trend Residuals")+
  ylab("Latitude")+xlab("Longitude")


ggmap(zoomGOM)+
  geom_point(data=spHerring,aes(long,lat,color=krige_pred+exp(pred)),shape=15,size=4)+
  scale_color_distiller(palette = "RdYlBu",direction=-1,name="Catch Predictions\nTrend+Krige")+
  ylab("Latitude")+xlab("Longitude")

ggmap(zoomGOM)+
  geom_point(data=spHerring,aes(long,lat,color=log(catch)),shape=15,size=4)+
  scale_color_distiller(palette = "RdYlBu",direction=-1,name="log(Catch)")+
  ylab("Latitude")+xlab("Longitude")

ggplot(spHerring,aes(log(catch),krige_pred+exp(pred)))+
  geom_point()+geom_smooth(method="lm")+geom_abline(slope=1,intercept = 0)


ggplot(spHerring,aes(catch,exp(krige_pred+(pred))))+
  geom_point()+geom_smooth(method="lm")+geom_abline()

check<-data.frame(orig=spHerring$catch,pred=exp(spHerring$krige_pred+spHerring$pred))%>%
  filter(orig!=pred)%>%
  mutate(diff=orig-pred)





#############################
#Thin-plate splines for each season
#Sand Lance--a few spots have much higher catch than anywhere else, so they skew the residuals greatly
lance<-filter(GoM_tcatch_st,Species=="NORTHERN SAND LANCE")
trend<-lm(catchable_ERelNum~LONG+LAT+season+Date,data=lance)
summary(trend)


#Thin-plate spline attempt

SPlanceTPS<-Tps(cbind(filter(lance,season=="SPRING")$LONG,filter(lance,season=="SPRING")$LAT),
                filter(lance,season=="SPRING")$catchable_ERelNum,lon.lat=T,miles=F)
SPpredlanceTPS<-predict(SPlanceTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))

FAlanceTPS<-Tps(cbind(filter(lance,season=="FALL")$LONG,filter(lance,season=="FALL")$LAT),
                filter(lance,season=="FALL")$catchable_ERelNum,lon.lat=T,miles=F)
FApredlanceTPS<-predict(FAlanceTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))

#quilt.plot(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT,predLanceTPS)
lancePreds<-bind_rows(data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT, season="SPRING",
                                 catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,preds=SPpredlanceTPS),
                      data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT, season="FALL",
                                 catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,preds=FApredlanceTPS))
ggmap(zoomGOM)+
  geom_point(data=lancePreds,aes(LONG,LAT,color=preds),shape=15,size=4)+
  facet_wrap(~season)+
  scale_color_distiller(palette="RdYlBu",name="Sand Lance\nDensity\n(count)")+
  xlab("Longitude")+ylab("Latitude")




pred<-predict(trend,data.frame(LONG=lance$LONG,LAT=lance$LAT,season=lance$season))
quilt.plot(lance$LONG,lance$LAT,pred)
quilt.plot(lance$LONG,lance$LAT,residuals(trend))


lance$res<-residuals(trend)

spLance<-lance
coordinates(spLance)<-~LONG+LAT
spLance<-as.geodata(spLance,data.col=25)
spLance<-jitterDupCoords(spLance,max=0.01)

ghat=variog(spLance,max.dist = 200)
plot(ghat,pch=19)


#Alewife--Dominated by a few trawls with much higher catch
alewife<-filter(GoM_tcatch_st,Species=="ALEWIFE")
trend<-lm(catchable_ERelNum~LONG+LAT+season,data=alewife)
summary(trend)

#Thin-plate spline attempt
alewifeTPS<-Tps(cbind(alewife$LONG,alewife$LAT),alewife$catchable_ERelNum,lon.lat=T,miles=F)
predAlewifeTPS<-predict(alewifeTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))
#quilt.plot(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT,predAlewifeTPS)
alewifePreds<-data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT,
                         catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,predAlewifeTPS)
ggmap(zoomGOM)+
  geom_point(data=alewifePreds,aes(LONG,LAT,color=predAlewifeTPS),shape=15,size=4)+
  scale_color_distiller(palette="RdYlBu",name="Alewife Density\n(count)")+
  xlab("Longitude")+ylab("Latitude")


alewife.geo<-alewife
coordinates(alewife.geo)<-~LONG+LAT
alewife.geo<-as.geodata(alewife.geo,data.col=22)

pred<-predict(trend,data.frame(LONG=alewife$LONG,LAT=alewife$LAT,season=alewife$season))
quilt.plot(alewife$LONG,alewife$LAT,pred)
points(alewife.geo,add=T)

alewife.pred<-data.frame(LONG=alewife$LONG,LAT=alewife$LAT,season=alewife$season,catchable_ERelNum=alewife$catchable_ERelNum,pred)
ggplot(alewife.pred)+
  geom_point(aes(catchable_ERelNum,pred))

quilt.plot(alewife$LONG,alewife$LAT,residuals(trend))


alewife$res<-residuals(trend)

spAlewife<-alewife
coordinates(spAlewife)<-~LONG+LAT
spAlewife<-as.geodata(spAlewife,data.col=25)
spLance<-jitterDupCoords(spLance,max=0.01)

ghat=variog(spAlewife,max.dist = 10)
plot(ghat,pch=19)

ghat=variog(alewife.geo,max.dist=10)
plot(ghat,pch=19)


#Silver Hake
shake<-filter(GoM_tcatch_st,Species=="SILVER HAKE")
trend<-lm(catchable_ERelNum~LONG+LAT+season+Date,data=shake)
summary(trend)


#Thin-plate spline attempt
shakeTPS<-Tps(cbind(shake$LONG,shake$LAT),shake$catchable_ERelNum,lon.lat=T,miles=F)
predShakeTPS<-predict(shakeTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))
#quilt.plot(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT,predLanceTPS)
shakePreds<-data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT,
                       catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,predShakeTPS)
ggmap(zoomGOM)+
  geom_point(data=shakePreds,aes(LONG,LAT,color=predShakeTPS),shape=15,size=4)+
  scale_color_distiller(palette="RdYlBu",name="Silver Hake Density\n(count)")+
  xlab("Longitude")+ylab("Latitude")


#White Hake
whake<-filter(GoM_tcatch_st,Species=="WHITE HAKE")
trend<-lm(catchable_ERelNum~LONG+LAT+season+Date,data=whake)
summary(trend)


#Thin-plate spline attempt
SPwhakeTPS<-Tps(cbind(filter(whake,season=="SPRING")$LONG,filter(whake,season=="SPRING")$LAT),
              filter(whake,season=="SPRING")$catchable_ERelNum,lon.lat=T,miles=F)
SPpredWhakeTPS<-predict(SPwhakeTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))

FAwhakeTPS<-Tps(cbind(filter(whake,season=="FALL")$LONG,filter(whake,season=="FALL")$LAT),
                filter(whake,season=="FALL")$catchable_ERelNum,lon.lat=T,miles=F)
FApredWhakeTPS<-predict(FAwhakeTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))

#quilt.plot(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT,predLanceTPS)
whakePreds<-bind_rows(data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT, season="SPRING",
                                 catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,preds=SPpredWhakeTPS),
                      data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT, season="FALL",
                                 catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,preds=FApredWhakeTPS))
ggmap(zoomGOM)+
  geom_point(data=whakePreds,aes(LONG,LAT,color=preds),shape=15,size=4)+
  facet_wrap(~season)+
  scale_color_distiller(palette="RdYlBu",name="White Hake Density\n(count)")+
  xlab("Longitude")+ylab("Latitude")


#Red Hake
rhake<-filter(GoM_tcatch_st,Species=="RED HAKE")
trend<-lm(catchable_ERelNum~LONG+LAT+season+Date,data=rhake)
summary(trend)


#Thin-plate spline attempt
SPrhakeTPS<-Tps(cbind(filter(rhake,season=="SPRING")$LONG,filter(rhake,season=="SPRING")$LAT),
                filter(rhake,season=="SPRING")$catchable_ERelNum,lon.lat=T,miles=F)
SPpredRhakeTPS<-predict(SPrhakeTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))

FArhakeTPS<-Tps(cbind(filter(rhake,season=="FALL")$LONG,filter(rhake,season=="FALL")$LAT),
                filter(rhake,season=="FALL")$catchable_ERelNum,lon.lat=T,miles=F)
FApredRhakeTPS<-predict(FArhakeTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))

#quilt.plot(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT,predLanceTPS)
rhakePreds<-bind_rows(data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT, season="SPRING",
                                 catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,preds=SPpredRhakeTPS),
                      data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT, season="FALL",
                                 catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,preds=FApredRhakeTPS))
ggmap(zoomGOM)+
  geom_point(data=rhakePreds,aes(LONG,LAT,color=preds),shape=15,size=4)+
  facet_wrap(~season)+
  scale_color_distiller(palette="RdYlBu",name="Red Hake Density\n(count)")+
  xlab("Longitude")+ylab("Latitude")






#And in a season way...
#Atlantic Herring
spHerring<-filter(GoM_tcatch_st,Species=="ATLANTIC HERRING" & season=="SPRING")
faHerring<-filter(GoM_tcatch_st,Species=="ATLANTIC HERRING" & season=="FALL")
trend<-lm(catchable_ERelNum~LONG+LAT+Date,data=spHerring)
summary(trend)
trend<-lm(catchable_ERelNum~LONG+LAT+Date,data=faHerring)
summary(trend)


#Thin-plate spline attempt
spHerringTPS<-Tps(cbind(spHerring$LONG,spHerring$LAT),spHerring$catchable_ERelNum,lon.lat=T,miles=F)
faHerringTPS<-Tps(cbind(faHerring$LONG,faHerring$LAT),faHerring$catchable_ERelNum,lon.lat=T,miles=F)
predspHerringTPS<-predict(spHerringTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))
predfaHerringTPS<-predict(faHerringTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))
#quilt.plot(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT,predLanceTPS)
seasHerringPreds<-bind_rows(data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT,
                                       catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,pred=predspHerringTPS,season="SPRING"),
                            data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT,
                                       catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,pred=predfaHerringTPS,season="FALL"))
seasHerringPreds$season<-factor(seasHerringPreds$season,levels=c("SPRING","FALL"))

ggmap(zoomGOM)+
  geom_point(data=seasHerringPreds,aes(LONG,LAT,color=pred),shape=15,size=4)+
  facet_wrap(~season)+
  scale_color_distiller(palette="RdYlBu",name="Atlantic Herring\nDensity\n(count)")+
  xlab("Longitude")+ylab("Latitude")



#Atlantic Herring
spbHerring<-filter(GoM_tcatch_st,Species=="BLUEBACK HERRING" & season=="SPRING")
fabHerring<-filter(GoM_tcatch_st,Species=="BLUEBACK HERRING" & season=="FALL")
trend<-lm(catchable_ERelNum~LONG+LAT+Date,data=spbHerring)
summary(trend)
trend<-lm(catchable_ERelNum~LONG+LAT+Date,data=fabHerring)
summary(trend)


#Thin-plate spline attempt
spbHerringTPS<-Tps(cbind(spbHerring$LONG,spbHerring$LAT),spbHerring$catchable_ERelNum,lon.lat=T,miles=F)
fabHerringTPS<-Tps(cbind(fabHerring$LONG,fabHerring$LAT),fabHerring$catchable_ERelNum,lon.lat=T,miles=F)
predspbHerringTPS<-predict(spbHerringTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))
predfabHerringTPS<-predict(fabHerringTPS,cbind(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT))
#quilt.plot(GoM_tcatch_st$LONG,GoM_tcatch_st$LAT,predLanceTPS)
seasbHerringPreds<-bind_rows(data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT,
                                       catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,pred=predspbHerringTPS,season="SPRING"),
                            data.frame(LONG=GoM_tcatch_st$LONG,LAT=GoM_tcatch_st$LAT,
                                       catchable_ERelNum=GoM_tcatch_st$catchable_ERelNum,pred=predfabHerringTPS,season="FALL"))
seasbHerringPreds$season<-factor(seasbHerringPreds$season,levels=c("SPRING","FALL"))

ggmap(zoomGOM)+
  geom_point(data=seasbHerringPreds,aes(LONG,LAT,color=pred),shape=15,size=4)+
  facet_wrap(~season)+
  scale_color_distiller(palette="RdYlBu",name="Blueback Herring\nDensity\n(count)")+
  xlab("Longitude")+ylab("Latitude")





#Based on the distance from the Isles
test<-filter(GoM_tcatch_st,Count>5)
ggplot(test)+
  geom_point(aes(shoalsDistance_km,catchable_ERelNum,fill=Species),shape=21,size=4,show.legend=F)+
  geom_smooth(aes(shoalsDistance_km,catchable_ERelNum,group=Species))+
  facet_wrap(~Species,scales="free_y")+
  scale_fill_brewer(palette="Spectral")






# Temporal variation ------------------------------------------------------


ggplot(GoM_tcatch_st)+
  geom_point(aes(yday(Date),catchable_ERelNum,fill=Species),shape=21,size=5)+
  scale_fill_brewer(palette = "Set1")

#For each date, regardless of year and species
GoM_tcatch_date<-GoM_tcatch_st%>%
  group_by(nDate=yday(Date))%>%
  summarise(dateCatch=sum(catchable_ERelNum))%>%
  ggplot()+geom_line(aes(nDate,dateCatch))+geom_point(aes(nDate,dateCatch))
GoM_tcatch_date









# Bathymetry of GoM Area --------------------------------------------------

library(raster)

bathy<-raster("Bathymetry/GEBCO_2020_17_Aug_2020_c7bb8b333b62/gebco_2020_n44.8_s41.0_w-70.9_e-63.44.nc",varname="elevation")

bathyDF<-as.data.frame(bathy[[1]],xy=T)
colnames(bathyDF)<-c("x","y","z")
bathyDF$absZ<-abs(bathyDF$z)

bathyDF$Neg<-ifelse(bathyDF$z>=0,NA,bathyDF$z)

library(ggmap)
load("GoM_Map.R")
white<-c(42.966,-70.625)


ggmap(zoomGOM)+
  geom_raster(data=bathyDF,aes(x,y,fill=Neg,alpha=ifelse(is.na(bathyDF$Neg),0,1)))+
  coord_quickmap()+
  scale_fill_distiller(palette="RdYlBu",name="Depth (m)")+
  guides(alpha="none")+
  ylab("Latitude")+xlab("Longitude")


#Cube-root transform depth
bathyDF$Pos<-((bathyDF$Neg*-1)^(1/3))*-1


ggmap(zoomGOM)+
  geom_raster(data=bathyDF,aes(x,y,fill=Pos,alpha=ifelse(is.na(bathyDF$Pos),0,1)))+
  coord_quickmap()+
  scale_fill_gradientn(colors=rev(brewer.pal(9,"RdYlBu")),breaks=c(-4,-8,-12,-16),labels=c(-64,-512,-1728,-4096),name="Depth (m)")+
  guides(alpha="none")+
  ylab("Latitude")+xlab("Longitude")




wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
coast  <- readOGR(dsn=path.expand("./Shorelines"),layer="ne_10m_coastline",p4s=wgs.84)

GoMcoast<-crop(coast,bathy)
plot(GoMcoast)

#Convert + to NA




