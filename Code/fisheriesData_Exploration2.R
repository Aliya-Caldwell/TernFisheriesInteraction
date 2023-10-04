
setwd("C:/Users/nh1087/Documents/UNH Research/Seabirds/Data/")
rm(list=ls())
set.seed(42)

library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(RColorBrewer)

theme_set(theme_bw(base_size=25))

#Better categorization function
spNMFSCats<-function(x,y) {
  if (grepl("HAKE",x,ignore.case=T)) {
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



#Fisheries dependent data for all species in the North Atlantic--1950-2018

fullDep<-read_csv("foss_landings.csv")
colnames(fullDep)<-gsub(" ","_",colnames(fullDep))

#States included
unique(fullDep$State)
#Years
range(fullDep$Year)
#Species
unique(fullDep$Scientific_Name)
species<-unique(fullDep$NMFS_Name)

View(species)
#** - These names represent aggregations of more than one species. They are not inclusive, but rather represent landings 
#where we do not have species-specific data. Selecting "Sharks", for example, will not return all sharks but only 
#those where we do not have more specific information.



#Very broad, first go at what might be the important species for the seabirds
seabird_species<-subset(species,grepl(paste(c("HERRING","ALEWIFE","SAND LANCE","BUTTERFISH","HAKE","POLLOCK","MACKEREL"),collapse="|"),species))
View(seabird_species)

#Removing the asterisks because it's repeated by species that don't have it and ruins regex
seabird_species<-sort(unique(gsub(" \\*\\*","",seabird_species)))


#What data exists on those species
seabirdDep<-filter(fullDep,grepl(paste(seabird_species,collapse="|"),fullDep$NMFS_Name))%>%
  dplyr::select(Year,State,Species=NMFS_Name,Scientific_Name,Collection,Pounds)
seabirdDep<-seabirdDep[!is.na(seabirdDep$Pounds),]


seabirdDep$Sp_Category<-sapply(seabirdDep$Species,spNMFSCats)


categoryDep<-seabirdDep%>%
  dplyr::group_by(Year,Sp_Category)%>%
  summarise(Catch=sum(Pounds))
ggplot(categoryDep,aes(x=Year,y=Catch,color=Sp_Category))+
  geom_point(size=3)+
  geom_path(lwd=1.1)+
  scale_color_brewer(palette = "Set2",name="Family")+
  ylab("Annual Catch (lbs)")+
  ggtitle("Fishery (Comm + Rec) Dependent Data from NMFS")

depProps<-seabirdDep%>%
  dplyr::group_by(Year)%>%
  mutate(Total=sum(Pounds))%>%
  dplyr::group_by(Year,Sp_Category)%>%
  mutate(Prop=sum(Pounds)/Total)%>%
  dplyr::select(Year,Family=Sp_Category,Total,Prop)%>%
  unique()

depProps$Year<-as.numeric(depProps$Year)

#Graphing the proportions changing over time
ggplot(depProps)+
  geom_col(aes(x=Year,y=Prop,fill=Family))+geom_point(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  geom_line(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  scale_y_continuous(expand=c(0,0.005),sec.axis=sec_axis(~.*max(depProps$Total),name="Total Landings Weight"))+
  scale_x_continuous(breaks=seq(1950,2018,by=2),expand=c(0,0.01))+
  theme(axis.text.x.bottom=element_text(angle=30,hjust=1,vjust=1,size=20))+
  ggtitle("NMFS Fishery Dependent Data")+ylab("Proportion of Total Landings Weight (lbs)")




#SAME idea with the ACCSP Date download
accspData<-read_csv("non-confidential_commercial_landings_by_selected_parameters.csv",
                    col_types = cols(Pounds = col_double()))
colnames(accspData)<-gsub(" ","_",colnames(accspData))

accsp_species<-unique(subset(accspData$Common_Name,grepl(paste(c("HERRING","LANCE","BUTTERFISH","HAKE","POLLOCK","MACKEREL"),collapse="|"),accspData$Common_Name)))

seabird_accsp<-filter(accspData,grepl(paste(accsp_species,collapse="|"),accspData$Common_Name))%>%
  dplyr::select(Year,Species=Common_Name,Pounds)%>%
  arrange(Year,Species)
seabird_accsp<-seabird_accsp[!is.na(seabird_accsp$Pounds),]


seabird_accsp$Sp_Category<-sapply(seabird_accsp$Species,spNMFSCats)
category_accsp<-seabird_accsp%>%
  dplyr::group_by(Year,Sp_Category)%>%
  dplyr::summarise(Catch=sum(Pounds))

ggplot(category_accsp,aes(Year,Catch,color=Sp_Category))+
  geom_point(size=3)+
  geom_path(lwd=1.1)+
  ylab("Annual Catch (lbs) ACCSP")+
  scale_color_brewer(palette = "Set2",name="Species Family")


accspProps<-seabird_accsp%>%
  dplyr::group_by(Year)%>%
  mutate(Total=sum(Pounds))%>%
  dplyr::group_by(Year,Sp_Category)%>%
  mutate(Prop=sum(Pounds)/Total)%>%
  dplyr::select(Year,Family=Sp_Category,Total,Prop)%>%
  unique()

accspProps$Year<-as.numeric(accspProps$Year)

#Graphing the proportions changing over time
ggplot(accspProps)+
  geom_col(aes(x=Year,y=Prop,fill=Family))+geom_point(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  geom_line(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  scale_y_continuous(expand=c(0,0.005),sec.axis=sec_axis(~.*max(accspProps$Total),name="Total Landings Weight"))+
  scale_x_continuous(breaks=seq(min(accspProps$Year),max(accspProps$Year),by=2),expand=c(0,0.01))+
  theme(axis.text.x.bottom=element_text(angle=30,hjust=1,vjust=1,size=20))+
  ggtitle("ACCSP Fishery Dependent Data")+ylab("Proportion of Total Landings Weight")


dataCompleteness<-filter(accspData,grepl(paste(accsp_species,collapse="|"),accspData$Common_Name))%>%
  mutate(perc=gsub("[%,>,<,=]","",`%_Display`),
         perc=ifelse(grepl("-85",perc),"80",ifelse(grepl("-95",perc),"90",perc)),
         perc=as.numeric(perc))
hist(dataCompleteness$perc)
incomplete<-subset(dataCompleteness,perc<100&perc>1)
hist(incomplete$perc)
incomplete$percC<-as.character(incomplete$perc)
ggplot(incomplete)+
  geom_bar(aes(x=Common_Name,fill=percC))+
  theme(axis.text.x = element_text(angle=90,size=10))
#

# Fishery Independent--NMFS Trawl Data ------------------------------------

#Spring Trawl

springT<-read_csv("NMFS Trawls/Spring/22561_UNION_FSCS_SVCAT.csv", 
                  col_types = cols(CRUISE6 = col_character(), ID = col_character(), 
                                   STATION = col_double(), STRATUM = col_double(), 
                                   TOW = col_double()))

springT$Year<-substr(springT$CRUISE6,1,4)


species_trawl<-unique(springT$LOGGED_SPECIES_NAME)
seabird_trawl<-subset(species_trawl,grepl(paste(c("HAKE","BUTTERFISH","POLLOCK","MACKEREL","ALEWIFE","HERRING","SAND LANCE"),collapse="|"),ignore.case=T,species_trawl))
View(seabird_trawl)



seabird_springT<-filter(springT,grepl(paste(seabird_trawl,collapse="|"),
                                      springT$LOGGED_SPECIES_NAME,ignore.case=T))%>%
  dplyr::select(Year,Species=LOGGED_SPECIES_NAME,Pounds=EXPCATCHWT,Count=EXPCATCHNUM)%>%
  arrange(Year,Species)
seabird_springT<-seabird_springT[!is.na(seabird_springT$Pounds),]


seabird_springT$Family<-sapply(seabird_springT$Species,spNMFSCats)

springT_simple<-seabird_springT%>%
  group_by(Year,Family)%>%
  summarise(Catch_Wt=sum(Pounds),
            Catch_Num=sum(Count))%>%
  arrange(Year,Family)

springT_simple$Year<-as.numeric(springT_simple$Year)

ggplot(springT_simple,aes(Year,Catch_Wt,color=Family))+
  geom_point(size=3)+
  geom_path(lwd=1.1)+
  ggtitle("NMFS FINNS Spring Trawl Data")+ylab("Catch Weight")+
  scale_color_brewer(palette = "Set2")+
  scale_x_continuous(expand=c(0,1))+
  scale_y_continuous(expand=c(0,200))


#Proportion of catch in each year
springT_props<-seabird_springT%>%
  mutate(Year=substr(Year,1,4))%>%
  dplyr::group_by(Year)%>%
  mutate(Total=sum(Pounds))%>%
  dplyr::group_by(Year,Family)%>%
  mutate(Prop=sum(Pounds)/Total)%>%
  dplyr::select(Year,Family,Total,Prop)%>%
  unique()

springT_props$Year<-as.numeric(springT_props$Year)

#Graphing the proportions changing over time
ggplot(springT_props)+
  geom_col(aes(x=Year,y=Prop,fill=Family))+
  geom_point(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  geom_line(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  scale_y_continuous(expand=c(0,0.005),sec.axis=sec_axis(~.*max(springT_props$Total),name="Total Catch"))+
  scale_x_continuous(breaks=springT_props$Year[seq(1,length(springT_props$Year),by=12)],expand=c(0,0.01))+
  theme(axis.text.x.bottom=element_text(angle=30,hjust=1,vjust=1,size=20))+
  ggtitle("NMFS FINNS Spring Trawl Data")+ylab("Proportion of Total Catch Weight")


##Fall Trawl

fallT<-read_csv("NMFS Trawls/Fall/22560_UNION_FSCS_SVCAT.csv", 
                col_types = cols(CRUISE6 = col_character(), ID = col_character(), 
                                 STATION = col_double(), STRATUM = col_double(), 
                                 TOW = col_double()))

fallT$Year<-substr(fallT$CRUISE6,1,4)


seabird_fallT<-filter(fallT,grepl(paste(seabird_trawl,collapse="|"),
                                  fallT$LOGGED_SPECIES_NAME,ignore.case=T))%>%
  dplyr::select(Year,Species=LOGGED_SPECIES_NAME,Pounds=EXPCATCHWT,Count=EXPCATCHNUM)%>%
  arrange(Year,Species)
seabird_fallT<-seabird_fallT[!is.na(seabird_fallT$Pounds),]


seabird_fallT$Family<-sapply(seabird_fallT$Species,spNMFSCats)

fallT_simple<-seabird_fallT%>%
  mutate(Year=substr(Year,1,4))%>%
  group_by(Year,Family)%>%
  summarise(Catch_Wt=sum(Pounds),
            Catch_Num=sum(Count))%>%
  arrange(Year,Family)

fallT_simple$Year<-as.numeric(fallT_simple$Year)

ggplot(fallT_simple,aes(Year,Catch_Wt,color=Family))+
  geom_point(size=3)+
  geom_path(lwd=1.1)+
  ggtitle("NMFS FINNS Fall Trawl Data")+ylab("Catch Weight")

#Proportion of catch in each year
fallT_props<-seabird_fallT%>%
  mutate(Year=substr(Year,1,4))%>%
  dplyr::group_by(Year)%>%
  mutate(Total=sum(Pounds))%>%
  dplyr::group_by(Year,Family)%>%
  mutate(Prop=sum(Pounds)/Total)%>%
  dplyr::select(Year,Family,Total,Prop)%>%
  unique()

fallT_props$Year<-as.numeric(fallT_props$Year)

#Graphing the proportions changing over time
ggplot(fallT_props)+
  geom_col(aes(x=Year,y=Prop,fill=Family))+geom_point(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  geom_line(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  scale_y_continuous(expand=c(0,0.005),sec.axis=sec_axis(~.*max(fallT_props$Total),name="Total Catch"))+
  scale_x_continuous(breaks=fallT_props$Year[seq(1,length(fallT_props$Year),by=12)],expand=c(0,0.01))+
  theme(axis.text.x.bottom=element_text(angle=30,hjust=1,vjust=1,size=20))+
  ggtitle("NMFS FINNS Fall Trawl Data")+ylab("Proportion of Total Catch Weight")





# Other -------------------------------------------------------------------

#fallT$Family<-sapply(fallT$LOGGED_SPECIES_NAME,spNMFSCats)

fallT_year<-fallT%>%
  group_by(Year,Family)%>%
  summarise(Catch_Wt=sum(EXPCATCHWT,na.rm=T),
            Catch_Num=sum(EXPCATCHNUM,na.rm=T))%>%
  arrange(Year,Family)

fallT_year$Year<-as.numeric(fallT_year$Year)

ggplot(fallT_year,aes(Year,Catch_Wt,color=Family))+
  geom_point(size=3)+
  geom_path(lwd=1.1)+
  ggtitle("NMFS FINNS Fall Trawl Data")+ylab("Catch Weight")

#Proportion of catch in each year
fallT_prop<-fallT%>%
  dplyr::group_by(Year)%>%
  mutate(Total=sum(EXPCATCHWT,na.rm=T))%>%
  dplyr::group_by(Year,Family)%>%
  mutate(Prop=sum(EXPCATCHWT,na.rm=T)/Total)%>%
  dplyr::select(Year,Family,Total,Prop)%>%
  unique()

fallT_prop$Year<-as.numeric(fallT_prop$Year)

#Graphing the proportions changing over time
ggplot(fallT_prop)+
  geom_col(aes(x=Year,y=Prop,fill=Family))+geom_point(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  geom_line(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  scale_y_continuous(expand=c(0,0.005),sec.axis=sec_axis(~.*max(fallT_prop$Total),name="Total Catch"))+
  scale_x_continuous(breaks=fallT_prop$Year[seq(1,length(fallT_prop$Year),by=14)],expand=c(0,0.01))+
  theme(axis.text.x.bottom=element_text(angle=30,hjust=1,vjust=1,size=20))+
  ggtitle("NMFS FINNS Fall Trawl Data")+ylab("Proportion of Total Catch Weight")





#What species are being put into the listed groups and how common are they (each tally is a time they were in a trawl)
#Line 1 are the most common (in the hundreds-thousands)
#Line 2 are regular (in the tens-hundred)
#Line 3 are uncommon (less than tens)

hake<-table(subset(fallT$LOGGED_SPECIES_NAME,grepl("hake",fallT$LOGGED_SPECIES_NAME,ignore.case = T)))
View(hake)
#silver, red, spotted, white,
#offshore, longfin, 
#carolina, southern, blue

herring<-table(subset(springT$LOGGED_SPECIES_NAME,grepl(paste(c("alewi","herring"),collapse="|"),
                                                        springT$LOGGED_SPECIES_NAME,ignore.case = T)))
View(herring)
#Atlantic, alewife, blueback
#round, thread, 
#dwarf

mackerel<-table(subset(fallT$LOGGED_SPECIES_NAME,grepl("mackerel",fallT$LOGGED_SPECIES_NAME,ignore.case=T)))
View(mackerel)
#Atlantic
#Chub, scad, king, spanish
#snake, frigate

lance<-table(subset(springT$LOGGED_SPECIES_NAME,grepl("sand lance",springT$LOGGED_SPECIES_NAME,ignore.case=T)))
View(lance)
#Northern, 
#american

pollock<-table(subset(fallT$LOGGED_SPECIES_NAME,grepl("pollock",fallT$LOGGED_SPECIES_NAME,ignore.case=T)))
View(pollock)
#pollock
butterfish<-table(subset(fallT$LOGGED_SPECIES_NAME,grepl("butterfish",fallT$LOGGED_SPECIES_NAME,ignore.case=T)))
View(butterfish)
#butterfish









# Locations ---------------------------------------------------------------


#Trawl positions
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

#Mapping the trawl points
library(ggmap)
register_google(key="AIzaSyAeL1o4KZ_yiSUx3EcDNM8SbSJWjkPW4IM")
zoom6<-get_googlemap(center=c(-70.25,39.75),zoom=6,maptype="satellite")

#Both are missing values quite a bit, 20% for duration and 35% for speed
  #Replaced those with the average
catchLocations$TOWDUR<-ifelse(is.na(catchLocations$TOWDUR),mean(catchLocations$TOWDUR,na.rm=T),catchLocations$TOWDUR)
catchLocations$DESSPEED<-ifelse(is.na(catchLocations$DESSPEED),mean(catchLocations$DESSPEED,na.rm=T),catchLocations$DESSPEED)
catchLocations$Effort<-catchLocations$DESSPEED*catchLocations$TOWDUR


#Cropping the data to just the points for each trawl

uniqueTrawls<-unique(dplyr::select(catchLocations,ID,LONG,LAT,AREA,DESSPEED,TOWDUR,Effort))

ggmap(zoom6)+
  geom_point(data=uniqueTrawls,aes(x=LONG,y=LAT,color=AREA))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.text=element_text(size=40),axis.title=element_text(size=50))+
  guides(color=guide_legend(override.aes=list(size=5)))


#Checking the range of trawl sampling effort--speed, duration, and the product
ggplot(uniqueTrawls)+
  geom_boxplot(aes(x=1,y=DESSPEED),color="purple",fill="orchid")+
  geom_jitter(aes(x=1,y=DESSPEED),alpha=0.5)+
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank())

ggplot(uniqueTrawls)+
  geom_boxplot(aes(x=1,y=TOWDUR),color="purple",fill="orchid")+
  geom_jitter(aes(x=1,y=TOWDUR),alpha=0.5)+
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank())

ggplot(uniqueTrawls)+
  geom_boxplot(aes(x=1,y=Effort),color="purple",fill="orchid")+
  geom_jitter(aes(x=1,y=Effort),alpha=0.5)+
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank())


# Cropping down to GoM ----------------------------------------------------

#What area codes are in the GOM--very broad area
white<-c(42.966,-70.625)

#zoomGOM<-get_googlemap(center=c(white[2]+2,white[1]+0.5),zoom=7,maptype="satellite")
#save(zoomGOM,file="GoM_Map.R")

load("GoM_Map.R")
ggmap(zoomGOM)+
  geom_point(data=subset(unique(dplyr::select(catchLocations,ID,LONG,LAT,AREA)),AREA<523),
             aes(x=LONG,y=LAT,color=AREA),size=5)+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.text=element_text(size=40),axis.title=element_text(size=50))+
  guides(color=guide_legend(override.aes=list(size=5)))

GOMtrawls<-subset(catchLocations,AREA<523)

GoM_Seabirds<-filter(GOMtrawls,grepl(paste(seabird_trawl,collapse="|"),
                                           GOMtrawls$LOGGED_SPECIES_NAME,ignore.case=T))%>%
  dplyr::select(Year,CRUISE6,STRATUM,STATION,Species=LOGGED_SPECIES_NAME,season,
                Pounds=EXPCATCHWT,Count=EXPCATCHNUM,LONG:Effort)%>%
  arrange(Year,Species)
GoM_Seabirds<-GoM_Seabirds[!is.na(GoM_Seabirds$Pounds),]


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
  summarise(Catch_Wt=sum(Pounds),
            Catch_Num=sum(Count,na.rm=T))%>%
  mutate(Rel_Wt=Catch_Wt/Trawls,
         Rel_Num=Catch_Num/Trawls,
         ERel_Wt=Catch_Wt/Trawls_Efforts,
         ERel_Num=Catch_Num/Trawls_Efforts)%>%
  arrange(Year,season,Family)

GOM_simple$Year<-as.numeric(GOM_simple$Year)


#Proportion of catch in each year
GoM_props<-GoM_Seabirds%>%
  dplyr::group_by(Year,season)%>%
  mutate(Total=sum(Pounds))%>%
  dplyr::group_by(Year,season,Family)%>%
  mutate(Prop=sum(Pounds)/Total)%>%
  dplyr::select(Year,season,Family,Total,Prop)%>%
  unique()

GoM_props$Year<-as.numeric(GoM_props$Year)



ggplot(subset(GOM_simple,season=="SPRING"),aes(Year,Catch_Wt,color=Family))+
  geom_point(size=3)+
  geom_path(lwd=1.1)+
  ggtitle("NMFS FINNS Spring Trawl Data in GoM")+ylab("Catch Weight")

#Using bars instead per Nathan's suggestion
ggplot(subset(GOM_simple,season=="SPRING"),aes(Year,Catch_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,15000),expand=c(0,20))+
  scale_x_continuous(breaks=seq(min(subset(GOM_simple,season=="SPRING")$Year),
                                max(subset(GOM_simple,season=="SPRING")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1),panel.grid.major.x=element_line(size=0.5))+
  ggtitle("NMFS FINNS Spring Trawl Data in GoM")+ylab("Catch Weight")

#Using a weight standardized by the number of unique trawls in the data for the area in each year
ggplot(subset(GOM_simple,season=="SPRING"),aes(Year,Rel_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,125),expand=c(0,0.01))+
  scale_x_continuous(breaks=seq(min(subset(GOM_simple,season=="SPRING")$Year),
                                max(subset(GOM_simple,season=="SPRING")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1),panel.grid.major.x=element_line(size=0.5))+
  ggtitle("NMFS FINNS Spring Trawl Data in GoM")+ylab("Catch Weight per Trawl")

#Edit: The EFFORT from the CPUE is now equal to the distance traveled by the trawls in that season-year (ERel_Wt)
ggplot(subset(GOM_simple,season=="SPRING"),aes(Year,ERel_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,1.6),expand=c(0,0.01))+
  scale_x_continuous(breaks=seq(min(subset(GOM_simple,season=="SPRING")$Year),
                                max(subset(GOM_simple,season=="SPRING")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1),panel.grid.major.x=element_line(size=0.5))+
  ggtitle("NMFS FINNS Spring Trawl Data in GoM")+ylab("Catch Weight per Trawl Distance")

#Graphing the proportions changing over time
ggplot(subset(GoM_props,season=="SPRING"))+
  geom_col(aes(x=Year,y=Prop,fill=Family))+geom_point(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  geom_line(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  scale_y_continuous(expand=c(0,0.005),sec.axis=sec_axis(~.*max(subset(GoM_props,season=="SPRING")$Total),name="Total Catch"))+
  scale_x_continuous(breaks=seq(min(subset(GoM_props,season=="SPRING")$Year),
                                max(subset(GoM_props,season=="SPRING")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x.bottom=element_text(angle=30,hjust=1,vjust=1,size=20))+
  ggtitle("NMFS FINNS Spring Trawl Data in GoM")+ylab("Proportion of Total Catch Weight")



#Fall
ggplot(subset(GOM_simple,season=="FALL"),aes(Year,Catch_Wt,color=Family))+
  geom_point(size=3)+
  geom_path(lwd=1.1)+
  ggtitle("NMFS FINNS Fall Trawl Data in GoM")+ylab("Catch Weight")

#Using bars instead per Nathan's suggestion
ggplot(subset(GOM_simple,season=="FALL"),aes(Year,Catch_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,25000),expand=c(0,20))+
  scale_x_continuous(breaks=seq(min(subset(GOM_simple,season=="FALL")$Year),
                                max(subset(GOM_simple,season=="FALL")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1),panel.grid.major.x=element_line(size=0.5))+
  ggtitle("NMFS FINNS Fall Trawl Data in GoM")+ylab("Catch Weight")

#Using a weight standardized by the number of unique trawls in the data for the area in each year
ggplot(subset(GOM_simple,season=="FALL"),aes(Year,Rel_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,200),expand=c(0,1))+
  scale_x_continuous(breaks=seq(min(subset(GOM_simple,season=="FALL")$Year),
                                max(subset(GOM_simple,season=="FALL")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1),panel.grid.major.x=element_line(size=0.5))+
  ggtitle("NMFS FINNS Fall Trawl Data in GoM")+ylab("Catch Weight per Trawl")

#Edit: The EFFORT from the CPUE is now equal to the distance traveled by the trawls in that season-year (ERel_Wt)
ggplot(subset(GOM_simple,season=="FALL"),aes(Year,ERel_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,3),expand=c(0,0.01))+
  scale_x_continuous(breaks=seq(min(subset(GOM_simple,season=="FALL")$Year),
                                max(subset(GOM_simple,season=="FALL")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1),panel.grid.major.x=element_line(size=0.5))+
  ggtitle("NMFS FINNS Fall Trawl Data in GoM")+ylab("Catch Weight per Trawl Distance")

#Graphing the proportions changing over time
ggplot(subset(GoM_props,season=="FALL"))+
  geom_col(aes(x=Year,y=Prop,fill=Family))+geom_point(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  geom_line(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  scale_y_continuous(expand=c(0,0.005),sec.axis=sec_axis(~.*max(subset(GoM_props,season=="FALL")$Total),name="Total Catch"))+
  scale_x_continuous(breaks=seq(min(subset(GoM_props,season=="FALL")$Year),
                                max(subset(GoM_props,season=="FALL")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x.bottom=element_text(angle=30,hjust=1,vjust=1,size=20))+
  ggtitle("NMFS FINNS Fall Trawl Data in GoM")+ylab("Proportion of Total Catch Weight")







# Buffer around White Island ----------------------------------------------

white<-c(42.966,-70.625)
buffer_seabirds<-GoM_Seabirds%>%
  rowwise()%>%
  mutate(distance_km=argosfilter::distance_m(white[1],white[2],LAT,LONG)/1000)%>%
  filter(distance_km<100)%>%
  ungroup()


#Plotting the map of those trawls in the 100km buffer

library(ggmap)
register_google(key="AIzaSyAeL1o4KZ_yiSUx3EcDNM8SbSJWjkPW4IM")
zoom9<-get_googlemap(center=c(white[2]+0.5,white[1]),zoom=9,size=c(960,960),maptype="satellite")
save(zoom9,file="isles_Map.R")


buffer_map<-unique(dplyr::select(buffer_seabirds,season,Year,CRUISE6,STRATUM,STATION,LONG,LAT,TOWDUR,DESSPEED,Effort))
load("isles_Map.R")
par(mar=c(0,0,0,0))
ggmap(zoom9)+
  geom_point(data=buffer_map,aes(x=LONG,y=LAT,color=season),size=4)+
  xlab("Longitude")+ylab("Latitude")+scale_color_brewer(palette="Set2",name="Season")+
  theme(legend.text=element_text(size=30),legend.title=element_text(size=35),
        axis.text=element_text(size=20),axis.title=element_text(size=25))+
  guides(color=guide_legend(override.aes=list(size=7)))



#Reorganizing them for the right plots on the catch
Buffer_simple<-buffer_seabirds%>%
  dplyr::select(Year,season,CRUISE6,STATION,STRATUM,Effort)%>%unique()%>%
  group_by(Year,season)%>%
  mutate(Trawls_Efforts=sum(Effort))%>%
  right_join(buffer_seabirds,Buffer_simple,by=c("Year","season","CRUISE6","STATION","STRATUM","Effort"))%>%
  group_by(Year,season)%>%
  mutate(Trawls=n_distinct(CRUISE6,STRATUM,STATION))%>%
  ungroup()%>%
  group_by(Year,season,Trawls,Trawls_Efforts,Family)%>%
  summarise(Catch_Wt=sum(Pounds),
            Catch_Num=sum(Count))%>%
  mutate(Rel_Wt=Catch_Wt/Trawls,
         Rel_Num=Catch_Num/Trawls,
         ERel_Wt=Catch_Wt/Trawls_Efforts,
         ERel_Num=Catch_Num/Trawls_Efforts)%>%
  arrange(Year,season,Family)

Buffer_simple$Year<-as.numeric(Buffer_simple$Year)


#Proportion of catch in each year
Buffer_props<-buffer_seabirds%>%
  dplyr::group_by(Year,season)%>%
  mutate(Total=sum(Pounds))%>%
  dplyr::group_by(Year,season,Family)%>%
  mutate(Prop=sum(Pounds)/Total)%>%
  dplyr::select(Year,season,Family,Total,Prop)%>%
  unique()%>%
  arrange(Year,season,Family)

Buffer_props$Year<-as.numeric(Buffer_props$Year)

#Spring Plots
#Line plot
ggplot(subset(Buffer_simple,season=="SPRING"),aes(Year,Catch_Wt,color=Family))+
  geom_point(size=3)+
  geom_path(lwd=1.1)+
  ggtitle("NMFS FINNS Spring Trawl Data in 100km of White Island")+ylab("Catch Weight")

#Using bars instead per Nathan's suggestion
ggplot(subset(Buffer_simple,season=="SPRING"),aes(Year,Catch_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,6000),expand=c(0,20))+
  scale_x_continuous(breaks=seq(min(subset(Buffer_simple,season=="SPRING")$Year),
                                max(subset(Buffer_simple,season=="SPRING")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1))+
  ggtitle("NMFS FINNS Spring Trawl Data in 100km of White Island")+ylab("Catch Weight")

#Relative catch based on the number of unique trawls in the area for that season-year
ggplot(subset(Buffer_simple,season=="SPRING"),aes(Year,Rel_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,135),expand=c(0,1))+
  scale_x_continuous(breaks=seq(min(subset(Buffer_simple,season=="SPRING")$Year),
                                max(subset(Buffer_simple,season=="SPRING")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1),panel.grid.major=element_line(size=0.5))+
  ggtitle("NMFS FINNS Spring Trawl Data in 100km of White Island")+ylab("Catch Weight per Trawl")

#Edit: The EFFORT in CPUE is now equal to the distance sampled in the year-season sum(speed*distance)
ggplot(subset(Buffer_simple,season=="SPRING"),aes(Year,ERel_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,2),expand=c(0,0.01))+
  scale_x_continuous(breaks=seq(min(subset(Buffer_simple,season=="SPRING")$Year),
                                max(subset(Buffer_simple,season=="SPRING")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1),panel.grid.major=element_line(size=0.5))+
  ggtitle("NMFS FINNS Spring Trawl Data in 100km of White Island")+ylab("Catch Weight per Trawl Distance")

#Graphing the proportions changing over time
ggplot(subset(Buffer_props,season=="SPRING"))+
  geom_col(aes(x=Year,y=Prop,fill=Family))+geom_point(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  geom_line(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  scale_y_continuous(expand=c(0,0.005),sec.axis=sec_axis(~.*max(subset(Buffer_props,season=="SPRING")$Total),name="Total Catch"))+
  scale_x_continuous(breaks=seq(min(subset(Buffer_props,season=="SPRING")$Year),
                                max(subset(Buffer_props,season=="SPRING")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x.bottom=element_text(angle=30,hjust=1,vjust=1,size=20))+
  ggtitle("NMFS FINNS spring Trawl Data in 100km of White Island")+ylab("Proportion of Total Catch Weight")



#Fall Plots
#Line plot
ggplot(subset(Buffer_simple,season=="FALL"),aes(Year,Catch_Wt,color=Family))+
  geom_point(size=3)+
  geom_path(lwd=1.1)+
  ggtitle("NMFS FINNS Fall Trawl Data in 100km of White Island")+ylab("Catch Weight")

#Using bars instead per Nathan's suggestion
ggplot(subset(Buffer_simple,season=="FALL"),aes(Year,Catch_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,6000),expand=c(0,20))+
  scale_x_continuous(breaks=seq(min(subset(Buffer_simple,season=="FALL")$Year),
                                max(subset(Buffer_simple,season=="FALL")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1))+
  ggtitle("NMFS FINNS Fall Trawl Data in 100km of White Island")+ylab("Catch Weight")

#Plotting the relative catch based on the number of unique trawls that went out that year-season
ggplot(subset(Buffer_simple,season=="FALL"),aes(Year,Rel_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,230),expand=c(0,1))+
  scale_x_continuous(breaks=seq(min(subset(Buffer_simple,season=="FALL")$Year),
                                max(subset(Buffer_simple,season=="FALL")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1),panel.grid.major=element_line(size=0.5))+
  ggtitle("NMFS FINNS Fall Trawl Data in 100km of White Island")+ylab("Catch Weight per Trawl")

#Edit: The EFFORT is now the trawl distance covered in a year-season as the sum of the product of speed and distance
ggplot(subset(Buffer_simple,season=="FALL"),aes(Year,ERel_Wt,fill=Family))+
  geom_col(color="black")+
  scale_y_continuous(limits=c(0,3.25),expand=c(0,0.01))+
  scale_x_continuous(breaks=seq(min(subset(Buffer_simple,season=="FALL")$Year),
                                max(subset(Buffer_simple,season=="FALL")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x=element_text(angle=30,hjust=1),panel.grid.major=element_line(size=0.5))+
  ggtitle("NMFS FINNS Fall Trawl Data in 100km of White Island")+ylab("Catch Weight per Trawl Distance")


#Graphing the proportions changing over time
ggplot(subset(Buffer_props,season=="FALL"))+
  geom_col(aes(x=Year,y=Prop,fill=Family))+geom_point(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  geom_line(aes(x=Year,y=Total/max(Total)),alpha=0.4)+
  scale_y_continuous(expand=c(0,0.005),
                     sec.axis=sec_axis(~.*max(subset(Buffer_props,season=="FALL")$Total),name="Total Catch"))+
  scale_x_continuous(breaks=seq(min(subset(Buffer_props,season=="FALL")$Year),
                                max(subset(Buffer_props,season=="FALL")$Year),by=2),expand=c(0,0.1))+
  theme(axis.text.x.bottom=element_text(angle=30,hjust=1,vjust=1,size=20))+
  ggtitle("NMFS FINNS Fall Trawl Data in 100km of White Island")+ylab("Proportion of Total Catch Weight")













# Size Structure ----------------------------------------------------------

fallTrawl_Sizes<-read_csv("~/UNH Research/Seabirds/Data/NMFS Trawls/Fall/22560_UNION_FSCS_SVLEN.csv", 
                          col_types = cols(CATCHSEX = col_character(), ID = col_character(),
                                           CRUISE6 = col_character(), STATION = col_number(),
                                           STRATUM = col_number(), TOW = col_number()))

springTrawl_Sizes<-read_csv("~/UNH Research/Seabirds/Data/NMFS Trawls/Spring/22561_UNION_FSCS_SVLEN.csv", 
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



# von Bertalanffy and Length-Ages ---------------------------------------------------------

#von Bertalanffy curves for the different species
vonB<-function(Li,k,t0,t) {
  l=Li*(1-exp(-1*k*(t-t0)))
}

#Silver Hake
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

#Northern Sand Lance
NSLli<-249
NSLk<-0.26
NSLt0<--1.10

NSL_vonB<-vonB(NSLli,NSLk,NSLt0,seq(0,20))

NSLDF<-data.frame(Length=NSL_vonB,Age=seq(0,20),source="von Bertalanffy")

ggplot(NSLDF)+
  geom_line(aes(Age,Length),lwd=1.5,color="grey30")+
  geom_point(aes(Age,Length),size=4)+
  xlab("Age (yrs)")+ylab("Length (cm)")

#Less than 1 year Sand Lance
NSL_y0<-data.frame(Age=numeric(length=11),Length=numeric(length=11),source="Logistic")
for (M in 0:10) {
  NSL_y0[M+1,1]<-M/12
  NSL_y0[M+1,2]<-(107.7/(1+exp((-0.71)*(M-5.17))))
}

ggplot(NSL_y0)+
  geom_point(aes(x=Age,y=Length))

#All together now!
allNSL<-bind_rows(NSL_y0,NSLDF[2:20,])

ggplot(allNSL)+
  geom_point(aes(x=Age,y=Length,fill=source),shape=21,size=5,stroke=1.1)+
  scale_fill_brewer(palette="Set2",name="Source Equation")+
  theme(legend.position=c(0.135,0.875),legend.background=element_rect(fill=rgb(0,0,0,alpha=0)))

#What about the age data that I already have from the NMFS trawls?
fallAges<-read_csv("NMFS Trawls/Fall/22560_UNION_FSCS_SVBIO.csv")
springAges<-read_csv("NMFS Trawls/Spring/22561_UNION_FSCS_SVBIO.csv")

fallAges$season<-"Fall"
springAges$season<-"Spring"
springAges$Year<-as.numeric(substr(springAges$CRUISE6,1,4))
fallAges$Year<-as.numeric(substr(fallAges$CRUISE6,1,4))

Tr<-rbind(fallT[,6:7],springT[,6:7])
TrU<-Tr[!duplicated(Tr$SVSPP),]
ages<-left_join(rbind(fallAges,springAges),TrU,by="SVSPP")



ages_seabirds<-filter(ages,grepl(paste(seabird_trawl,collapse="|"),
                                     ages$LOGGED_SPECIES_NAME,ignore.case=T))%>%
  dplyr::select(Year,CRUISE6,STRATUM,STATION,Species=LOGGED_SPECIES_NAME,SVSPP,season,LENGTH,AGE)%>%
  arrange(Year,Species)

ages_seabirds$Family<-sapply(ages_seabirds$Species,spNMFSCats)

ages_seabirds<-ages_seabirds[!is.na(ages_seabirds$AGE),]
ages_seabirds$season<-factor(ages_seabirds$season,levels=c("Spring","Fall"))


ggplot(ages_seabirds)+
  geom_point(aes(x=AGE,y=LENGTH,fill=season),shape=21,alpha=0.6)+
  facet_wrap(~Family)+
  scale_fill_brewer(palette = "Dark2",name="Trawl Season")+
  ylab("Length (cm)")+xlab("Age (yrs)")+
  guides(fill=guide_legend(override.aes = list(size=6)))

library(nlme)

nmfsVBs <- nlsList(LENGTH ~ Li*(1-exp(-1*k*(AGE-t0))) | Species,
                   data=ages_seabirds,
                   start=c(Li=249, k=0.26, t0=-1.10),
                   na.action=na.omit)
VBcoefs<-coef(nmfsVBs)
VBcoefs$Species<-rownames(VBcoefs)

vonB_preds<-data.frame(Species=numeric(length=151),Age=numeric(length=151),Length=numeric(length=151))
start=1

for (j in 1:9) {
  end=start+max(subset(ages_seabirds,Species==VBcoefs[j,4])$AGE)
  vonB_preds[start:end,1]<-as.character(VBcoefs[j,4])
  vonB_preds[start:end,2]<-seq(0,max(subset(ages_seabirds,Species==VBcoefs[j,4])$AGE))
  vonB_preds[start:end,3]<-vonB(VBcoefs[j,1],VBcoefs[j,2],VBcoefs[j,3],
                                seq(0,max(subset(ages_seabirds,Species==VBcoefs[j,4])$AGE)))
  start=start+(end-start+1)
}

ggplot()+
  geom_hline(yintercept=7.5,lty=2)+
  geom_point(data=ages_seabirds,aes(AGE,LENGTH,fill=Species),shape=21,size=2,alpha=0.2,show.legend=F)+
  geom_line(data=vonB_preds,aes(Age,Length,color=Species),lwd=1.25,show.legend=F)+
  #geom_point(data=vonB_preds,aes(Age,Length,fill=Species),shape=21,size=4,stroke=1.05,show.legend=F)+
  facet_wrap(~Species)+
  scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+
  ylab("Length (cm)")+xlab("Age (yrs)")

ggplot()+
  geom_hline(yintercept=7.5,lty=2)+
  geom_jitter(data=ages_seabirds,aes(AGE,LENGTH,fill=Species),shape=21,size=2,alpha=0.3,width=0.2,show.legend=F)+
  geom_line(data=vonB_preds,aes(Age,Length,color=Species),lwd=1.5,show.legend=F)+
  #geom_point(data=vonB_preds,aes(Age,Length,fill=Species),shape=21,size=4,stroke=1.05,show.legend=F)+
  scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+
  ylab("Length (cm)")+xlab("Age (yrs)")



#Same but with Von B Grouped at the family level
nmfsVB_fams <- nlsList(LENGTH ~ Li*(1-exp(-1*k*(AGE-t0))) | Family,
                       data=ages_seabirds,
                       start=c(Li=249, k=0.26, t0=-1.10),
                       na.action=na.omit)
VBcoefs_fams<-coef(nmfsVB_fams)
VBcoefs_fams$Family<-rownames(VBcoefs_fams)

vonB_fam_preds<-data.frame(Family=numeric(length=102),Age=numeric(length=102),Length=numeric(length=102))
start=1

for (j in 1:5) {
  end=start+max(subset(ages_seabirds,Family==VBcoefs_fams[j,4])$AGE)
  vonB_fam_preds[start:end,1]<-as.character(VBcoefs_fams[j,4])
  vonB_fam_preds[start:end,2]<-seq(0,max(subset(ages_seabirds,Family==VBcoefs_fams[j,4])$AGE))
  vonB_fam_preds[start:end,3]<-vonB(VBcoefs_fams[j,1],VBcoefs_fams[j,2],VBcoefs_fams[j,3],
                                seq(0,max(subset(ages_seabirds,Family==VBcoefs_fams[j,4])$AGE)))
  start=start+(end-start+1)
}

ggplot()+
  geom_hline(yintercept=7.5,lty=2)+
  geom_point(data=ages_seabirds,aes(AGE,LENGTH,fill=Family),shape=21,size=2,alpha=0.2,show.legend=F)+
  geom_line(data=vonB_fam_preds,aes(Age,Length,color=Family),lwd=1.25,show.legend=F)+
  #geom_point(data=vonB_fam_preds,aes(Age,Length,fill=Family),shape=21,size=4,stroke=1.05,show.legend=F)+
  facet_wrap(~Family)+
  scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+
  ylab("Length (cm)")+xlab("Age (yrs)")

ggplot()+
  geom_hline(yintercept=7.5,lty=2)+
  geom_jitter(data=ages_seabirds,aes(AGE,LENGTH,fill=Family),shape=21,size=2,alpha=0.3,width=0.2,show.legend=F)+
  geom_line(data=vonB_fam_preds,aes(Age,Length,color=Family),lwd=1.25,show.legend=F)+
  #geom_point(data=vonB_fam_preds,aes(Age,Length,fill=Family),shape=21,size=4,stroke=1.05,show.legend=F)+
  scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+
  ylab("Length (cm)")+xlab("Age (yrs)")


#Combination plot of them--facet family with species dots and lines
vonB_preds$Family<-sapply(vonB_preds$Species,spNMFSCats)

ggplot()+
  geom_hline(yintercept=7.5,lty=2)+
  geom_jitter(data=ages_seabirds,aes(AGE,LENGTH,fill=Species),width=0.2,shape=21,size=2,alpha=0.2,show.legend=F)+
  geom_line(data=vonB_preds,aes(Age,Length,color=Species),lwd=1.25,show.legend=F)+
  #geom_point(data=vonB_preds,aes(Age,Length,fill=Species),shape=21,size=4,stroke=1.05,show.legend=F)+
  facet_wrap(~Family)+
  scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+
  ylab("Length (cm)")+xlab("Age (yrs)")
