

### SUBSETTING FISHERIES DATASETS TO TERN CATCHABLE CPUE###

library(ggplot2)
library(plyr)
library(dwdplyr)
library(tidyr)
#

# Size range of tern catch ------------------------------------------------

setwd("/Users/aliyae/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Official Most Recent T-Feeding Document")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Official Most Recent T-Feeding Document")

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

#each species all time
EachSizeAllTime<-dfclean %>% 
  filter(!prey %in% c("")) %>% 
  group_by(prey) %>% 
  summarise(MaxSizeCM=max(size,na.rm=T), MinSizeCM=min(size,na.rm=T), MeanSizeCM=mean(size,na.rm=T))

#all species each year
AllSizeEachYear<-dfclean %>% 
  filter(!year %in% c("")) %>% 
  group_by(year) %>% 
  summarise(MaxSizeCM=max(size,na.rm=T), MinSizeCM=min(size,na.rm=T), MeanSizeCM=mean(size,na.rm=T))

#each species each year
EachSizeEachYear<-dfclean %>% 
  filter(!prey %in% c("")) %>% 
  group_by(year,prey) %>% 
  summarise(MaxSizeCM=max(size,na.rm=T), MinSizeCM=min(size,na.rm=T), MeanSizeCM=mean(size,na.rm=T))


#look at size distribution to determine max/min for subsetting
      #HERRING
percentile95<-quantile(subset(dfclean, prey=="herring" & size>0)$size, 0.95)
percentile99<-quantile(subset(dfclean, prey=="herring" & size>0)$size, 0.99)
ggplot(subset(dfclean, prey=="herring" & size>0))+
  geom_density(aes(x=size))+theme_bw()+
  geom_vline(xintercept=percentile95, color="red", linetype="dashed", size=1)+
  geom_vline(xintercept=percentile99, color="red", linetype="dashed", size=1)+
  labs(x ="size cm")



# 
# herringsub<-dfclean %>%
#   dplyr::filter(!year %in% c(2003, 2004, 2010, 2012, 2006, 2017)) %>%  #2006 and 2017 have <15 herring samples
#   dplyr::filter(prey %in% "herring")
# 
# library(RColorBrewer)
# nb.cols<-17
# mycolors <- colorRampPalette(brewer.pal(8, "BuGn"))(nb.cols)
# 
# ggplot(herringsub)+theme_bw()+
#   geom_density(aes(x=as.numeric(size), color=as.character(year), fill=as.character(year)),
#                adjust=3,alpha=0.7,size=1)+
#   scale_fill_manual(values=mycolors,name="year")+
#   scale_color_manual(values=mycolors,name="year")+
#   xlab("length (cm)")+
#   guides(color=guide_legend("year"))#+
#   facet_wrap(~year)


# Von Bert Curves and Size Bins  --------------------------------------------------------

vonB<-function(Li,k,t0,t) {
  l=Li*(1-exp(-1*k*(t-t0)))
}

      #Li = asymptotic size
      #k = growth coefficient
      #t0 = theoretical age when size is zero
      #t = age

#ATLANTIC HERRING - from Burbank et al. 2023
  #(https://www.mdpi.com/2410-3888/8/4/205)
  #this is using the values they found for fall spawners in the gulf of st lawrence
herrLi<-345.83 
herrk<-0.3
herrt0<- -1.1

herr_vonB<-vonB(herrLi,herrk,herrt0,seq(0,5))

herrDF<-data.frame(Length=herr_vonB,Age=seq(0,5))

par(mar=c(5,5,5,5))
plot(seq(0,5),herr_vonB,type="b")
abline(h=152,col="firebrick3") #Point of largest tern provision (19cm)

ggplot(herrDF)+
  theme_bw()+
  geom_line(aes(Age,Length),lwd=.5,color="grey30")+
  geom_point(aes(Age,Length),size=1)+
  geom_hline(yintercept=152, color = "red")+
  xlab("Age (yrs)")+ylab("Length (mm)")


    #calculate age at max and min tern sizes for year 0 then calculate sizes for time lags 
age <- function(Li, k, t0, l) {
  t = -(log(1 - l / Li) / k) + t0
  return(t)
}

max_length_lag0 = 152
min_length_lag0 = -8 #setting this to whatever produces a minimum closest to the real tern minimum (.9525cm) for year 0
ageMaxFall <- age(herrLi, herrk, herrt0, max_length_lag0) + (1/12) #add 1/12 of a year to account for growth between tern sampling (august) and fisheries sampling (october)
ageMinFall <- age(herrLi, herrk, herrt0, min_length_lag0) + (1/12)
ageMaxSpring <- age(herrLi, herrk, herrt0, max_length_lag0) + (8/12) #add 8/12 of a year to account for growth between tern sampling (august) and fisheries sampling (may)
ageMinSpring <- age(herrLi, herrk, herrt0, min_length_lag0) + (8/12)

          #calculate sizes at 1-5 lags fall
CatchableSizeBinsFall <- data.frame(
  lag_year = integer(),
  max_length = numeric(),
  min_length = numeric()
)

for (lag in 0:5) {
  max_length <- vonB(herrLi, herrk, herrt0, (ageMaxFall + lag))
  min_length <- vonB(herrLi, herrk, herrt0, (ageMinFall + lag))
  
  CatchableSizeBinsFall <- rbind(CatchableSizeBinsFall, data.frame(lag_year = lag, max_length = max_length, min_length = min_length))
}

        #calculate sizes at 1-5 lags spring
CatchableSizeBinsSpring <- data.frame(
  lag_year = integer(),
  max_length = numeric(),
  min_length = numeric()
)

for (lag in 0:5) {
  max_length <- vonB(herrLi, herrk, herrt0, (ageMaxSpring + lag))
  min_length <- vonB(herrLi, herrk, herrt0, (ageMinSpring + lag))
  
  CatchableSizeBinsSpring <- rbind(CatchableSizeBinsSpring, data.frame(lag_year = lag, max_length = max_length, min_length = min_length))
}


# 
# #Silver Hake - from nate
# hakeLi<-44.88
# hakek<-0.354
# haket0<-0.092
# 
# hake_vonB<-vonB(hakeLi,hakek,haket0,seq(0,20))
# 
# hakeDF<-data.frame(Length=hake_vonB,Age=seq(0,20))
# 
# par(mar=c(5,5,5,5))
# plot(seq(0,20),hake_vonB,type="b")
# abline(h=7.5,col="firebrick3") #Point of largest tern provision
# 
# ggplot(hakeDF)+
#   geom_line(aes(Age,Length),lwd=1.5,color="grey30")+
#   geom_point(aes(Age,Length),size=4)+
#   xlab("Age (yrs)")+ylab("Length (cm)")
# 
# #Northern Sand Lance - from nate
# NSLli<-249
# NSLk<-0.26
# NSLt0<--1.10
# 
# NSL_vonB<-vonB(NSLli,NSLk,NSLt0,seq(0,20))
# 
# NSLDF<-data.frame(Length=NSL_vonB,Age=seq(0,20),source="von Bertalanffy")
# 
# ggplot(NSLDF)+
#   geom_line(aes(Age,Length),lwd=1.5,color="grey30")+
#   geom_point(aes(Age,Length),size=4)+
#   xlab("Age (yrs)")+ylab("Length (cm)")
# 
# #Less than 1 year Sand Lance - from nate
# NSL_y0<-data.frame(Age=numeric(length=11),Length=numeric(length=11),source="Logistic")
# for (M in 0:10) {
#   NSL_y0[M+1,1]<-M/12
#   NSL_y0[M+1,2]<-(107.7/(1+exp((-0.71)*(M-5.17))))
# }
# 
# ggplot(NSL_y0)+
#   geom_point(aes(x=Age,y=Length))
# 
# allNSL<-bind_rows(NSL_y0,NSLDF[2:20,])
# ggplot(allNSL)+
#   geom_point(aes(x=Age,y=Length,fill=source),shape=21,size=5,stroke=1.1)+
#   scale_fill_brewer(palette="Set2",name="Source Equation")+
#   theme(legend.position=c(0.135,0.875),legend.background=element_rect(fill=rgb(0,0,0,alpha=0)))
# 
# 


# MEDMR Inshore Trawl Data ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

setwd("/Users/aliyae/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl")


# SUMMARY and CLEANING OF DFs #
#_____________________________#

#CATCH DATA#
DMR_raw<-read.csv("InshoreTrawlCatchData.csv")
DMR_raw$Date <- as.POSIXct(DMR_raw$Date)

    #how many tows per season per year
DMR_tow_num<-DMR_raw %>% 
  group_by(Season, Year) %>% 
  summarise(nTows=n_distinct(DMR_raw$Tow_Number)) #--> ok so they do 123 tows per survey

    #when are the surveys?
DMR_tows<-DMR_raw %>%
  select(Survey, Season, Year, Tow_Number, Region, Stratum,Date) %>% 
  filter(! Region %in% c(3,4,5)) %>% 
  distinct() %>%  
  mutate(mmddDate = substring(Date, 6,10),
         mmDate = substring(Date, 6,7))

DMR_tow_months<-DMR_tows %>% 
  group_by(Region,mmDate) %>% 
  summarise(n=n())

DMR_tow_days<-DMR_tows %>% 
  group_by(Region,mmddDate) %>% 
  summarise(n=n())

ggplot(DMR_tow_days, aes(x=mmddDate, y=n, fill=as.character(Region)))+theme_bw()+
  geom_histogram(stat="identity")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(fill='Region') 

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
#         #abundance
# ggplot(subset(DMR_catch_all,TotCatch > 50))+
#   geom_col(aes(x=Year, y=TotCatch, fill=Common_Name))+
#   theme_classic()+
#   ylab("Abundance")+xlab("Year")+
#   theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
#         legend.text = element_text(size=12),legend.title = element_text(size=15),
#         panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
#   scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
#   labs(fill="Prey")
# 
#         #proportion
# ggplot(DMR_catch_all)+
#   geom_col(aes(x=Year, y=Prop, fill=Common_Name))+
#   theme_classic()+
#   ylab("Proportion of Total")+xlab("Year")+
#   theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
#         legend.text = element_text(size=12),legend.title = element_text(size=15),
#         panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
#   scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))+
#   labs(fill="Prey")

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
        # 
        # #write data to csv 
        # library(readr)
        # #all species that terns eat
        # write_csv(DMR_catch_all_terns, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Fisheries Data/MEDMR Inshore Trawl/annual_DMR_summary.csv")
        # #just herring in the spring
        # DMR_catch_herr<-subset(DMR_catch_all_terns, Common_Name == "Herring Atlantic")
        # DMR_catch_herr<-subset(DMR_catch_herr, Season == "Spring")
        # write_csv(DMR_catch_herr, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Diet--Fisheries/Data/Fisheries Data/MEDMR Inshore Trawl/annual_DMR_herring_summary.csv")


# LENGTH DATA #
setwd("/Users/aliyae/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl")

DMR_met<-read.csv("InshoreTrawlLengthData.csv")
DMR_met_species<-as.data.frame(unique(DMR_met$Common_Name))
DMR_met <- DMR_met%>% 
  filter(!grepl("Astarte|Shrimp|Dollar|Star|Scallop|Crab|Krill|Jelly|Mussel|
                Snail|Clam|Lobster|Octopus|Pandalus|Cucumber|Urchin|Axius|
                Mysidacea|Cyclocardia|Lebbeid|Porbeagle|Anemone|Sponges|Skate", 
                Common_Name))
            # #year 0 proportion and abundance all species
        # DMR_met_all<-DMR_met %>% 
        #   dplyr::filter(Region == "1") %>% 
        #   dplyr::group_by(Season, Year, Common_Name) %>% 
        #   dplyr::summarise(TotCatch=n()) %>% 
        #   dplyr::mutate(Prop=TotCatch/sum(TotCatch))


# CALCULATE TERN CATCHABLE CPUE #
#_______________________________#

  # HERRING ONLY #
        #this work flow corrects the number of herring caught based on size ranges catchable by terns as determined by von bert curves
        #end goal is to make DF of all HERRING prop and abund for years 0-2 size corrected
        #the proportion of other species caught is NOT size corrected. so we have a df with the proportion of catchable herring in the total catch (of all sizes)
        #this workflow calculates one catchable proportion for each year and then applies that to every tow; alternatively, we could calculate a catch proportion for each individual tow and then apply those......... but this might not work well across other datasets?

setwd("/Users/aliyae/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl")
setwd("C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl")

met<-read.csv("InshoreTrawlLengthData.csv")
met_species<-as.data.frame(unique(met$Common_Name))
met <- met%>% 
  filter(!grepl("Astarte|Shrimp|Dollar|Star|Scallop|Crab|Krill|Jelly|Mussel|
                Snail|Clam|Lobster|Octopus|Pandalus|Cucumber|Urchin|Axius|
                Mysidacea|Cyclocardia|Lebbeid|Porbeagle|Anemone|Sponges|Skate", 
                Common_Name))
met$Common_Name[met$Common_Name == "Herring Atlantic"] <- 'Herring'
met$Common_Name[met$Common_Name == "Herring Blueback"] <- 'Herring'
met$Common_Name[met$Common_Name == "Alewife"] <- 'Herring'
met$LengthMM <- met$Length*10

raw<-read.csv("InshoreTrawlCatchData.csv") #bring in catch data
raw$Date <- as.POSIXct(raw$Date)
raw <- raw %>% 
  filter(!grepl("Astarte|Shrimp|Dollar|Star|Scallop|Crab|Krill|Jelly|Mussel|
                Snail|Clam|Lobster|Octopus|Pandalus|Cucumber|Urchin|Axius|
                Mysidacea|Cyclocardia|Lebbeid|Porbeagle|Anemone|Sponges|Skate", 
                Common_Name))
raw$Common_Name[raw$Common_Name == "Herring Atlantic"] <- 'Herring'
raw$Common_Name[raw$Common_Name == "Herring Blueback"] <- 'Herring'
raw$Common_Name[raw$Common_Name == "Alewife"] <- 'Herring'


#for year 0 proportion and abundance catchable CPUE FALL REGION 1#

      #df with all catch from x season y region
F_Reg1<-raw %>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Fall")

      #df with herring catch from x season y region
herr_F_Reg1<-F_Reg1 %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_F_Reg1<-met%>% #determin proportion of herring that are catchable by terns (i.e. proportion out of only herring)
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (0:152))/n())) #so we are pulling this length range from the tables we made of the relevant size bins;also, rounding down/up on the margins because the colon doesn't listen to decimal points only integers

herr_F_Reg1<-full_join(herr_F_Reg1,herr_PropCatch_F_Reg1) #add catchable proportion (prop out of all herring only) to herring data

herr_F_Reg1<-herr_F_Reg1%>% #determine number of herring that were catchable vs non-catchable 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_F_Reg1<-herr_F_Reg1%>% #subset out herring that are catchable
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable) #calling this number caught because i will rename it to "catchable" nex
herr_Catchable_F_Reg1$Common_Name<-"Catchable Herring"


herr_NonCatchable_F_Reg1<-herr_F_Reg1 %>% #subset out herring that are non-catchable
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_F_Reg1$Common_Name<-"Non-Catchable Herring"

noherr_F_Reg1<-F_Reg1 %>% #subset out non-herring from total catch
  filter(Common_Name != "Herring") #remove old herring numbers from the df

F_Reg1<-rbind(herr_Catchable_F_Reg1,herr_NonCatchable_F_Reg1) #add catchable and non-catchable herring back together
F_Reg1<-rbind(F_Reg1,noherr_F_Reg1) %>% #add herrings back to total catch
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) #calculate abundance

F_Reg1_totCatch<-F_Reg1 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

F_Reg1<-full_join(F_Reg1,F_Reg1_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_0_F_Reg1<-F_Reg1 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% 
  dplyr::rename(PropYr0=Prop,TotCatchYr0=Abund) %>% 
  dplyr::select(Year,PropYr0,TotCatchYr0)
        #this is the final df for year 0 fall region 1




#for year 1 proportion and abundance catchable CPUE FALL REGION 1#
      #note: this will override all of the DF names form above other than the final df (herr_0_F_Reg1)
F_Reg1<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Fall")
herr_F_Reg1<-F_Reg1 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_F_Reg1<-met%>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (90:206))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_F_Reg1<-full_join(herr_F_Reg1,herr_PropCatch_F_Reg1) 

herr_F_Reg1<-herr_F_Reg1%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_F_Reg1<-herr_F_Reg1%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_F_Reg1$Common_Name<-"Catchable Herring"


herr_NonCatchable_F_Reg1<-herr_F_Reg1 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_F_Reg1$Common_Name<-"Non-Catchable Herring"

noherr_F_Reg1<-F_Reg1 %>% 
  filter(Common_Name != "Herring") 

F_Reg1<-rbind(herr_Catchable_F_Reg1,herr_NonCatchable_F_Reg1) 
F_Reg1<-rbind(F_Reg1,noherr_F_Reg1) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

F_Reg1_totCatch<-F_Reg1 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

F_Reg1<-full_join(F_Reg1,F_Reg1_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_1_F_Reg1<-F_Reg1 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr1=Prop,TotCatchYr1=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr1,TotCatchYr1) #THIS LINE IS DIFFERENT FROM YR 0 CODE
      #final df for year 1 fall region 1



#for year 2 proportion and abundance catchable CPUE FALL REGION 1#
      #note: this will override all of the DF names form above other than the final df (herr_0_F_Reg1)
F_Reg1<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Fall")
herr_F_Reg1<-F_Reg1 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_F_Reg1<-met%>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (156:242))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_F_Reg1<-full_join(herr_F_Reg1,herr_PropCatch_F_Reg1) 

herr_F_Reg1<-herr_F_Reg1%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_F_Reg1<-herr_F_Reg1%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_F_Reg1$Common_Name<-"Catchable Herring"


herr_NonCatchable_F_Reg1<-herr_F_Reg1 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_F_Reg1$Common_Name<-"Non-Catchable Herring"

noherr_F_Reg1<-F_Reg1 %>% 
  filter(Common_Name != "Herring") 

F_Reg1<-rbind(herr_Catchable_F_Reg1,herr_NonCatchable_F_Reg1) 
F_Reg1<-rbind(F_Reg1,noherr_F_Reg1) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

F_Reg1_totCatch<-F_Reg1 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

F_Reg1<-full_join(F_Reg1,F_Reg1_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_2_F_Reg1<-F_Reg1 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr2=Prop,TotCatchYr2=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr2,TotCatchYr2) #THIS LINE IS DIFFERENT FROM YR 0 CODE
        #final df for year 2 fall region 1



#for year 3 proportion and abundance catchable CPUE FALL REGION 1#
      #note: this will override all of the DF names form above other than the final df (herr_0_F_Reg1)
F_Reg1<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Fall")
herr_F_Reg1<-F_Reg1 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_F_Reg1<-met%>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (205:267))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_F_Reg1<-full_join(herr_F_Reg1,herr_PropCatch_F_Reg1) 

herr_F_Reg1<-herr_F_Reg1%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_F_Reg1<-herr_F_Reg1%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_F_Reg1$Common_Name<-"Catchable Herring"


herr_NonCatchable_F_Reg1<-herr_F_Reg1 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_F_Reg1$Common_Name<-"Non-Catchable Herring"

noherr_F_Reg1<-F_Reg1 %>% 
  filter(Common_Name != "Herring") 

F_Reg1<-rbind(herr_Catchable_F_Reg1,herr_NonCatchable_F_Reg1) 
F_Reg1<-rbind(F_Reg1,noherr_F_Reg1) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

F_Reg1_totCatch<-F_Reg1 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

F_Reg1<-full_join(F_Reg1,F_Reg1_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_3_F_Reg1<-F_Reg1 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr3=Prop,TotCatchYr3=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr3,TotCatchYr3) #THIS LINE IS DIFFERENT FROM YR 0 CODE
        #final df for year 3 fall region 1



#for year 4 proportion and abundance catchable CPUE FALL REGION 1#
        #note: this will override all of the DF names form above other than the final df (herr_0_F_Reg1)
F_Reg1<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Fall")
herr_F_Reg1<-F_Reg1 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_F_Reg1<-met%>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (241:289))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_F_Reg1<-full_join(herr_F_Reg1,herr_PropCatch_F_Reg1) 

herr_F_Reg1<-herr_F_Reg1%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_F_Reg1<-herr_F_Reg1%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_F_Reg1$Common_Name<-"Catchable Herring"


herr_NonCatchable_F_Reg1<-herr_F_Reg1 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_F_Reg1$Common_Name<-"Non-Catchable Herring"

noherr_F_Reg1<-F_Reg1 %>% 
  filter(Common_Name != "Herring") 

F_Reg1<-rbind(herr_Catchable_F_Reg1,herr_NonCatchable_F_Reg1) 
F_Reg1<-rbind(F_Reg1,noherr_F_Reg1) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

F_Reg1_totCatch<-F_Reg1 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

F_Reg1<-full_join(F_Reg1,F_Reg1_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_4_F_Reg1<-F_Reg1 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr4=Prop,TotCatchYr4=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr4,TotCatchYr4) #THIS LINE IS DIFFERENT FROM YR 0 CODE
      #final df for year 4 fall region 1

#combine into single df and save#
herr_0_F_Reg1$Year<-as.integer(herr_0_F_Reg1$Year)
herr_1_F_Reg1$Year<-as.integer(herr_1_F_Reg1$Year)
herr_2_F_Reg1$Year<-as.integer(herr_2_F_Reg1$Year)
herr_3_F_Reg1$Year<-as.integer(herr_3_F_Reg1$Year)
herr_4_F_Reg1$Year<-as.integer(herr_4_F_Reg1$Year)

DMR_herr_lags01234_F_Reg1<-full_join(herr_0_F_Reg1, herr_1_F_Reg1, by='Year')
DMR_herr_lags01234_F_Reg1<-left_join(DMR_herr_lags01234_F_Reg1, herr_2_F_Reg1, by='Year')
DMR_herr_lags01234_F_Reg1<-left_join(DMR_herr_lags01234_F_Reg1, herr_3_F_Reg1, by='Year')
DMR_herr_lags01234_F_Reg1<-left_join(DMR_herr_lags01234_F_Reg1, herr_4_F_Reg1, by='Year') 


DMR_herr_lags01234_F_Reg1<-DMR_herr_lags01234_F_Reg1%>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(PropYr1lag = lag(PropYr1, n=1),
    TotCatchYr1lag =lag(TotCatchYr1, n=1),
    PropYr2lag = lag(PropYr2, n=2),
    TotCatchYr2lag = lag(TotCatchYr2, n=2),
    PropYr3lag = lag(PropYr3, n=3),
    TotCatchYr3lag = lag(TotCatchYr3, n=3),
    PropYr4lag = lag(PropYr4, n=4),
    TotCatchYr4lag = lag(TotCatchYr4, n=4))


library(readr)
write_csv(DMR_herr_lags0123_F_Reg1, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl/DMR_herr_lags01234_F_Reg1.csv")
write_csv(DMR_herr_lags0123_F_Reg1, "/Users/aliyae/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl/DMR_herr_lags01234_F_Reg1.csv")






#for year 0 proportion and abundance catchable CPUE Spring REGION 1#

    #df with all catch from x season y region
S_Reg1<-raw %>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Spring")

    #df with herring catch from x season y region
herr_S_Reg1<-S_Reg1 %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_S_Reg1<-met%>% #determin proportion of herring that are catchable by terns (i.e. proportion out of only herring)
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Spring") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (0:187))/n())) #so we are pulling this length range from the tables we made of the relevant size bins;also, rounding down/up on the margins because the colon doesn't listen to decimal points only integers

herr_S_Reg1<-full_join(herr_S_Reg1,herr_PropCatch_S_Reg1) #add catchable proportion (prop out of all herring only) to herring data

herr_S_Reg1<-herr_S_Reg1%>% #determine number of herring that were catchable vs non-catchable 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_S_Reg1<-herr_S_Reg1%>% #subset out herring that are catchable
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable) #calling this number caught because i will rename it to "catchable" nex
herr_Catchable_S_Reg1$Common_Name<-"Catchable Herring"


herr_NonCatchable_S_Reg1<-herr_S_Reg1 %>% #subset out herring that are non-catchable
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_S_Reg1$Common_Name<-"Non-Catchable Herring"

noherr_S_Reg1<-S_Reg1 %>% #subset out non-herring from total catch
  filter(Common_Name != "Herring") #remove old herring numbers from the df

S_Reg1<-rbind(herr_Catchable_S_Reg1,herr_NonCatchable_S_Reg1) #add catchable and non-catchable herring back together
S_Reg1<-rbind(S_Reg1,noherr_S_Reg1) %>% #add herrings back to total catch
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) #calculate abundance

S_Reg1_totCatch<-S_Reg1 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

S_Reg1<-full_join(S_Reg1,S_Reg1_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_0_S_Reg1<-S_Reg1 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% 
  dplyr::rename(PropYr0=Prop,TotCatchYr0=Abund) %>% 
  dplyr::select(Year,PropYr0,TotCatchYr0)
      #this is the final df for year 0 Spring region 1




#for year 1 proportion and abundance catchable CPUE Spring REGION 1#
    #note: this will override all of the DF names form above other than the final df (herr_0_S_Reg1)
S_Reg1<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Spring")
herr_S_Reg1<-S_Reg1 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_S_Reg1<-met%>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Spring") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (131:228))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_S_Reg1<-full_join(herr_S_Reg1,herr_PropCatch_S_Reg1) 

herr_S_Reg1<-herr_S_Reg1%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_S_Reg1<-herr_S_Reg1%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_S_Reg1$Common_Name<-"Catchable Herring"


herr_NonCatchable_S_Reg1<-herr_S_Reg1 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_S_Reg1$Common_Name<-"Non-Catchable Herring"

noherr_S_Reg1<-S_Reg1 %>% 
  filter(Common_Name != "Herring") 

S_Reg1<-rbind(herr_Catchable_S_Reg1,herr_NonCatchable_S_Reg1) 
S_Reg1<-rbind(S_Reg1,noherr_S_Reg1) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

S_Reg1_totCatch<-S_Reg1 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

S_Reg1<-full_join(S_Reg1,S_Reg1_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_1_S_Reg1<-S_Reg1 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr1=Prop,TotCatchYr1=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr1,TotCatchYr1) #THIS LINE IS DIFFERENT FROM YR 0 CODE
      #final df for year 1 Spring region 1
  


#for year 2 proportion and abundance catchable CPUE Spring REGION 1#
    #note: this will override all of the DF names form above other than the final df (herr_0_S_Reg1)
S_Reg1<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Spring")
herr_S_Reg1<-S_Reg1 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_S_Reg1<-met%>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Spring") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (187:259))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_S_Reg1<-full_join(herr_S_Reg1,herr_PropCatch_S_Reg1) 

herr_S_Reg1<-herr_S_Reg1%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_S_Reg1<-herr_S_Reg1%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_S_Reg1$Common_Name<-"Catchable Herring"


herr_NonCatchable_S_Reg1<-herr_S_Reg1 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_S_Reg1$Common_Name<-"Non-Catchable Herring"

noherr_S_Reg1<-S_Reg1 %>% 
  filter(Common_Name != "Herring") 

S_Reg1<-rbind(herr_Catchable_S_Reg1,herr_NonCatchable_S_Reg1) 
S_Reg1<-rbind(S_Reg1,noherr_S_Reg1) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

S_Reg1_totCatch<-S_Reg1 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

S_Reg1<-full_join(S_Reg1,S_Reg1_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_2_S_Reg1<-S_Reg1 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr2=Prop,TotCatchYr2=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr2,TotCatchYr2) #THIS LINE IS DIFFERENT FROM YR 0 CODE
      #final df for year 2 Spring region 1



#for year 3 proportion and abundance catchable CPUE Spring REGION 1#
    #note: this will override all of the DF names form above other than the final df (herr_0_S_Reg1)
S_Reg1<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Spring")
herr_S_Reg1<-S_Reg1 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_S_Reg1<-met%>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Spring") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (228:281))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_S_Reg1<-full_join(herr_S_Reg1,herr_PropCatch_S_Reg1) 

herr_S_Reg1<-herr_S_Reg1%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_S_Reg1<-herr_S_Reg1%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_S_Reg1$Common_Name<-"Catchable Herring"


herr_NonCatchable_S_Reg1<-herr_S_Reg1 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_S_Reg1$Common_Name<-"Non-Catchable Herring"

noherr_S_Reg1<-S_Reg1 %>% 
  filter(Common_Name != "Herring") 

S_Reg1<-rbind(herr_Catchable_S_Reg1,herr_NonCatchable_S_Reg1) 
S_Reg1<-rbind(S_Reg1,noherr_S_Reg1) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

S_Reg1_totCatch<-S_Reg1 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

S_Reg1<-full_join(S_Reg1,S_Reg1_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_3_S_Reg1<-S_Reg1 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr3=Prop,TotCatchYr3=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr3,TotCatchYr3) #THIS LINE IS DIFFERENT FROM YR 0 CODE
      #final df for year 3 Spring region 1



#for year 4 proportion and abundance catchable CPUE Spring REGION 1#
    #note: this will override all of the DF names form above other than the final df (herr_0_S_Reg1)
S_Reg1<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Season == "Spring")
herr_S_Reg1<-S_Reg1 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_S_Reg1<-met%>% 
  dplyr::filter(Region == "1") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Spring") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (259:298))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_S_Reg1<-full_join(herr_S_Reg1,herr_PropCatch_S_Reg1) 

herr_S_Reg1<-herr_S_Reg1%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_S_Reg1<-herr_S_Reg1%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_S_Reg1$Common_Name<-"Catchable Herring"


herr_NonCatchable_S_Reg1<-herr_S_Reg1 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_S_Reg1$Common_Name<-"Non-Catchable Herring"

noherr_S_Reg1<-S_Reg1 %>% 
  filter(Common_Name != "Herring") 

S_Reg1<-rbind(herr_Catchable_S_Reg1,herr_NonCatchable_S_Reg1) 
S_Reg1<-rbind(S_Reg1,noherr_S_Reg1) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

S_Reg1_totCatch<-S_Reg1 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

S_Reg1<-full_join(S_Reg1,S_Reg1_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_4_S_Reg1<-S_Reg1 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr4=Prop,TotCatchYr4=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr4,TotCatchYr4) #THIS LINE IS DIFFERENT FROM YR 0 CODE
      #final df for year 4 Spring region 1

  #combine into single df and save#
herr_0_S_Reg1$Year<-as.integer(herr_0_S_Reg1$Year)
herr_1_S_Reg1$Year<-as.integer(herr_1_S_Reg1$Year)
herr_2_S_Reg1$Year<-as.integer(herr_2_S_Reg1$Year)
herr_3_S_Reg1$Year<-as.integer(herr_3_S_Reg1$Year)
herr_4_S_Reg1$Year<-as.integer(herr_4_S_Reg1$Year)

DMR_herr_lags01234_S_Reg1<-full_join(herr_0_S_Reg1, herr_1_S_Reg1, by='Year')
DMR_herr_lags01234_S_Reg1<-left_join(DMR_herr_lags01234_S_Reg1, herr_2_S_Reg1, by='Year')
DMR_herr_lags01234_S_Reg1<-left_join(DMR_herr_lags01234_S_Reg1, herr_3_S_Reg1, by='Year')
DMR_herr_lags01234_S_Reg1<-left_join(DMR_herr_lags01234_S_Reg1, herr_4_S_Reg1, by='Year') 


DMR_herr_lags01234_S_Reg1<-DMR_herr_lags01234_S_Reg1%>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(PropYr1lag = lag(PropYr1, n=1),
                TotCatchYr1lag =lag(TotCatchYr1, n=1),
                PropYr2lag = lag(PropYr2, n=2),
                TotCatchYr2lag = lag(TotCatchYr2, n=2),
                PropYr3lag = lag(PropYr3, n=3),
                TotCatchYr3lag = lag(TotCatchYr3, n=3),
                PropYr4lag = lag(PropYr4, n=4),
                TotCatchYr4lag = lag(TotCatchYr4, n=4))


library(readr)
write_csv(DMR_herr_lags0123_S_Reg1, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl/DMR_herr_lags01234_S_Reg1.csv")
write_csv(DMR_herr_lags0123_S_Reg1, "/Users/aliyae/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl/DMR_herr_lags01234_S_Reg1.csv")





#for year 0 proportion and abundance catchable CPUE FALL REGIONS 1&2#

#df with all catch from x season y region
F_Reg12<-raw %>% 
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Season == "Fall")

#df with herring catch from x season y region
herr_F_Reg12<-F_Reg12 %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_F_Reg12<-met%>% #determin proportion of herring that are catchable by terns (i.e. proportion out of only herring)
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (0:152))/n())) #so we are pulling this length range from the tables we made of the relevant size bins;also, rounding down/up on the margins because the colon doesn't listen to decimal points only integers

herr_F_Reg12<-full_join(herr_F_Reg12,herr_PropCatch_F_Reg12) #add catchable proportion (prop out of all herring only) to herring data

herr_F_Reg12<-herr_F_Reg12%>% #determine number of herring that were catchable vs non-catchable 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_F_Reg12<-herr_F_Reg12%>% #subset out herring that are catchable
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable) #calling this number caught because i will rename it to "catchable" nex
herr_Catchable_F_Reg12$Common_Name<-"Catchable Herring"


herr_NonCatchable_F_Reg12<-herr_F_Reg12 %>% #subset out herring that are non-catchable
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_F_Reg12$Common_Name<-"Non-Catchable Herring"

noherr_F_Reg12<-F_Reg12 %>% #subset out non-herring from total catch
  filter(Common_Name != "Herring") #remove old herring numbers from the df

F_Reg12<-rbind(herr_Catchable_F_Reg12,herr_NonCatchable_F_Reg12) #add catchable and non-catchable herring back together
F_Reg12<-rbind(F_Reg12,noherr_F_Reg12) %>% #add herrings back to total catch
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) #calculate abundance

F_Reg12_totCatch<-F_Reg12 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

F_Reg12<-full_join(F_Reg12,F_Reg12_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_0_F_Reg12<-F_Reg12 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% 
  dplyr::rename(PropYr0=Prop,TotCatchYr0=Abund) %>% 
  dplyr::select(Year,PropYr0,TotCatchYr0)
#this is the final df for year 0 fall REGIONS 1&2




#for year 1 proportion and abundance catchable CPUE FALL REGIONS 1&2#
#note: this will override all of the DF names form above other than the final df (herr_0_F_Reg12)
F_Reg12<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Season == "Fall")
herr_F_Reg12<-F_Reg12 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_F_Reg12<-met%>% 
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (90:206))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_F_Reg12<-full_join(herr_F_Reg12,herr_PropCatch_F_Reg12) 

herr_F_Reg12<-herr_F_Reg12%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_F_Reg12<-herr_F_Reg12%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_F_Reg12$Common_Name<-"Catchable Herring"


herr_NonCatchable_F_Reg12<-herr_F_Reg12 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_F_Reg12$Common_Name<-"Non-Catchable Herring"

noherr_F_Reg12<-F_Reg12 %>% 
  filter(Common_Name != "Herring") 

F_Reg12<-rbind(herr_Catchable_F_Reg12,herr_NonCatchable_F_Reg12) 
F_Reg12<-rbind(F_Reg12,noherr_F_Reg12) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

F_Reg12_totCatch<-F_Reg12 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

F_Reg12<-full_join(F_Reg12,F_Reg12_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_1_F_Reg12<-F_Reg12 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr1=Prop,TotCatchYr1=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr1,TotCatchYr1) #THIS LINE IS DIFFERENT FROM YR 0 CODE
#final df for year 1 fall REGIONS 1&2



#for year 2 proportion and abundance catchable CPUE FALL REGIONS 1&2#
#note: this will override all of the DF names form above other than the final df (herr_0_F_Reg12)
F_Reg12<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Season == "Fall")
herr_F_Reg12<-F_Reg12 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_F_Reg12<-met%>% 
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (156:242))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_F_Reg12<-full_join(herr_F_Reg12,herr_PropCatch_F_Reg12) 

herr_F_Reg12<-herr_F_Reg12%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_F_Reg12<-herr_F_Reg12%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_F_Reg12$Common_Name<-"Catchable Herring"


herr_NonCatchable_F_Reg12<-herr_F_Reg12 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_F_Reg12$Common_Name<-"Non-Catchable Herring"

noherr_F_Reg12<-F_Reg12 %>% 
  filter(Common_Name != "Herring") 

F_Reg12<-rbind(herr_Catchable_F_Reg12,herr_NonCatchable_F_Reg12) 
F_Reg12<-rbind(F_Reg12,noherr_F_Reg12) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

F_Reg12_totCatch<-F_Reg12 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

F_Reg12<-full_join(F_Reg12,F_Reg12_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_2_F_Reg12<-F_Reg12 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr2=Prop,TotCatchYr2=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr2,TotCatchYr2) #THIS LINE IS DIFFERENT FROM YR 0 CODE
#final df for year 2 fall REGIONS 1&2



#for year 3 proportion and abundance catchable CPUE FALL REGIONS 1&2#
#note: this will override all of the DF names form above other than the final df (herr_0_F_Reg12)
F_Reg12<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Season == "Fall")
herr_F_Reg12<-F_Reg12 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_F_Reg12<-met%>% 
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (205:267))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_F_Reg12<-full_join(herr_F_Reg12,herr_PropCatch_F_Reg12) 

herr_F_Reg12<-herr_F_Reg12%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_F_Reg12<-herr_F_Reg12%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_F_Reg12$Common_Name<-"Catchable Herring"


herr_NonCatchable_F_Reg12<-herr_F_Reg12 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_F_Reg12$Common_Name<-"Non-Catchable Herring"

noherr_F_Reg12<-F_Reg12 %>% 
  filter(Common_Name != "Herring") 

F_Reg12<-rbind(herr_Catchable_F_Reg12,herr_NonCatchable_F_Reg12) 
F_Reg12<-rbind(F_Reg12,noherr_F_Reg12) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

F_Reg12_totCatch<-F_Reg12 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

F_Reg12<-full_join(F_Reg12,F_Reg12_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_3_F_Reg12<-F_Reg12 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr3=Prop,TotCatchYr3=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr3,TotCatchYr3) #THIS LINE IS DIFFERENT FROM YR 0 CODE
#final df for year 3 fall REGIONS 1&2



#for year 4 proportion and abundance catchable CPUE FALL REGIONS 1&2#
#note: this will override all of the DF names form above other than the final df (herr_0_F_Reg12)
F_Reg12<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Season == "Fall")
herr_F_Reg12<-F_Reg12 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_F_Reg12<-met%>% 
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Fall") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (241:289))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_F_Reg12<-full_join(herr_F_Reg12,herr_PropCatch_F_Reg12) 

herr_F_Reg12<-herr_F_Reg12%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_F_Reg12<-herr_F_Reg12%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_F_Reg12$Common_Name<-"Catchable Herring"


herr_NonCatchable_F_Reg12<-herr_F_Reg12 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_F_Reg12$Common_Name<-"Non-Catchable Herring"

noherr_F_Reg12<-F_Reg12 %>% 
  filter(Common_Name != "Herring") 

F_Reg12<-rbind(herr_Catchable_F_Reg12,herr_NonCatchable_F_Reg12) 
F_Reg12<-rbind(F_Reg12,noherr_F_Reg12) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

F_Reg12_totCatch<-F_Reg12 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

F_Reg12<-full_join(F_Reg12,F_Reg12_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_4_F_Reg12<-F_Reg12 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr4=Prop,TotCatchYr4=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr4,TotCatchYr4) #THIS LINE IS DIFFERENT FROM YR 0 CODE
#final df for year 4 fall REGIONS 1&2

#combine into single df and save#
herr_0_F_Reg12$Year<-as.integer(herr_0_F_Reg12$Year)
herr_1_F_Reg12$Year<-as.integer(herr_1_F_Reg12$Year)
herr_2_F_Reg12$Year<-as.integer(herr_2_F_Reg12$Year)
herr_3_F_Reg12$Year<-as.integer(herr_3_F_Reg12$Year)
herr_4_F_Reg12$Year<-as.integer(herr_4_F_Reg12$Year)

DMR_herr_lags01234_F_Reg12<-full_join(herr_0_F_Reg12, herr_1_F_Reg12, by='Year')
DMR_herr_lags01234_F_Reg12<-left_join(DMR_herr_lags01234_F_Reg12, herr_2_F_Reg12, by='Year')
DMR_herr_lags01234_F_Reg12<-left_join(DMR_herr_lags01234_F_Reg12, herr_3_F_Reg12, by='Year')
DMR_herr_lags01234_F_Reg12<-left_join(DMR_herr_lags01234_F_Reg12, herr_4_F_Reg12, by='Year') 


DMR_herr_lags01234_F_Reg12<-DMR_herr_lags01234_F_Reg12%>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(PropYr1lag = lag(PropYr1, n=1),
                TotCatchYr1lag =lag(TotCatchYr1, n=1),
                PropYr2lag = lag(PropYr2, n=2),
                TotCatchYr2lag = lag(TotCatchYr2, n=2),
                PropYr3lag = lag(PropYr3, n=3),
                TotCatchYr3lag = lag(TotCatchYr3, n=3),
                PropYr4lag = lag(PropYr4, n=4),
                TotCatchYr4lag = lag(TotCatchYr4, n=4))


library(readr)
write_csv(DMR_herr_lags0123_F_Reg12, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl/DMR_herr_lags01234_F_Reg12.csv")
write_csv(DMR_herr_lags0123_F_Reg12, "/Users/aliyae/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl/DMR_herr_lags01234_F_Reg12.csv")






#for year 0 proportion and abundance catchable CPUE Spring REGIONS 1&2#

#df with all catch from x season y region
S_Reg12<-raw %>% 
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Season == "Spring")

#df with herring catch from x season y region
herr_S_Reg12<-S_Reg12 %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_S_Reg12<-met%>% #determin proportion of herring that are catchable by terns (i.e. proportion out of only herring)
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Spring") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (0:187))/n())) #so we are pulling this length range from the tables we made of the relevant size bins;also, rounding down/up on the margins because the colon doesn't listen to decimal points only integers

herr_S_Reg12<-full_join(herr_S_Reg12,herr_PropCatch_S_Reg12) #add catchable proportion (prop out of all herring only) to herring data

herr_S_Reg12<-herr_S_Reg12%>% #determine number of herring that were catchable vs non-catchable 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_S_Reg12<-herr_S_Reg12%>% #subset out herring that are catchable
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable) #calling this number caught because i will rename it to "catchable" nex
herr_Catchable_S_Reg12$Common_Name<-"Catchable Herring"


herr_NonCatchable_S_Reg12<-herr_S_Reg12 %>% #subset out herring that are non-catchable
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_S_Reg12$Common_Name<-"Non-Catchable Herring"

noherr_S_Reg12<-S_Reg12 %>% #subset out non-herring from total catch
  filter(Common_Name != "Herring") #remove old herring numbers from the df

S_Reg12<-rbind(herr_Catchable_S_Reg12,herr_NonCatchable_S_Reg12) #add catchable and non-catchable herring back together
S_Reg12<-rbind(S_Reg12,noherr_S_Reg12) %>% #add herrings back to total catch
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) #calculate abundance

S_Reg12_totCatch<-S_Reg12 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

S_Reg12<-full_join(S_Reg12,S_Reg12_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_0_S_Reg12<-S_Reg12 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% 
  dplyr::rename(PropYr0=Prop,TotCatchYr0=Abund) %>% 
  dplyr::select(Year,PropYr0,TotCatchYr0)
#this is the final df for year 0 Spring REGIONS 1&2






#for year 1 proportion and abundance catchable CPUE Spring REGIONS 1&2#
    #note: this will override all of the DF names form above other than the final df (herr_0_S_Reg12)
S_Reg12<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Season == "Spring")
herr_S_Reg12<-S_Reg12 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_S_Reg12<-met%>% 
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Spring") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (131:228))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_S_Reg12<-full_join(herr_S_Reg12,herr_PropCatch_S_Reg12) 

herr_S_Reg12<-herr_S_Reg12%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_S_Reg12<-herr_S_Reg12%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_S_Reg12$Common_Name<-"Catchable Herring"


herr_NonCatchable_S_Reg12<-herr_S_Reg12 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_S_Reg12$Common_Name<-"Non-Catchable Herring"

noherr_S_Reg12<-S_Reg12 %>% 
  filter(Common_Name != "Herring") 

S_Reg12<-rbind(herr_Catchable_S_Reg12,herr_NonCatchable_S_Reg12) 
S_Reg12<-rbind(S_Reg12,noherr_S_Reg12) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

S_Reg12_totCatch<-S_Reg12 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

S_Reg12<-full_join(S_Reg12,S_Reg12_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_1_S_Reg12<-S_Reg12 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr1=Prop,TotCatchYr1=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr1,TotCatchYr1) #THIS LINE IS DIFFERENT FROM YR 0 CODE
        #final df for year 1 Spring REGIONS 1&2



#for year 2 proportion and abundance catchable CPUE Spring REGIONS 1&2#
    #note: this will override all of the DF names form above other than the final df (herr_0_S_Reg12)
S_Reg12<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Season == "Spring")
herr_S_Reg12<-S_Reg12 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_S_Reg12<-met%>% 
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Spring") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (187:259))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_S_Reg12<-full_join(herr_S_Reg12,herr_PropCatch_S_Reg12) 

herr_S_Reg12<-herr_S_Reg12%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_S_Reg12<-herr_S_Reg12%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_S_Reg12$Common_Name<-"Catchable Herring"


herr_NonCatchable_S_Reg12<-herr_S_Reg12 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_S_Reg12$Common_Name<-"Non-Catchable Herring"

noherr_S_Reg12<-S_Reg12 %>% 
  filter(Common_Name != "Herring") 

S_Reg12<-rbind(herr_Catchable_S_Reg12,herr_NonCatchable_S_Reg12) 
S_Reg12<-rbind(S_Reg12,noherr_S_Reg12) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

S_Reg12_totCatch<-S_Reg12 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

S_Reg12<-full_join(S_Reg12,S_Reg12_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_2_S_Reg12<-S_Reg12 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr2=Prop,TotCatchYr2=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr2,TotCatchYr2) #THIS LINE IS DIFFERENT FROM YR 0 CODE
        #final df for year 2 Spring REGIONS 1&2



#for year 3 proportion and abundance catchable CPUE Spring REGIONS 1&2#
    #note: this will override all of the DF names form above other than the final df (herr_0_S_Reg12)
S_Reg12<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Season == "Spring")
herr_S_Reg12<-S_Reg12 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_S_Reg12<-met%>% 
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Spring") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (228:281))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_S_Reg12<-full_join(herr_S_Reg12,herr_PropCatch_S_Reg12) 

herr_S_Reg12<-herr_S_Reg12%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_S_Reg12<-herr_S_Reg12%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_S_Reg12$Common_Name<-"Catchable Herring"


herr_NonCatchable_S_Reg12<-herr_S_Reg12 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_S_Reg12$Common_Name<-"Non-Catchable Herring"

noherr_S_Reg12<-S_Reg12 %>% 
  filter(Common_Name != "Herring") 

S_Reg12<-rbind(herr_Catchable_S_Reg12,herr_NonCatchable_S_Reg12) 
S_Reg12<-rbind(S_Reg12,noherr_S_Reg12) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

S_Reg12_totCatch<-S_Reg12 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

S_Reg12<-full_join(S_Reg12,S_Reg12_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_3_S_Reg12<-S_Reg12 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr3=Prop,TotCatchYr3=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr3,TotCatchYr3) #THIS LINE IS DIFFERENT FROM YR 0 CODE
        #final df for year 3 Spring REGIONS 1&2



#for year 4 proportion and abundance catchable CPUE Spring REGIONS 1&2#
    #note: this will override all of the DF names form above other than the final df (herr_0_S_Reg12)
S_Reg12<-raw %>% #need to run these again cuz we renamed them above
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Season == "Spring")
herr_S_Reg12<-S_Reg12 %>% #need to run these again cuz we renamed them above
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)

herr_PropCatch_S_Reg12<-met%>% 
  dplyr::filter(Region == "1" | Region == "2") %>% 
  dplyr::filter(Common_Name == "Herring") %>% 
  dplyr::filter(Season == "Spring") %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(PropCatchable=(sum(LengthMM %in% (259:298))/n())) #THIS LINE IS DIFFERENT FROM yr 0 CODE

herr_S_Reg12<-full_join(herr_S_Reg12,herr_PropCatch_S_Reg12) 

herr_S_Reg12<-herr_S_Reg12%>% 
  group_by(Year) %>% 
  mutate(Number_Catchable=Number_Caught*PropCatchable) %>% 
  mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchable))) 

herr_Catchable_S_Reg12<-herr_S_Reg12%>% 
  select(Year,Common_Name,Number_Catchable) %>% 
  rename(Number_Caught=Number_Catchable)
herr_Catchable_S_Reg12$Common_Name<-"Catchable Herring"


herr_NonCatchable_S_Reg12<-herr_S_Reg12 %>% 
  select(Year,Common_Name,Number_NonCatchable) %>% 
  rename(Number_Caught=Number_NonCatchable)
herr_NonCatchable_S_Reg12$Common_Name<-"Non-Catchable Herring"

noherr_S_Reg12<-S_Reg12 %>% 
  filter(Common_Name != "Herring") 

S_Reg12<-rbind(herr_Catchable_S_Reg12,herr_NonCatchable_S_Reg12) 
S_Reg12<-rbind(S_Reg12,noherr_S_Reg12) %>% 
  dplyr::group_by(Year, Common_Name) %>% 
  dplyr::summarise(Abund=sum(Number_Caught)) 

S_Reg12_totCatch<-S_Reg12 %>% #calculate total catch for each year
  group_by(Year) %>% 
  na.omit() %>% 
  dplyr::summarise(AnnualCatch=sum(Abund))

S_Reg12<-full_join(S_Reg12,S_Reg12_totCatch) %>% #add total annual catches back into df and calculate proportional abundance of each prey item
  mutate(Prop=Abund/AnnualCatch)

herr_4_S_Reg12<-S_Reg12 %>% #pull out only the abundance and relative proportion of catchable herring 
  dplyr::filter(Common_Name == "Catchable Herring") %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::rename(PropYr4=Prop,TotCatchYr4=Abund) %>% #THIS LINE IS DIFFERENT FROM YR 0 CODE
  dplyr::select(Year,PropYr4,TotCatchYr4) #THIS LINE IS DIFFERENT FROM YR 0 CODE
        #final df for year 4 Spring REGIONS 1&2

  #combine into single df and save#
herr_0_S_Reg12$Year<-as.integer(herr_0_S_Reg12$Year)
herr_1_S_Reg12$Year<-as.integer(herr_1_S_Reg12$Year)
herr_2_S_Reg12$Year<-as.integer(herr_2_S_Reg12$Year)
herr_3_S_Reg12$Year<-as.integer(herr_3_S_Reg12$Year)
herr_4_S_Reg12$Year<-as.integer(herr_4_S_Reg12$Year)

DMR_herr_lags01234_S_Reg12<-full_join(herr_0_S_Reg12, herr_1_S_Reg12, by='Year')
DMR_herr_lags01234_S_Reg12<-left_join(DMR_herr_lags01234_S_Reg12, herr_2_S_Reg12, by='Year')
DMR_herr_lags01234_S_Reg12<-left_join(DMR_herr_lags01234_S_Reg12, herr_3_S_Reg12, by='Year')
DMR_herr_lags01234_S_Reg12<-left_join(DMR_herr_lags01234_S_Reg12, herr_4_S_Reg12, by='Year') 


DMR_herr_lags01234_S_Reg12<-DMR_herr_lags01234_S_Reg12%>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(PropYr1lag = lag(PropYr1, n=1),
                TotCatchYr1lag =lag(TotCatchYr1, n=1),
                PropYr2lag = lag(PropYr2, n=2),
                TotCatchYr2lag = lag(TotCatchYr2, n=2),
                PropYr3lag = lag(PropYr3, n=3),
                TotCatchYr3lag = lag(TotCatchYr3, n=3),
                PropYr4lag = lag(PropYr4, n=4),
                TotCatchYr4lag = lag(TotCatchYr4, n=4))


library(readr)
write_csv(DMR_herr_lags0123_S_Reg12, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl/DMR_herr_lags01234_S_Reg12.csv")
write_csv(DMR_herr_lags0123_S_Reg12, "/Users/aliyae/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl/DMR_herr_lags01234_S_Reg12.csv")


#OLD CODE THAT IS MESSY
#       #for year 0 proportion and abundance catchable CPUE FALL REGION 1#
# DMR_herr_prop_catchable_Fall_Region1<-DMR_met%>% #determin proportion of herring that are catchable by terns
#   dplyr::filter(Region == "1") %>% 
#   dplyr::filter(Common_Name %in% c("Herring Atlantic","Herring Blueback","Alewife")) %>% 
#   dplyr::filter(Season == "Fall") %>% 
#   dplyr::group_by(Year) %>% 
#   dplyr::summarise(PropCatchableYr0=(sum(Length < 15.68)/n())) #so we are pulling this from the tables we made of the relevant size bins
#           #this is the proportion of catchable herring out of all fish caught (not proportion of catchable herring out of CATCHABLE other species)
# 
# DMR_raw<-read.csv("InshoreTrawlCatchData.csv") #bring in catch data
# DMR_raw$Date <- as.POSIXct(DMR_raw$Date)
# DMR_raw <- DMR_raw %>% 
#   filter(!grepl("Astarte|Shrimp|Dollar|Star|Scallop|Crab|Krill|Jelly|Mussel|
#                 Snail|Clam|Lobster|Octopus|Pandalus|Cucumber|Urchin|Axius|
#                 Mysidacea|Cyclocardia|Lebbeid|Porbeagle|Anemone|Sponges|Skate", 
#                 Common_Name))
# DMR_catch_all_Fall_Region1<-DMR_raw %>% 
#   dplyr::filter(Region == "1") %>% 
#   dplyr::filter(Season == "Fall")
# 
# DMR_catch_all_Fall_Region1$Common_Name[DMR_catch_all_Fall_Region1$Common_Name == "Herring Atlantic"] <- 'Herring'
# DMR_catch_all_Fall_Region1$Common_Name[DMR_catch_all_Fall_Region1$Common_Name == "Herring Blueback"] <- 'Herring'
# DMR_catch_all_Fall_Region1$Common_Name[DMR_catch_all_Fall_Region1$Common_Name == "Alewife"] <- 'Herring'
#           #combined all herring species; should I just do atlantic herring? 
# 
# DMR_catch_herr_Fall_Region1<-DMR_catch_all_Fall_Region1 %>% 
#   dplyr::filter(Common_Name == "Herring") %>% 
#   dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)
# 
# DMR_catch_herr_prop_Fall_Region1<-full_join(DMR_catch_herr_Fall_Region1,DMR_herr_prop_catchable_Fall_Region1) #join catch data with calculation of catchable proportion
# 
# DMR_catch_herr_prop_Fall_Region1<-DMR_catch_herr_prop_Fall_Region1 %>% #determine number that were catchable vs non-catchable from total catch
#   group_by(Year) %>% 
#   mutate(Number_Catchable=Number_Caught*PropCatchableYr0) %>% 
#   mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchableYr0)))
# 
# DMR_catchable_herr_Fall_Region1<-DMR_catch_herr_prop_Fall_Region1 %>% #renamed number catchable to number caught
#   select(Year,Common_Name,Number_Catchable) %>% 
#   rename(Number_Caught=Number_Catchable)
# 
# DMR_non_catchable_herr_Fall_Region1<-DMR_catch_herr_prop_Fall_Region1 %>% 
#   select(Year,Common_Name,Number_NonCatchable) %>% 
#   rename(Number_Caught=Number_NonCatchable)
# DMR_non_catchable_herr_Fall_Region1$Common_Name<-"Non-Catchable Herring"
# 
# DMR_catch_no_herr_Fall_Region1<-DMR_catch_all_Fall_Region1 %>% 
#   filter(Common_Name != "Herring")
# 
# DMR_catch_all_Fall_Region1<-rbind(DMR_catchable_herr_Fall_Region1,DMR_non_catchable_herr_Fall_Region1)
# DMR_catch_all_Fall_Region1<-rbind(DMR_catch_all_Fall_Region1,DMR_catch_no_herr_Fall_Region1)
# 
# DMR_catch_all_Fall_Region1<-DMR_catch_all_Fall_Region1%>% 
#   dplyr::group_by(Year, Common_Name) %>% 
#   dplyr::summarise(TotCatch=sum(Number_Caught),n=n()) %>% 
#   na.omit() %>% 
#   dplyr::mutate(Prop=n/sum(n))
# 
# DMR_herr_0_Fall_Region1<-DMR_catch_all_Fall_Region1 %>% 
#   dplyr::filter(Common_Name == "Herring") %>% 
#   #dplyr::filter(Region == "1") %>% 
#   #dplyr::filter(Season == "Fall") %>% 
#   #::group_by(Year) %>% 
#   #dplyr::summarise(TotCatchYr0=n()) %>% 
#   #dplyr::mutate(PropYr0=TotCatchYr0/sum(TotCatchYr0)) %>% 
#   dplyr::rename(PropYr0=Prop,TotCatchYr0=TotCatch) %>% 
#   dplyr::select(Year,PropYr0,TotCatchYr0)
# 
# 
# 
#     #for year 1 proportion and abundance catchable CPUE#
# DMR_herr_prop_catchable_1_Fall_Region1<-DMR_met%>% 
#   dplyr::filter(Region == "1") %>% 
#   dplyr::filter(Common_Name %in% c("Herring Atlantic","Herring Blueback","Alewife")) %>% 
#   dplyr::filter(Season == "Fall") %>% 
#   dplyr::group_by(Year) %>% 
#   dplyr::summarise(PropCatchableYr1=(sum(Length %in% (9.02:20.58))/n())) #lengths 16-20cm (lower and upper size ranges from year 0 plus 1 year of growth from VB curve)
# 
# DMR_catch_all_1_Fall_Region1<-DMR_raw %>% 
#   dplyr::filter(Region == "1") %>% 
#   dplyr::filter(Season == "Fall")
# 
# DMR_catch_all_1_Fall_Region1$Common_Name[DMR_catch_all_1_Fall_Region1$Common_Name == "Herring Atlantic"] <- 'Herring'
# DMR_catch_all_1_Fall_Region1$Common_Name[DMR_catch_all_1_Fall_Region1$Common_Name == "Herring Blueback"] <- 'Herring'
# DMR_catch_all_1_Fall_Region1$Common_Name[DMR_catch_all_1_Fall_Region1$Common_Name == "Alewife"] <- 'Herring'
# 
# DMR_catch_herr_1_Fall_Region1<-DMR_catch_all_1_Fall_Region1 %>% 
#   dplyr::filter(Common_Name == "Herring") %>% 
#   dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)
# 
# DMR_catch_herr_prop_1_Fall_Region1<-full_join(DMR_catch_herr_1_Fall_Region1,DMR_herr_prop_catchable_1_Fall_Region1) 
# 
# DMR_catch_herr_prop_1_Fall_Region1<-DMR_catch_herr_prop_1_Fall_Region1 %>% 
#   group_by(Year) %>% 
#   mutate(Number_Catchable=Number_Caught*PropCatchableYr1) %>% 
#   mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchableYr1)))
# 
# DMR_catchable_herr_1_Fall_Region1<-DMR_catch_herr_prop_1_Fall_Region1 %>% 
#   select(Year,Common_Name,Number_Catchable) %>% 
#   rename(Number_Caught=Number_Catchable)
# 
# DMR_non_catchable_herr_1_Fall_Region1<-DMR_catch_herr_prop_1_Fall_Region1 %>% 
#   select(Year,Common_Name,Number_NonCatchable) %>% 
#   rename(Number_Caught=Number_NonCatchable)
# DMR_non_catchable_herr_1_Fall_Region1$Common_Name<-"Non-Catchable Herring"
# 
# DMR_catch_no_herr_1_Fall_Region1<-DMR_catch_all_1_Fall_Region1 %>% 
#   filter(Common_Name != "Herring")
# 
# DMR_catch_all_1_Fall_Region1<-rbind(DMR_catchable_herr_1_Fall_Region1,DMR_non_catchable_herr_1_Fall_Region1)
# DMR_catch_all_1_Fall_Region1<-rbind(DMR_catch_all_1_Fall_Region1,DMR_catch_no_herr_1_Fall_Region1)
# 
# DMR_catch_all_1_Fall_Region1<-DMR_catch_all_1_Fall_Region1%>% 
#   dplyr::group_by(Year, Common_Name) %>% 
#   dplyr::summarise(TotCatch=sum(Number_Caught),n=n()) %>% 
#   na.omit() %>% 
#   dplyr::mutate(Prop=n/sum(n))
# 
# DMR_herr_1_Fall_Region1<-DMR_catch_all_1_Fall_Region1 %>% 
#   dplyr::filter(Common_Name == "Herring") 
# 
# DMR_herr_1_Fall_Region1<-as.data.frame(as.matrix(DMR_herr_1_Fall_Region1))
# 
# DMR_herr_1_Fall_Region1<-DMR_herr_1_Fall_Region1%>% 
#   dplyr::mutate(PropYr1=lag(Prop,n=1),TotCatchYr1=lag(TotCatch,n=1))%>% 
#   dplyr::select(Year,PropYr1,TotCatchYr1)
# 
# 
#     # for year 2 proportion and abundance catchable CPUE #
# DMR_herr_prop_catchable_2_Fall_Region1<-DMR_met%>% 
#   dplyr::filter(Region == "1") %>% 
#   dplyr::filter(Common_Name %in% c("Herring Atlantic","Herring Blueback","Alewife")) %>% 
#   dplyr::filter(Season == "Fall") %>% 
#   dplyr::group_by(Year) %>% 
#   dplyr::summarise(PropCatchableYr2=(sum(Length %in% (15.64:24.21))/n()))
# 
# DMR_catch_all_2_Fall_Region1<-DMR_raw %>% #bring in catch data
#   dplyr::filter(Region == "1") %>% 
#   dplyr::filter(Season == "Fall")
# 
# DMR_catch_all_2_Fall_Region1$Common_Name[DMR_catch_all_2_Fall_Region1$Common_Name == "Herring Atlantic"] <- 'Herring'
# DMR_catch_all_2_Fall_Region1$Common_Name[DMR_catch_all_2_Fall_Region1$Common_Name == "Herring Blueback"] <- 'Herring'
# DMR_catch_all_2_Fall_Region1$Common_Name[DMR_catch_all_2_Fall_Region1$Common_Name == "Alewife"] <- 'Herring'
# 
# DMR_catch_herr_2_Fall_Region1<-DMR_catch_all_2_Fall_Region1 %>% 
#   dplyr::filter(Common_Name == "Herring") %>% 
#   dplyr::select(Year,Common_Name,Number_Caught,Expanded_Catch)
# 
# DMR_catch_herr_prop_2_Fall_Region1<-full_join(DMR_catch_herr_2_Fall_Region1,DMR_herr_prop_catchable_2_Fall_Region1) 
# 
# DMR_catch_herr_prop_2_Fall_Region1<-DMR_catch_herr_prop_2_Fall_Region1 %>% 
#   group_by(Year) %>% 
#   mutate(Number_Catchable=Number_Caught*PropCatchableYr2) %>% 
#   mutate(Number_NonCatchable=(Number_Caught-(Number_Caught*PropCatchableYr2)))
# 
# DMR_catchable_herr_2_Fall_Region1<-DMR_catch_herr_prop_2_Fall_Region1 %>% 
#   select(Year,Common_Name,Number_Catchable) %>% 
#   rename(Number_Caught=Number_Catchable)
# 
# DMR_non_catchable_herr_2_Fall_Region1<-DMR_catch_herr_prop_2_Fall_Region1 %>% 
#   select(Year,Common_Name,Number_NonCatchable) %>% 
#   rename(Number_Caught=Number_NonCatchable)
# DMR_non_catchable_herr_2_Fall_Region1$Common_Name<-"Non-Catchable Herring"
# 
# DMR_catch_no_herr_2_Fall_Region1<-DMR_catch_all_2_Fall_Region1 %>% 
#   filter(Common_Name != "Herring")
# 
# DMR_catch_all_2_Fall_Region1<-rbind(DMR_catchable_herr_2_Fall_Region1,DMR_non_catchable_herr_2_Fall_Region1)
# DMR_catch_all_2_Fall_Region1<-rbind(DMR_catch_all_2_Fall_Region1,DMR_catch_no_herr_2_Fall_Region1)
# 
# DMR_catch_all_2_Fall_Region1<-DMR_catch_all_2_Fall_Region1%>% 
#   dplyr::group_by(Year, Common_Name) %>% 
#   dplyr::summarise(TotCatch=sum(Number_Caught),n=n()) %>% 
#   na.omit() %>% 
#   dplyr::mutate(Prop=n/sum(n))
# 
# DMR_herr_2_Fall_Region1<-DMR_catch_all_2_Fall_Region1 %>% 
#   dplyr::filter(Common_Name == "Herring") 
# 
# DMR_herr_2_Fall_Region1<-as.data.frame(as.matrix(DMR_herr_2_Fall_Region1))
# 
# DMR_herr_2_Fall_Region1<-DMR_herr_2_Fall_Region1%>% 
#   dplyr::mutate(PropYr2=lag(Prop,n=2),TotCatchYr2=lag(TotCatch,n=2)) %>% 
#   dplyr::select(Year,PropYr2,TotCatchYr2)
# 
#     #combine into single df and save#
# DMR_herr_1_Fall_Region1$Year<-as.integer(DMR_herr_1_Fall_Region1$Year)
# DMR_herr_2_Fall_Region1$Year<-as.integer(DMR_herr_2_Fall_Region1$Year)
# 
# DMR_herr_lags012_Fall_Region1<-full_join(DMR_herr_0_Fall_Region1, DMR_herr_1_Fall_Region1, by='Year')
# DMR_herr_lags012_Fall_Region1<-left_join(DMR_herr_lags012_Fall_Region1, DMR_herr_2_Fall_Region1, by='Year')
# 
# library(readr)
# write_csv(DMR_herr_lags012, "C:/Users/aec1075/OneDrive - USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl/DMR_herring_size_corrected.csv")
# write_csv(DMR_herr_lags012, "/Users/aliyae/Library/CloudStorage/OneDrive-USNH/PhD UNH/Research/Tern fisheries interaction/TernFisheriesInteractions/Data/Fisheries Data/MEDMR Inshore Trawl/DMR_herring_size_corrected.csv")
