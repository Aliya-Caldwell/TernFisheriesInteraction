#------------------------------------------------------------------------------------------------------------
########### RELATIVE ABUNDANCE COMPARISONS ############
#------------------------------------------------------------------------------------------------------------
rm(list=ls())
getwd()
library(ggplot2)
library(plyr)
library(dplyr)

########## CRATE TABLE w BOTH DATASETS ABUNDANCES ##################

#------tern relative abundance (#caught from each family per nest hrs)------

setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
tprov<-read.csv("cleaned tern provisioning.csv")
ternfeeding<-subset(tprov,Species=="Herring"|Species=="Hake"|Species=="Butterfish"|
                      Species=="Pollock"|Species=="Mackerel"|Species=="Sandlance")
ternfeedingprops<-ternfeeding %>% 
  dplyr::group_by(Year, Species.2) %>%
  dplyr::summarise(n=n()) %>% 
  dplyr::mutate(prop=n/sum(n)) %>% 
  dplyr::mutate(dataset="terns") %>% 
  dplyr::rename(Family=Species.2) %>% 
  dplyr::mutate_at(.vars=vars(Family),.funs=funs(toupper))

#tern feeding sampling effort
seffort<-read.csv("Tern provisioning sampling effort.csv")
effort<-seffort %>% 
  dplyr::mutate(totnesthrs=Tot.nest.hrs) %>% 
  dplyr::select(Year,totnesthrs)

#combine sampling effort and tern data
ternrelabundance<-merge(ternfeedingprops,effort) %>% 
  dplyr::mutate(relabundance=(n/totnesthrs)) %>% 
  dplyr::rename(effort=totnesthrs) 

#------fisheries relative abundance FALL ---------
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Prepped Fisheries Data")
fisheries<-read.csv("GoM_NMFS_preppedData.csv")
fisheries<-filter(fisheries, !Year %in% c(1963:1998))
fisheriesfall<-subset(fisheries, season=="FALL")%>% 
  dplyr::mutate(dataset="fisheries") 

fisheriesrelabundance<-fisheriesfall %>% 
  dplyr::select(Year, Family, Catch_Num, Prop, dataset, Trawls,Rel_Num,) %>%
  dplyr::rename(n=Catch_Num) %>% 
  dplyr::rename(prop=Prop) %>%
  dplyr::rename(relabundance=Rel_Num) %>% 
  dplyr::rename(effort=Trawls)
#------fisheries relative abundance SPRING ---------
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Prepped Fisheries Data")
fisheries<-read.csv("GoM_NMFS_preppedData.csv")
fisheries<-filter(fisheries, !Year %in% c(1963:1998))
fisheriesspring<-subset(fisheries, season=="SPRING")%>% 
  dplyr::mutate(dataset="fisheries") 

fisheriesrelabundance<-fisheriesspring %>% 
  dplyr::select(Year, Family, Catch_Num, Prop, dataset, Trawls,Rel_Num,) %>%
  dplyr::rename(n=Catch_Num) %>% 
  dplyr::rename(prop=Prop) %>%
  dplyr::rename(relabundance=Rel_Num) %>% 
  dplyr::rename(effort=Trawls)

#------fall fisheries relative abundance CORRECTED FOR SUMMER SIZE and also uncluding PROPORTIONS---------
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Prepped Fisheries Data")
fisheriesadj<-read.csv("GoM_adjCatch_ternsized.csv")
fisheriesadj<-filter(fisheriesadj, !Year %in% c(1963:1998))

fisheriesadjfall<-subset(fisheriesadj, season=="FALL")%>% 
  dplyr::mutate(dataset="fisheries") %>% 
  dplyr::mutate(effort=as.numeric("NA"))

fisheriesadjrelabundance<-fisheriesadjfall %>% 
  dplyr::select(Year, Family, famCatch_Num, famProp, dataset, effort, famcatchable_RelNum,) %>%
  dplyr::rename(n=famCatch_Num) %>% 
  dplyr::rename(prop=famProp) %>%
  dplyr::rename(relabundance=famcatchable_RelNum) 

#---combine fisheries and tern datasets----
    #had to put a ghost column for effort into the fisheries dataset otherwise these two wouldnt combine
    #and the error "column 'effort' cant be converted from character to numeric" is coming up so then i added as.numeric to it
    #and now it works but dont be doing anything with that effort column without thinking abt this
abundances<-bind_rows(fisheriesadjrelabundance, ternrelabundance)
    #dont change the name of this table from "abundances" because it is referenced way down in the code to make a figure

#------spring fisheries relative abundance CORRECTED FOR SUMMER SIZE and also uncluding PROPORTIONS---------
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Prepped Fisheries Data")
fisheriesadj<-read.csv("GoM_adjCatch_ternsized.csv")
fisheriesadj<-filter(fisheriesadj, !Year %in% c(1963:1998))

fisheriesadjspring<-subset(fisheriesadj, season=="SPRING")%>% 
  dplyr::mutate(dataset="fisheries") %>% 
  dplyr::mutate(effort=as.numeric("NA"))

fisheriesadjrelabundance<-fisheriesadjspring %>% 
  dplyr::select(Year, Family, famCatch_Num, famProp, dataset, effort, famcatchable_RelNum,) %>%
  dplyr::rename(n=famCatch_Num) %>% 
  dplyr::rename(prop=famProp) %>%
  dplyr::rename(relabundance=famcatchable_RelNum) 

#---combine fisheries and tern datasets----
#had to put a ghost column for effort into the fisheries dataset otherwise these two wouldnt combine
#and the error "column 'effort' cant be converted from character to numeric" is coming up so then i added as.numeric to it
#and now it works but dont be doing anything with that effort column without thinking abt this
springabundances<-bind_rows(fisheriesadjrelabundance, ternrelabundance)
#dont change the name of this table from "abundances" because it is referenced way down in the code to make a figure

####### COMPARE PROPORTIONS STATISTICALLY ########
  #want to compare the proportiond in the tern diet to those in the fisheries diet by year
tryatest<-subset(abundances, Year=="2008")
chisq.test(tryatest$dataset,tryatest$prop)

#-------### SCATTERPLOT OF ABUNDANCES with CORRELATION TESTS #####-------------------------------
  #using a new csv file which is just a rearrangement of the table from above entitled "abundances"
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
dat<-read.csv("Tern_fisheries_data_all.csv")

#remove low tern sample size years 
data<-filter(dat, !Year %in% c(2003, 2004, 2010, 2012))

#################### ADJUSTED FALL ################

#hake (fisheries N labeled on top of year and tern N labeled below)
hakedata<-subset(data, Family=="HAKE")
  #Correlation coefficient
kendall<-cor.test(hakedata$t_abundance,hakedata$f_abundance_adjfall,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,hakedata$f_abundance_adjfall,method="spearman")
head(spearman)
  #plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjfall))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjfall), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Hake", aes(x=0.05,y=470), size=5, face="plain")+
  geom_text(label="spearmans: rho= - 0.46, p= 0.05", aes(x=0.2, y=470), face="plain")
  
  #Correlation coefficient
kendall<-cor.test(hakedata$t_abundance,hakedata$f_abundance_adjfall,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,hakedata$f_abundance_adjfall,method="spearman")
head(spearman)

#herring
herrdata<-subset(data, Family=="HERRING")
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjfall))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjfall), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Herring", aes(x=0.15,y=100), size=5)
  
  #Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjfall,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjfall,method="spearman")
head(spearman)

### ADJUSTED SPRING #########
#hake (fisheries N labeled on top of year and tern N labeled below)
hakedata<-subset(data, Family=="HAKE")
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjspring))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjspring), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Hake", aes(x=0.1,y=300), size=5)

  #Correlation coefficient
kendall<-cor.test(hakedata$t_abundance,herrdata$f_abundance_adjspring,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,herrdata$f_abundance_adjspring,method="spearman")
head(spearman)

#herring
herrdata<-subset(data, Family=="HERRING")
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjspring))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjspring), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Herring", aes(x=0.15,y=100), size=5)+ylim(0,100)

  #Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjspring,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjspring,method="spearman")
head(spearman)

### NON-ADJUSTED SPRING ####
  #didnt do correlation coeffficients on these yet cuz i dont think they necessarily make sense to do when theres no lag
#hake (fisheries N labeled on top of year and tern N labeled below)
hakedata<-subset(data, Family=="HAKE")
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring))+
  geom_text(aes(label=Year, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=0, size=3)+
  geom_text(aes(label=f_N_spring, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=25, size=3)+
  geom_text(aes(label=t_N, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=-25, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+
  geom_text(label="Hake\n pts labeled fisheries N, year, tern N", aes(x=0.15,y=1280), size=5)

#herring
herrdata<-subset(data, Family=="HERRING")
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring))+
  geom_text(aes(label=Year, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=0, size=3)+
  geom_text(aes(label=f_N_spring, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=10, size=3)+
  geom_text(aes(label=t_N, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=-10, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+
  geom_text(label="Herring\n pts labeled fisheries N, year, tern N", aes(x=0.15,y=480), size=5)

### NON- ADJUSTED FALL ####
#hake (fisheries N labeled on top of year and tern N labeled below)
hakedata<-subset(data, Family=="HAKE")
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall))+
  geom_text(aes(label=Year, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=0, size=3)+
  geom_text(aes(label=f_N_fall, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=20, size=3)+
  geom_text(aes(label=t_N, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=-20, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1500)+
  geom_text(label="Hake\n pts labeled fisheries N, year, tern N", aes(x=0.15,y=1500), size=5)

#herring
herrdata<-subset(data, Family=="HERRING")
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall))+
  geom_text(aes(label=Year, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=0, size=3)+
  geom_text(aes(label=f_N_fall, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=20, size=3)+
  geom_text(aes(label=t_N, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=-20, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Herring\n pts labeled fisheries N, year, tern N", aes(x=0.15,y=1000), size=5)

### NON- ADJUSTED LAG FALL #### 
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
dat<-read.csv("Tern_fisheries_data_all_lagged.csv")
herrdata<-subset(dat, Family=="HERRING")
hakedata<-subset(dat, Family=="HAKE")

#herring 1 year lag
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Herring", aes(x=0.15,y=1000), size=5)
  
  #Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag1,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag1,method="spearman")
head(spearman)

#herring 2 year lag
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag2))+
  #geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("NMFS data (at provisioning sample yr + 2yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Fall Herring Abundance", aes(x=0.06, y=990), size=5)+
  geom_text(label="kendalls: tau=0.31, p=0.08; spearmans: rho=0.45, p=0.06", aes(x=0.13,y=960), size=4)

  #Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag2,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag2,method="spearman")
head(spearman)

#herring 3 year lag
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag2.1))+
  ylab("NMFS data (at provisioning sample yr + 3yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Fall Herring Abundance", aes(x=0.06, y=990), size=5)+
  geom_text(label="kendalls: tau=0.30, p=0.08; spearmans: rho=0.41, p=0.09", aes(x=0.13,y=960), size=4)

  #Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag2.1,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag2.1,method="spearman")
head(spearman)

#hake 1 year lag
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#hake 2 year lag
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag2))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#hake 3 year lag
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag2.1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag2.1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

### NON- ADJUSTED LAG SPRING #### 
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
dat<-read.csv("Tern_fisheries_data_all_lagged.csv")
herrdata<-subset(dat, Family=="HERRING")
hakedata<-subset(dat, Family=="HAKE")

#herring 1 year lag
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Herring", aes(x=0.15,y=1000), size=5)

#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag1,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag1,method="spearman")
head(spearman)

#herring 2 year lag
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_lag2))+
  ylab("NMFS data (at provisioning sample yr + 2yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Spring Herring Abundance", aes(x=0.06, y=990), size=5)+
  geom_text(label="kendalls: tau=0.37, p=0.03*; spearmans: rho=0.51, p=0.03*", aes(x=0.126,y=960), size=4)

#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag2,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag2,method="spearman")
head(spearman)

#herring 3 year lag
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_lag3))+
  ylab("NMFS data (at provisioning sample yr + 3yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Spring Herring Abundance", aes(x=0.06, y=990), size=5)+
  geom_text(label="kendalls: tau=0.26, p=0.13; spearmans: rho=0.37, p=0.13", aes(x=0.122,y=960), size=4)

#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag3,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag3,method="spearman")
head(spearman)

#hake 1 year lag
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#hake 2 year lag
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag2))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#hake 3 year lag
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag2.1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag2.1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

-------------------------------------------------
########### CATCH MAGNITUDE/ RELATIVE PROPORTION ############
#------------------------------------------------------------------------------------------------------------
rm(list=ls())
getwd()
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
library(ggplot2)
library(plyr)
library(dplyr)

tprov<-read.csv("cleaned tern provisioning.csv")

### fish per year relative to effort (in this case effort= number of blind hrs) ###
#csv file with #fish obs per year and tot blind time per yr 
ntimeunfiltered<-read.csv("Tern provisioning sampling effort.csv")
#filter out yrs w low sample size
ntime<-filter(ntimeunfiltered, !Year %in% c(2003, 2004, 2010, 2012))
#generate table w column for total obs/hr for each year
ntfish<-ntime %>% 
  dplyr::mutate(fishperhrs=Total.fish.observed/Tot.nest.hrs)
#plot table
ggplot(ntfish)+theme_bw()+
  geom_point(aes(x=ntfish$Year, y=ntfish$fishperhrs))+
  geom_line(aes(x=ntfish$Year, y=ntfish$fishperhrs))+xlab("Year")+ylab("fish per nest hr")
#plot table but different
#this is messed up
ggplot(ntfish, aes(fill="fish", x=Year, y=fishperhrs))+theme_bw(color="red")+
  geom_bar(position="dodge",stat="identity")
  +ylab("Catch Magnitude (fish/nest hr)")+xlab("Year")


### not sure what i was doing here but it seems like i was trying to get the number of each
#family group caught per year relative to the sampling effort (nest hrs)

#generate table w just the year and nest hrs
ternmag<-ntime %>% 
  dplyr::mutate(totnesthrs=Tot.nest.hrs) %>% 
  dplyr::select(Year,totnesthrs)

#get tern prop table
terns<-subset(tprov, Species.lumped=="fish")
ternprops<-terns %>% 
  dplyr::group_by(Year, Species.2) %>%
  dplyr::summarise(n=n()) %>% 
  dplyr::mutate(prop=n/sum(n)) %>% 
  dplyr::mutate(dataset="terns") %>% 
  dplyr::rename(Family=Species.2) %>% 
  dplyr::mutate_at(.vars=vars(Family),.funs=funs(toupper))

#append values for magnitude to tern prop table to get relative tern props
#i.e. number from each family caught devided by total nest hrs for each year
#this is convoluted BS ?
relternmag<-merge(ternprops, ternmag) %>% 
  dplyr::mutate(relmag=(n*prop/totnesthrs))

#------------------------------------------------------------------------------------------------------------
######################## SPECIES PROPORTIONS #########
#------------------------------------------------------------------------------------------------------------
getwd()
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")

library(ggplot2)
library(plyr)
library(dplyr)
library(grid)

tprov<-read.csv("cleaned tern provisioning.csv")

#-->subset out fish 
fishunfilt<-subset(tprov, Species.lumped=="fish")

#-->exclude 2003, 2010, 2012, and maybe 2004 due to small sample sizes
fish<-filter(fishunfilt, !Year %in% c(2003, 2004, 2010, 2012))

### proportions for top 6 sp plus "other" sp and unknown sp for yrs w suitable n ###

#-->calculate proportions
fprop<-fish %>% 
  dplyr::group_by(Year, Species.2) %>%
  dplyr::summarise(n=n()) %>% 
  dplyr::mutate(prop=n/sum(n))
  

#plot from saved CSV of table produced by above code 
  #need to eventually figure out how to make ggplot allow me to plot
  #this directly from the table produced-->
  #down below theres a version of this same thing where you dont need to read in a new csv
fishprop<-read.csv("Species prop.csv")
ggplot(fishprop)+ 
  geom_col(aes(x=Year, y=freq, fill=Species))+theme_bw()+
  ylab("Proportion of Total Observations")+    theme(axis.text = element_text(size=12), axis.title = element_text(size=15), 
                                                     legend.text = element_text(size=12),legend.title = element_text(size=15),
                                                     panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))

### plot "landings" on top of proportions ###--------------------------------------------------------------------------------------------

ternsunfilt<-subset(tprov,Species.lumped=="fish")
terns<-filter(ternsunfilt, !Year %in% c(2003, 2004, 2010, 2012))

#tern props
ternprop<-terns %>% 
  dplyr::group_by(Year, Species.2) %>%
  dplyr::summarise(n=n()) %>% 
  dplyr::mutate(prop=n/sum(n)) %>% 
  dplyr::mutate(dataset="terns") %>% 
  dplyr::rename(Family=Species.2) %>% 
  dplyr::mutate_at(.vars=vars(Family),.funs=funs(toupper))

#csv file with #fish obs per year and tot blind time per yr 
ntimeunfiltered<-read.csv("Tern provisioning sampling effort.csv")
#filter out yrs w low sample size
ntime<-filter(ntimeunfiltered, !Year %in% c(2003, 2004, 2010, 2012))

#generate table w column for total obs/hr for each year
ntfish<-ntime %>% 
  dplyr::mutate(fishperhrs=Total.fish.observed/Tot.nest.hrs)

#plot together
library(palettetown)
ggplot()+
  geom_col(data=ternprop,aes(x=Year, y=prop, fill=Family))+theme_bw()+ scale_fill_poke(pokemon = 'Charizard', spread = 8)+
  geom_point(data=ntfish,aes(x=Year, y=fishperhrs/2))+
  geom_line(data=ntfish,aes(x=Year, y=fishperhrs/2))+
  ylab("Proportion of Total Observations")+theme(axis.text = element_text(size=12), axis.title = element_text(size=15), 
                               legend.text = element_text(size=12),legend.title = element_text(size=15),
                                panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01),name="Year")+
  scale_y_continuous(expand=c(0,0.001),
                     name="Proportion in diet", sec.axis=sec_axis(~.*2, name="Colony landings (fish per nest hr)"))


### COMBINE FISHERIES AND TPROV DATA TO COMPARE ###----------------------------------------------
  #this is all using non-corrected numberies i.e. raw proportions based on total number caught per trawl 
  #or observed per year
tprov<-read.csv("cleaned tern provisioning.csv")
ntimeunfiltered<-read.csv("Tern provisioning sampling effort.csv")

setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Prepped Fisheries Data")
setwd("/Users/aliyacaldwell/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Prepped Fisheries Data")

fisheriesunfilt<-read.csv("100kmbuffer_NMFS_all&ternData.csv")

#subset tprov data
ternsunfilt<-subset(tprov,Species=="Herring"|Species=="Hake"|Species=="Butterfish"|
                      Species=="Pollock"|Species=="Mackerel"|Species=="Sandlance")
terns<-filter(ternsunfilt, !Year %in% c(2003, 2004, 2010, 2012))

#subset fisheries data
fisheries<-filter(fisheriesunfilt, !Year %in% c(1963:1998,2003, 2004, 2010, 2012))

#proportion calcs for tprov data
ternprop<-terns %>% 
  dplyr::group_by(Year, Species.2) %>%
  dplyr::summarise(n=n()) %>% 
  dplyr::mutate(prop=n/sum(n)) %>% 
  dplyr::mutate(dataset="terns") %>% 
  dplyr::rename(Family=Species.2) %>% 
  dplyr::mutate_at(.vars=vars(Family),.funs=funs(toupper))
  

#proportion calcs for fisheries data
fisheriesprop<-fisheries%>%
  dplyr::group_by(Year)%>%
  dplyr::mutate(Total=sum(Catch_Num))%>%
  dplyr::group_by(Year,Family)%>%
  dplyr::mutate(prop=Catch_Num/Total) %>% 
  dplyr::select(Year, Family, Catch_Num, prop) %>% 
  dplyr::mutate(dataset="fisheries") %>% 
  dplyr::rename(n=Catch_Num) 


#combine fisheries and terns prop datasets
allprops<-bind_rows(fisheriesprop, ternprop)

#plot side by side
ggplot(ternprop)+ 
  geom_col(aes(x=Year, y=prop, fill=Family))+theme_bw()+scale_fill_poke(pokemon = 'Charizard', spread = 6)+
  ylab("Proportion of Total Observations")+ theme(axis.text = element_text(size=12), axis.title = element_text(size=15), 
                                                     legend.text = element_text(size=12),legend.title = element_text(size=15),
                                                     panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))

ggplot(fisheriesprop)+ 
  geom_col(aes(x=Year, y=prop, fill=Family))+theme_bw()+scale_fill_poke(pokemon = 'Charizard', spread = 6)+
  ylab("Proportion of Total Observations")+theme(axis.text = element_text(size=12), axis.title = element_text(size=15), 
                                                     legend.text = element_text(size=12),legend.title = element_text(size=15),
                                                     panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_continuous(expand=c(0,0.01))+scale_y_continuous(expand=c(0,0.001))

#-----------PLOT PROPORTIONS USING DATASET GENERATED FROM ABUNDANCE CALC ------------
  #using datatable produced called "abundances"

ggplot(abundances)+
  geom_col(data=subset(abundances, dataset=="fisheries"),
          aes(x=Year, y=prop, fill=Family))+theme_bw()+
  geom_col(data=subset(abundances, dataset="terns"),
           aes(x=Year,y=prop,fill=Family))+
  facet_wrap(~dataset)
  #this is ugly, make it less ugly when less lazy :)


#------------------------------------------------------------------------------------------------------------
################# SIZE DISTRIBUTIONs ##############------------------------------------
#------------------------------------------------------------------------------------------------------------
rm(list=ls())
getwd()
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
library(ggplot2)
library(plyr)
library(dplyr)
library(pals)
library(wesanderson)
library(colorspace)
library(RColorBrewer)

tprov<-read.csv("cleaned tern provisioning.csv")

### plot of size distribution for top 6 species across all years ###
#-->subset for top 6 sp and filter out failed feeding events
fishsubunfilt<-subset(tprov, Species=="Herring"|Species=="Hake"|Species=="Butterfish"|
                  Species=="Pollock"|Species=="Mackerel"|Species=="Sandlance")
fishsub<-filter(fishsubunfilt, !Event %in% c("failed feeding"))

#plot mult panes
ggplot(fishsub)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=Species, fill=Species), 
               adjust=3,alpha=0.3)+
  facet_wrap(~Species)+
  scale_fill_manual(values = wes_palette("Darjeeling1", 6, "continuous"))+
  scale_color_manual(values = wes_palette("Darjeeling1", 6, "continuous"))+
  xlab("Length (cm)")

#plot single pane
ggplot(fishsub)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=Species, fill=Species), 
               adjust=3,alpha=0.3)+
  #facet_wrap(~Species)+
  #scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set2")+
  scale_fill_manual(values = wes_palette("Darjeeling1", 6, "continuous"))+
  scale_color_manual(values = wes_palette("Darjeeling1", 6, "continuous"))
  xlab("Length (cm)")


### HERRING length by yr ###

#create color palette
  

nb.cols<-18
mycolors <- colorRampPalette(brewer.pal(8, "BuGn"))(nb.cols)

#subset out herring
herring<-subset(fishsub, Species=="Herring")

#plot mult panes
ggplot(herring)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.7)+
  scale_fill_manual(values=mycolors,name="year")+scale_color_manual(values=mycolors,name="year")+
  xlab("Herring Length (cm)")+guides(color=guide_legend("year"))+
  facet_wrap(~Year)
#single pane plot
ggplot(herring)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.6)+
  scale_fill_manual(values=mycolors,name="year")+scale_color_manual(values=mycolors,name="year")+
  xlab("Herring Length (cm)")+guides(color=guide_legend("year"))

----------------------------
### HAKE length by year ###

nb.cols<-18
mycolors <- colorRampPalette(brewer.pal(8, "GnBu"))(nb.cols)

hake<-subset(fishsub, Species=="Hake")

#mult panes
ggplot(hake)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.7)+
  facet_wrap(~Year)+scale_fill_manual(values=mycolors,name="year")+scale_color_manual(values=mycolors,name="year")+
  xlab("Hake Length (cm)")+guides(color=guide_legend("year"))
#single paned plot
ggplot(hake)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.7)+
  scale_fill_manual(values=mycolors, name="year")+scale_color_manual(values=mycolors, name="Year")+
  xlab("Hake Length (cm)")+guides(color=guide_legend("year"))

----------------------------
### BUTTERFISH length by yr ###

nb.cols<-19
mycolors <- colorRampPalette(brewer.pal(8, "Reds"))(nb.cols)

butterfish<-subset(fishsub, Species=="Butterfish")

#nult panes
ggplot(butterfish)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.7)+
  facet_wrap(~Year)+scale_fill_manual(values=mycolors,name="year")+scale_color_manual(values=mycolors,name="year")+
  xlab("Hake Length (cm)")+guides(color=guide_legend("year"))
#single paned plot
ggplot(butterfish)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.6)+
  scale_fill_manual(values=mycolors, name="year")+scale_color_manual(values=mycolors, name="year")+
  xlab("Butterfish Length (cm)")+guides(color=guide_legend("year"))

----------------------------
### SANDLANCE length by year ###

nb.cols<-19

mycolors <- colorRampPalette(brewer.pal(8, "Blues"))(nb.cols)

sandlance<-subset(fishsub, Species=="Sandlance")

#w facet
ggplot(sandlance)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.7)+
  facet_wrap(~Year)+scale_fill_manual(values=mycolors,name="year")+scale_color_manual(values=mycolors,name="year")+
  xlab("/Sandlance Length (cm)")+guides(color=guide_legend("year"))
#single plot
ggplot(sandlance)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.6)+
  scale_fill_manual(values=mycolors, name="year")+scale_color_manual(values=mycolors, name="year")+
  xlab("Sandlance Length (cm)")+guides(color=guide_legend("year"))

----------------------------
### MACKEREL length by year ###
nb.cols<-19
mycolors <- colorRampPalette(brewer.pal(8, "Oranges"))(nb.cols)
mackerel<-subset(fishsub, Species=="Mackerel")
#w facet
ggplot(mackerel)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.7)+
  facet_wrap(~Year)+scale_fill_manual(values=mycolors,name="year")+scale_color_manual(values=mycolors,name="year")+
  xlab("Mackerel Length (cm)")+guides(color=guide_legend("year"))
#single plot
ggplot(mackerel)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.6)+
  scale_fill_manual(values=mycolors, name="year")+scale_color_manual(values=mycolors, name="year")+
  xlab("Mackerel Length (cm)")+guides(color=guide_legend("year"))

----------------------------
### POLLOCK length by year ###
nb.cols<-19
mycolors <- colorRampPalette(brewer.pal(8, "YlOrBr"))(nb.cols)
pollock<-subset(fishsub, Species=="Pollock")
#w facet
ggplot(pollock)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.7)+
  facet_wrap(~Year)+scale_fill_manual(values=mycolors,name="year")+scale_color_manual(values=mycolors,name="year")+
  xlab("Pollock Length (cm)")+guides(color=guide_legend("year"))
#single plot
ggplot(pollock)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.6)+
  scale_fill_manual(values=mycolors, name="year")+scale_color_manual(values=mycolors, name="year")+
  xlab("Pollock Length (cm)")+guides(color=guide_legend("year"))

----------------------------
###  fish length by year a;; species ###
nb.cols<-20
mycolors <- colorRampPalette(brewer.pal(8, "BuPu"))(nb.cols)
#w facet
ggplot(fishsub)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.7)+
  facet_wrap(~Year)+scale_fill_manual(values=mycolors,name="year")+scale_color_manual(values=mycolors,name="year")+
  xlab("Fish Length (cm)")+guides(color=guide_legend("year"))
#single plot
ggplot(fishsub)+theme_bw()+
  geom_density(aes(x=as.numeric(Length..cm.), color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.7)+
  scale_fill_manual(values=mycolors,name="year")+scale_color_manual(values=mycolors,name="year")+
  xlab("Fish Length (cm)")+guides(color=guide_legend("year"))

#------------------------------------------------------------------------------------------------------------
########### SAMPLING TIME DISTRIBUTION ############
#------------------------------------------------------------------------------------------------------------
rm(list=ls())
getwd()
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
library(ggplot2)
library(plyr)
library(dplyr)
library(pals)

tprov<-read.csv("cleaned tern provisioning.csv")
#remove small sample yrs
tprovfilt<-filter(tprov, !Year %in% c(2003, 2004, 2010, 2012))

#plot sampling time distribution by year
ggplot(tprovfilt)+theme_bw()+
  geom_density(aes(x=day.of.year, color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.6)+xlab("day of year")+
  facet_wrap(~Year)

#butterfish observed over time for Olivia
#sorry this is the ugliest thing ever but it was the best way for me to visualize this quickly
#the colored lines correspond to the top species observed that year (i.e. how many were observed at each day of the yr) 
#the dotted black line corresponds to the distribution of the total observations
#...so you can see basically that most of the peaks in species distribution correspond pretty well with when
#we were sampling most intensively with sandlance being the outlier
#TO DO: add sampling effort (i.e. nest hrs or blind time type of thing)??
tprovfilt<-filter(tprov, !Year %in% c(2003, 2004, 2010, 2012))
tprovfish<-subset(tprovfilt,Species=="Herring"|Species=="Hake"|Species=="Butterfish"|
                        Species=="Pollock"|Species=="Mackerel"|Species=="Sandlance")
fish2019<-subset(tprovfish, Year=="2015")
ggplot(fish2019)+theme_bw()+
  geom_density(aes(x=day.of.year, color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.0,color="black", size=1,linetype="dashed")+xlab("day of year")+
  geom_density(aes(x=day.of.year, color=Species.2, fill=Species.2),
               adjust=3,alpha=0.2,size=1)+xlab("day of year")
#OLIVIA THIS IS FOR YOU
ggplot()+theme_bw()+
  geom_density(data=tprovfish,aes(x=day.of.year, color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.4,color="cadetblue4", fill="cadetblue4", size=1)+xlab("day of year")+
  facet_wrap(~Year)+
  geom_density(data=subset(tprovfish, Species=="Butterfish"),aes(x=day.of.year, color=as.character(Year), fill=as.character(Year)),
               adjust=3,alpha=0.0,color="black", size=.5,linetype="dotted")+xlab("day of year")
               
    
#------------------------------------------------------------------------------------------------------------
########### RANDOM SUMMARY STUFF ############
#------------------------------------------------------------------------------------------------------------
rm(list=ls())
getwd()
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
library(ggplot2)
library(plyr)
library(dplyr)

tprov<-read.csv("cleaned tern provisioning.csv")

### observations/year of total fish ###
#--> subset out fish and create table
fish<-subset(tprov, Species.lumped=="fish")
nfish<-fish %>% 
  group_by(fish$Year)%>%
  dplyr::summarise(n=dplyr::n())

#-->plot yearly fish sample size 
ggplot(nfish)+
  geom_point(aes(x=nfish$`fish$Year`, y=nfish$n))+
  geom_line(aes(x=nfish$`fish$Year`, y=nfish$n))

### observations/year of fish species ###
#--> subset out fish and create table
fish<-subset(tprov, Species.lumped=="fish")
nfish<-fish %>% 
  dplyr::group_by(Year,Species.2)%>%
  dplyr::summarise(n=dplyr::n())

### observations/year of herring and hake ###
#--> subset out fish and create table
herringhake<-subset(tprov, Species=="Herring"|Species=="Hake")
nherringhake<-herringhake %>% 
  dplyr::group_by(Year,Species)%>%
  dplyr::summarise(n=dplyr::n())
