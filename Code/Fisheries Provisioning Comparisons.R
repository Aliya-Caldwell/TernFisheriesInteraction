#################################################################################
##########_____________ Fisheries Provisioning Comparison _____________##########
#################################################################################


##########_____________ SCATTERPLOTS OF ABUNDANCES with CORRELATION TESTS _____________##########
      ################_____ DATA NOT DNA WEIGHTED or 100km BUFFERED _________########
         ################_____ NO LAGS, ADJUSTED and NON-ADJUSTED________########
           ##################_____ df: "Tern_fisheries_data_all"________########

rm(list=ls())
getwd()
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
library(ggplot2)
library(plyr)
library(dplyr)

#read in data
dat<-read.csv("Tern_fisheries_data_all.csv")

#remove low tern sample size years 
data<-filter(dat, !Year %in% c(2003, 2004, 2010, 2012, 2016))

#subset families
hakedata<-subset(data, Family=="HAKE")
herrdata<-subset(data, Family=="HERRING")


#################### ADJUSTED FALL NO LAG ####################
#__________________________________________________________________

#HAKE
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

#HERRING
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjfall,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjfall,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjfall))+
  #geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjfall), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Herring", aes(x=0.15,y=100), size=5)

############### ADJUSTED SPRING NO LAG ############
#__________________________________________________________________

#HAKE
#Correlation Coefficient
kendall<-cor.test(hakedata$t_abundance,herrdata$f_abundance_adjspring,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,herrdata$f_abundance_adjspring,method="spearman")
head(spearman)
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjspring))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjspring), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Hake", aes(x=0.1,y=300), size=5)

#HERRING
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjspring,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjspring,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjspring))+
  #geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjspring), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Herring", aes(x=0.15,y=100), size=5)+ylim(0,100)


################### NON-ADJUSTED SPRING NO LAG #########
#__________________________________________________________________
#didnt do correlation coeffficients on these yet cuz i dont think they necessarily make sense to do when theres no lag
#because the majority of the fish in these trawls are likey adult size, so we need to backcast

#HAKE
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring))+
  geom_text(aes(label=Year, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=0, size=3)+
  geom_text(aes(label=f_N_spring, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=25, size=3)+
  geom_text(aes(label=t_N, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=-25, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+
  geom_text(label="Hake\n pts labeled fisheries N, year, tern N", aes(x=0.15,y=1280), size=5)

#HERRING
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring))+
  geom_text(aes(label=Year, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=0, size=3)+
  geom_text(aes(label=f_N_spring, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=10, size=3)+
  geom_text(aes(label=t_N, x=t_abundance, y=f_abundance_spring), nudge_x=0.02, nudge_y=-10, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+
  geom_text(label="Herring\n pts labeled fisheries N, year, tern N", aes(x=0.15,y=480), size=5)

################### NON- ADJUSTED FALL NO LAG #####################
#__________________________________________________________________

#HAKE
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall))+
  geom_text(aes(label=Year, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=0, size=3)+
  geom_text(aes(label=f_N_fall, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=20, size=3)+
  geom_text(aes(label=t_N, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=-20, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1500)+
  geom_text(label="Hake\n pts labeled fisheries N, year, tern N", aes(x=0.15,y=1500), size=5)

#HERRING
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall))+
  geom_text(aes(label=Year, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=0, size=3)+
  geom_text(aes(label=f_N_fall, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=20, size=3)+
  geom_text(aes(label=t_N, x=t_abundance, y=f_abundance_fall), nudge_x=0.02, nudge_y=-20, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Herring\n pts labeled fisheries N, year, tern N", aes(x=0.15,y=1000), size=5)


##########_____________ SCATTERPLOTS OF ABUNDANCES with CORRELATION TESTS _____________##########
      ################_____ DATA NOT DNA WEIGHTED or 100km BUFFERED _________########
            ################_____ WITH LAGS on NON-ADJUSTED DATA________########
            #############_____ df: "Tern_fisheries_data_all_lagged"________########

rm(list=ls())
getwd()
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
library(ggplot2)
library(plyr)
library(dplyr)

#read in data
dat<-read.csv("Tern_fisheries_data_all_lagged.csv")

#subset out years
#remove low tern sample size years 
data<-filter(dat, !Year.Tern %in% c(2003, 2004, 2010, 2012, 2016))

#subset families
herrdata<-subset(data, Family=="HERRING")
hakedata<-subset(data, Family=="HAKE")

################### FALL LAGGED DATA ####################
#__________________________________________________________________

#HERRING 1YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag1,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag1,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Herring", aes(x=0.15,y=1000), size=5)

#HERRING 2YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag2,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag2,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag2))+
  #geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("NMFS data (at provisioning sample yr + 2yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Fall Herring Abundance", aes(x=0.06, y=990), size=5)+
  geom_text(label="kendalls: tau=0.31, p=0.08; spearmans: rho=0.45, p=0.06", aes(x=0.13,y=960), size=4)

#HERRING 3YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag2.1,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_lag2.1,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag2.1))+
  ylab("NMFS data (at provisioning sample yr + 3yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Fall Herring Abundance", aes(x=0.06, y=990), size=5)+
  geom_text(label="kendalls: tau=0.30, p=0.08; spearmans: rho=0.41, p=0.09", aes(x=0.13,y=960), size=4)


#HAKE 1YEAR LAG
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#HAKE 2YEAR LAG
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag2))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#HAKE 3YEAR LAG
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_lag2.1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_lag2.1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)


################### SPRING LAGGED DATA ####################
#__________________________________________________________________

#HERRING 1YEAR LAG
  #Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag1,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag1,method="spearman")
head(spearman)
  #plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Herring", aes(x=0.15,y=1000), size=5)

#HERRING 2YEAR LAG
  #Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag2,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag2,method="spearman")
head(spearman)
  #plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_lag2))+
  ylab("NMFS data (at provisioning sample yr + 2yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Spring Herring Abundance", aes(x=0.06, y=990), size=5)+
  geom_text(label="kendalls: tau=0.37, p=0.03*; spearmans: rho=0.51, p=0.03*", aes(x=0.126,y=960), size=4)

#HERRING 3YEAR LAG
  #Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag3,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_lag3,method="spearman")
head(spearman)
  #plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_lag3))+
  ylab("NMFS data (at provisioning sample yr + 3yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Spring Herring Abundance", aes(x=0.06, y=990), size=5)+
  geom_text(label="kendalls: tau=0.26, p=0.13; spearmans: rho=0.37, p=0.13", aes(x=0.122,y=960), size=4)

#HAKE 1YEAR LAG
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#HAKE 2YEAR LAG
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_lag2))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#HAKE 3YEAR LAG
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_lag2.1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_lag2.1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

##########_____________ SCATTERPLOTS OF ABUNDANCES with CORRELATION TESTS _____________##########
      ################_____ DATA DNA WEIGHTED and with a 100km BUFFER _________########
         ################_____ NOT LAGGED, SIZE ADJUSTED ________########
              ################_____ df same as above ________########


rm(list=ls())
getwd()
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
library(ggplot2)
library(plyr)
library(dplyr)

#read in data
dat<-read.csv("Tern_fisheries_data_all_lagged.csv")

#subset out years
#remove low tern sample size years 
data<-filter(dat, !Year.Tern %in% c(2003, 2004, 2010, 2012, 2016))

#subset families
herrdata<-subset(data, Family=="HERRING")
hakedata<-subset(data, Family=="HAKE")


#################### ADJUSTED FALL NO LAG ####################
#___DNA,100km_______________________________________________________________

#HAKE
#Correlation coefficient 
kendall<-cor.test(hakedata$t_abundance,hakedata$f_abundance_adjfall_100km_DNAweight,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,hakedata$f_abundance_adjfall_100km_DNAweigh,method="spearman")
head(spearman)
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=hakedata$t_abundance,y=hakedata$f_abundance_adjfall_100km_DNAweigh))+
  geom_text(aes(label=hakedata$Year.Tern, x=t_abundance, y=hakedata$f_abundance_adjfall_100km_DNAweigh), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Hake", aes(x=0.05,y=470), size=5, face="plain")#+
  #geom_text(label="spearmans: rho= - 0.46, p= 0.05", aes(x=0.2, y=470), face="plain")

#HERRING
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjfall_100km_DNAweight,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjfall_100km_DNAweight,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjfall_100km_DNAweight))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjfall_100km_DNAweight), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Herring", aes(x=0.15,y=100), size=5)

############### ADJUSTED SPRING NO LAG ############
#___DNA, 100km_______________________________________________________________

#HAKE
#Correlation Coefficient
kendall<-cor.test(hakedata$t_abundance,herrdata$f_abundance_adjspring_100km_DNAweight,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,herrdata$f_abundance_adjspring_100km_DNAweight,method="spearman")
head(spearman)
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjspring_100km_DNAweight))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjspring_100km_DNAweight), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Hake", aes(x=0.1,y=300), size=5)

#HERRING
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjspring_100km_DNAweight,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjspring_100km_DNAweight,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjspring_100km_DNAweight))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjspring_100km_DNAweight), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Herring", aes(x=0.15,y=100), size=5)+ylim(0,100)


##########_____________ SCATTERPLOTS OF ABUNDANCES with CORRELATION TESTS _____________##########
      ################_____ DATA DNA WEIGHTED for GOM (no 100km buffer) _________########
            ################_____ NOT LAGGED, SIZE ADJUSTED ________########
                ################_____ df same as above ________########

rm(list=ls())
getwd()
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
library(ggplot2)
library(plyr)
library(dplyr)

#read in data
dat<-read.csv("Tern_fisheries_data_all_lagged.csv")

#subset out years
#remove low tern sample size years 
data<-filter(dat, !Year.Tern %in% c(2003, 2004, 2010, 2012, 2016))

#subset families
herrdata<-subset(data, Family=="HERRING")
hakedata<-subset(data, Family=="HAKE")


#################### ADJUSTED FALL NO LAG ####################
#___DNA,GOM_______________________________________________________________

#HAKE
#Correlation coefficient 
kendall<-cor.test(hakedata$t_abundance,hakedata$f_abundance_adjfall_GOM_DNAweight,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,hakedata$f_abundance_adjfall_GOM_DNAweight,method="spearman")
head(spearman)
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjfall_GOM_DNAweight))+
  #geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjfall_GOM_DNAweigh), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("NMFS data (in GOM at provisioning sample year)")+xlab("provisioning data")+
  geom_text(label="Fall Hake Abundance", aes(x=0.1,y=470),size=5)+xlim(0,1)+
  geom_text(label="spearman's correlation coefficient rho= - 0.51, p= 0.08", aes(x=0.21, y=455), face="plain")

#HERRING
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjfall_GOM_DNAweight,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjfall_GOM_DNAweight,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjfall_GOM_DNAweight))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjfall_GOM_DNAweight), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Herring", aes(x=0.15,y=100), size=5)

############### ADJUSTED SPRING NO LAG ############
#___DNA, GOM_______________________________________________________________

#HAKE
#Correlation Coefficient
kendall<-cor.test(hakedata$t_abundance,hakedata$f_abundance_adjspring_GOM_DNAweight,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,hakedata$f_abundance_adjspring_GOM_DNAweight,method="spearman")
head(spearman)
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjspring_GOM_DNAweight))+
  #geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjspring_GOM_DNAweight), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("NMFS data (in GOM at provisioning sample year)")+xlab("provisioning data")+
  geom_text(label="Spring Hake Abundance", aes(x=0.1,y=300), size=5)+xlim(0,1)+
  geom_text(label="spearman's correlation coefficient rho= - 0.58, p=0.04", aes(x=0.19, y=285))

#HERRING
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjspring_GOM_DNAweight,method="kendall")
head(kendall) #confused by the results of this 
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_adjspring_GOM_DNAweight,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_adjspring_GOM_DNAweight))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_adjspring_GOM_DNAweight), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring, adjusted)")+xlab("provisioning data abundance")+
  geom_text(label="Herring", aes(x=0.15,y=100), size=5)+ylim(0,100)


##########_____________ SCATTERPLOTS OF ABUNDANCES with CORRELATION TESTS _____________##########
        ################_____  DNA WEIGHTED and 100km BUFFERED _________########
          ################_____ WITH LAGS on NON-ADJUSTED DATA________########
          #############_____ df: "Tern_fisheries_data_all_lagged"________###

rm(list=ls())
getwd()
#for windows
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
#for mac
getwd()
setwd("/Users/aliyacaldwell/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
library(ggplot2)
library(plyr)
library(dplyr)

#read in data
dat<-read.csv("Tern_fisheries_data_all_lagged.csv")

#subset out years
#remove low tern sample size years 
data<-filter(dat, !Year.Tern %in% c(2003, 2004, 2010, 2012, 2016))

#subset families
herrdata<-subset(data, Family=="HERRING")
hakedata<-subset(data, Family=="HAKE")

################### FALL LAGGED DATA ####################
#__DNA, 100km________________________________________________________________

#HERRING 1YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag1,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag1,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_100km_DNAweight_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_100km_DNAweight_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Herring", aes(x=0.15,y=1000), size=5)

#HERRING 2YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag2,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag2,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_100km_DNAweight_lag2))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_100km_DNAweight_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("NMFS data (at provisioning sample yr + 2yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Fall Herring Abundance", aes(x=0.06, y=990), size=5)#+
  #geom_text(label="kendalls: tau=0.31, p=0.08; spearmans: rho=0.45, p=0.06", aes(x=0.13,y=960), size=4)

#HERRING 3YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag3,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag3,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_100km_DNAweight_lag3))+
  ylab("NMFS data (at provisioning sample yr + 3yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Fall Herring Abundance", aes(x=0.06, y=990), size=5)#+
  #geom_text(label="kendalls: tau=0.30, p=0.08; spearmans: rho=0.41, p=0.09", aes(x=0.13,y=960), size=4)


#HAKE 1YEAR LAG
#Correlation coefficient
kendall<-cor.test(hakedata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag1,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag1,method="spearman")
head(spearman)
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_100km_DNAweight_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_100km_DNAweight_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#HAKE 2YEAR LAG
#Correlation coefficient
kendall<-cor.test(hakedata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag2,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag2,method="spearman")
head(spearman)
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_100km_DNAweight_lag2))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_100km_DNAweight_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#HAKE 3YEAR LAG
#Correlation coefficient
kendall<-cor.test(hakedata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag3,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,herrdata$f_abundance_fall_100km_DNAweight_lag3,method="spearman")
head(spearman)
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_100km_DNAweight_lag3))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_100km_DNAweight_lag3), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)


################### SPRING LAGGED DATA ####################
#__DNA, 100km________________________________________________________________

#HERRING 1YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_100km_DNAweight_lag1,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_100km_DNAweight_lag1,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_100km_DNAweight_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_100km_DNAweight_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Herring", aes(x=0.15,y=1000), size=5)

#HERRING 2YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_100km_DNAweight_lag2,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_100km_DNAweight_lag2,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_100km_DNAweight_lag2))+
  ylab("NMFS data (at provisioning sample yr + 2yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Spring Herring Abundance", aes(x=0.06, y=990), size=5)+
  geom_text(label="kendalls: tau=0.37, p=0.03*; spearmans: rho=0.51, p=0.03*", aes(x=0.126,y=960), size=4)

#HERRING 3YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_100km_DNAweight_lag3,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_100km_DNAweight_lag3,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_100km_DNAweight_lag3))+
  ylab("NMFS data (at provisioning sample yr + 3yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Spring Herring Abundance", aes(x=0.06, y=990), size=5)+
  geom_text(label="kendalls: tau=0.26, p=0.13; spearmans: rho=0.37, p=0.13", aes(x=0.122,y=960), size=4)

#HAKE 1YEAR LAG
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_100km_DNAweight_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_100km_DNAweight_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#HAKE 2YEAR LAG
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_100km_DNAweight_lag2))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_100km_DNAweight_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#HAKE 3YEAR LAG 
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_100km_DNAweight_lag3))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_100km_DNAweight_lag3), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

##########_____________ SCATTERPLOTS OF ABUNDANCES with CORRELATION TESTS _____________##########
          ################_____  DNA WEIGHTED in GOM _________########
      ################_____ WITH LAGS on NON-ADJUSTED DATA________########
      #############_____ df: "Tern_fisheries_data_all_lagged"________###

rm(list=ls())
getwd()
#for windows
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
#for mac
getwd()
setwd("/Users/aliyacaldwell/Box/Furey Lab Shared Materials/TERNS/Diet--Fisheries/Data/Tern provisioning")
library(ggplot2)
library(plyr)
library(dplyr)

#read in data
dat<-read.csv("Tern_fisheries_data_all_lagged.csv")

#subset out years
#remove low tern sample size years 
data<-filter(dat, !Year.Tern %in% c(2003, 2004, 2010, 2012, 2016))

#subset families
herrdata<-subset(data, Family=="HERRING")
hakedata<-subset(data, Family=="HAKE")

################### FALL LAGGED DATA ####################
#__DNA, GOM________________________________________________________________

#HERRING 1YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_GOM_DNAweight_lag1,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_GOM_DNAweight_lag1,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_GOM_DNAweight_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_GOM_DNAweight_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Herring", aes(x=0.15,y=1000), size=5)

#HERRING 2YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_GOM_DNAweight_lag2,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_GOM_DNAweight_lag2,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_GOM_DNAweight_lag2))+
  #geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_GOM_DNAweight_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("NMFS data (in GOM at provisioning sample yr + 2yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Fall Herring Abundance", aes(x=0.1, y=990), size=5)+xlim(0,1)+
  geom_text(label="spearman's correlation coefficient rho=0.52, p=0.07", aes(x=0.18,y=960), size=4)

#HERRING 3YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_GOM_DNAweight_lag3,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_GOM_DNAweight_lag3,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_GOM_DNAweight_lag3))+
  ylab("NMFS data (at provisioning sample yr + 3yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Fall Herring Abundance", aes(x=0.06, y=990), size=5)#+
#geom_text(label="kendalls: tau=0.30, p=0.08; spearmans: rho=0.41, p=0.09", aes(x=0.13,y=960), size=4)


#HAKE 1YEAR LAG
#Correlation coefficient
kendall<-cor.test(hakedata$t_abundance,hakedata$f_abundance_fall_GOM_DNAweight_lag1,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,hakedata$f_abundance_fall_GOM_DNAweight_lag1,method="spearman")
head(spearman)
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_GOM_DNAweight_lag1))+
  #geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_GOM_DNAweight_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("NMFS data (in GOM at provisioning sample yr + 1yr")+xlab("provisioning data (at provisioning sample yr)")+ylim(0,1000)+xlim(0,1)+
  geom_text(label="Fall Hake abundance", aes(x=0.1,y=1000), size=5)


#HAKE 2YEAR LAG
#Correlation coefficient
kendall<-cor.test(hakedata$t_abundance,hakedata$f_abundance_fall_GOM_DNAweight_lag2,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,hakedata$f_abundance_fall_GOM_DNAweight_lag2,method="spearman")
head(spearman)
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_GOM_DNAweight_lag2))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_GOM_DNAweight_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#HAKE 3YEAR LAG
#Correlation coefficient
kendall<-cor.test(hakedata$t_abundance,hakedata$f_abundance_fall_GOM_DNAweight_lag3,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,hakedata$f_abundance_fall_GOM_DNAweight_lag3,method="spearman")
head(spearman)
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_GOM_DNAweight_lag3))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_fall_GOM_DNAweight_lag3), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (fall)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)


################### SPRING LAGGED DATA ####################
#__DNA, GOM________________________________________________________________

#HERRING 1YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_GOM_DNAweight_lag1,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_GOM_DNAweight_lag1,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_GOM_DNAweight_lag1))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_GOM_DNAweight_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Herring", aes(x=0.15,y=1000), size=5)

#HERRING 2YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_GOM_DNAweight_lag2,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_GOM_DNAweight_lag2,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_GOM_DNAweight_lag2))+
  ylab("NMFS data (at provisioning sample yr + 2yrs)")+xlab("Provisioning data (at provisioning sample yr)")+ylim(0,1000)+
  geom_text(label="Spring Herring Abundance", aes(x=0.1, y=990), size=5)+xlim(0,1)+
  geom_text(label="spearman's correlation coefficient rho=0.47, p=0.11", aes(x=0.17,y=960))

#HERRING 3YEAR LAG
#Correlation coefficient
kendall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_GOM_DNAweight_lag3,method="kendall")
head(kendall)
spearman<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_GOM_DNAweight_lag3,method="spearman")
head(spearman)
#plot
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_GOM_DNAweight_lag3))+
  ylab("NMFS data (at provisioning sample yr + 3yrs)")+xlab("Provisioning data (at provisioning sample yr)")+
  geom_smooth(method="lm",aes(x=t_abundance,y=f_abundance_spring_GOM_DNAweight_lag3), alpha=0.3, linetype="dashed", color="black", size=0.5)
  
  #geom_text(label="Spring Herring Abundance", aes(x=0.06, y=990), size=5)+
  #geom_text(label="kendalls: tau=0.26, p=0.13; spearmans: rho=0.37, p=0.13", aes(x=0.122,y=960), size=4)

#HAKE 1YEAR LAG
#correlation
kendall<-cor.test(hakedata$t_abundance,hakedata$f_abundance_spring_GOM_DNAweight_lag1,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,hakedata$f_abundance_spring_GOM_DNAweight_lag1,method="spearman")
head(spearman)
#plot
#plot
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_GOM_DNAweight_lag1))+
  #geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_GOM_DNAweight_lag1), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("NMFS data (at provisioning sample yr + 1yr)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Spring Hake Abundance", aes(x=0.15,y=1000), size=5)

#HAKE 2YEAR LAG
kendall<-cor.test(hakedata$t_abundance,hakedata$f_abundance_spring_GOM_DNAweight_lag2,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,hakedata$f_abundance_spring_GOM_DNAweight_lag2,method="spearman")
head(spearman)
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_GOM_DNAweight_lag2))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_GOM_DNAweight_lag2), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

#HAKE 3YEAR LAG
kendall<-cor.test(hakedata$t_abundance,hakedata$f_abundance_spring_GOM_DNAweight_lag3,method="kendall")
head(kendall)
spearman<-cor.test(hakedata$t_abundance,hakedata$f_abundance_spring_GOM_DNAweight_lag3,method="spearman")
head(spearman)
ggplot(hakedata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_GOM_DNAweight_lag3))+
  geom_text(aes(label=Year.Tern, x=t_abundance, y=f_abundance_spring_GOM_DNAweight_lag3), nudge_x=0.02, nudge_y=0, size=3)+
  ylab("Fisheries data abundance (spring)")+xlab("provisioning data abundance")+ylim(0,1000)+
  geom_text(label="Hake", aes(x=0.15,y=1000), size=5)

################### FALL and SPRING LAGGED DATA ####################
#__DNA, GOM________________________________________________________________

#herring 2 yr lag fall and spring
ggplot(herrdata)+theme_bw()+
  geom_point(aes(x=t_abundance,y=f_abundance_fall_GOM_DNAweight_lag2, colour='fall'), size=4)+labs(colour="Season")+
  ylab("NMFS data (in GOM at provisioning sample yr + 2yrs)")+xlab("Provisioning data (at provisioning sample yr)")+
  geom_text(label="Herring Abundance", aes(x=0.1, y=800), size=5)+
  geom_point(aes(x=t_abundance,y=f_abundance_spring_GOM_DNAweight_lag2, colour='spring'),size=4)+
  ylab("NMFS data (at provisioning sample yr + 2yrs)")+xlab("Provisioning data (at provisioning sample yr)")+xlim(0,1)+ylim(0,800)+
  geom_smooth(method="lm",aes(x=t_abundance,y=f_abundance_spring_GOM_DNAweight_lag2), color="#00BFC4", fill="#00BFC4", alpha=0.3, linetype="dashed", size=0.5)+
  geom_smooth(method="lm",aes(x=t_abundance,y=f_abundance_fall_GOM_DNAweight_lag2), color="#F8766D", fill="#F8766D",alpha=0.3, linetype="dashed", size=0.5)

spearmanspring<-cor.test(herrdata$t_abundance,herrdata$f_abundance_spring_GOM_DNAweight_lag2,method="spearman")
head(spearmanspring)
spearmansfall<-cor.test(herrdata$t_abundance,herrdata$f_abundance_fall_GOM_DNAweight_lag2,method="spearman")
head(spearmansfall)

     